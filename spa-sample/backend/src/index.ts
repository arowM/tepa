import express from "express";
import WebSocket, { WebSocketServer } from "ws";
import morgan from "morgan";
import cookieParser from "cookie-parser";
import cookie from "cookie";
import z from "zod";
import { join, dirname } from "node:path";
import { fileURLToPath } from "node:url";

import { Low } from "lowdb";
import { JSONFile } from "lowdb/node";
import type * as types from "./interface.js";
import { ChatProfile, WsEvent, WsRequest } from "./interface.js";

/* =============
 * Set up DB
 * ============= */

// File path
const __dirname = dirname(fileURLToPath(import.meta.url));
const file = join(__dirname, "../db.json");

// Configure lowdb to write to JSONFile
type Data = {
  users: Record<string, User>;
};

type User = {
  profile: Profile;
  color: string;
  password: string;
};

type Profile = {
  name: string;
};

/* ===========
 *  Migration
 * =========== */

const adapter = new JSONFile<Data>(file);
console.log("Migrate DB...");
const db = new Low(adapter, {
  users: {
    guest: {
      profile: {
        name: "Guest",
      },
      // ⚠ Just for demo. DO NOT STORE PLAIN PASSWORD!
      password: "guestPass",
      color: "#ff69b4",
    },
    guest2: {
      profile: {
        name: "Guest2",
      },
      // ⚠ Just for demo. DO NOT STORE PLAIN PASSWORD!
      password: "guestPass2",
      color: "#4b0082",
    },
  },
});

// Read data from JSON file, this will set db.data content
await db.read();

/* ========
 * Constants
 * ======== */

const AUTH_TOKEN_KEY = "auth_token";

/* ========
 * App
 * ======== */

const app = express();
app.use(cookieParser());
app.use(express.json());
app.use(morgan("combined"));

type LoginResponse = {
  profile: {
    id: string;
    name: string;
  };
};

type LoginRequest = {
  id: string;
  pass: string;
};

const LoginRequest: z.ZodType<LoginRequest> = z.object({
  id: z.string(),
  pass: z.string(),
});

app.post("/api/login", (req, res) => {
  const resBody = LoginRequest.safeParse(req.body);
  if (!resBody.success) {
    res.status(400).json({ code: "InvalidRequestBody" });
    return;
  }
  const body: LoginRequest = resBody.data;
  const userId: string = body.id;
  const user: User | undefined = db.data?.users[userId];
  if (user !== void 0 && body.pass === user.password) {
    // Just for demo. DO NOT use this logic in production.
    res.cookie(AUTH_TOKEN_KEY, userId, {
      httpOnly: true,
      secure: true,
    });
    const profile: Profile = user.profile;
    res.json(
      ensureTypeOf<LoginResponse>({
        profile: {
          id: userId,
          name: profile.name,
        },
      })
    );
    return;
  } else {
    res.status(401).json({ code: "IncorrectIdOrPassword" });
    return;
  }
});

// Authorization
app.use((req, res, next) => {
  const userId = req.cookies[AUTH_TOKEN_KEY];
  // Just for demo. DO NOT use this logic in production.
  const user: User | undefined = db.data?.users[userId];
  if (user === void 0) {
    res.status(401).json({ code: "LoginRequired" });
    return;
  }
  res.locals["user"] = userId;
  next();
});

type ProfileResponse = {
  profile: {
    id: string;
    name: string;
  };
};

app.get("/api/profile", (_req, res) => {
  const resUser = z.string().safeParse(res.locals["user"]);
  if (!resUser.success) {
    res.status(401).json({ code: "LoginRequired" });
    return;
  }
  ensureType<true>(resUser.success);
  const userId: string = resUser.data;
  const profile: Profile | undefined = db.data?.users[userId]?.profile;
  if (profile === void 0) {
    delete res.locals["user"];
    res.status(401).json({ code: "LoginRequired" });
    return;
  }
  ensureType<Profile>(profile);
  res.json(
    ensureTypeOf<ProfileResponse>({
      profile: {
        id: userId,
        name: profile.name,
      },
    })
  );
  return;
});

type EditProfileNameResponse = {
  profile: {
    id: string;
    name: string;
  };
};

type EditProfileNameRequest = {
  name: string;
};

const EditProfileNameRequest: z.ZodType<EditProfileNameRequest> = z.object({
  name: z.string(),
});

app.post("/api/edit-account", (req, res) => {
  const resBody = EditProfileNameRequest.safeParse(req.body);
  if (!resBody.success) {
    res.status(400).json({ code: "InvalidRequestBody" });
    return;
  }
  const body: EditProfileNameRequest = resBody.data;

  const resUser = z.string().safeParse(res.locals["user"]);
  if (!resUser.success) {
    res.status(401).json({ code: "LoginRequired" });
    return;
  }
  ensureType<true>(resUser.success);
  const userId: string = resUser.data;
  const userData = db.data?.users[userId];
  if (userData === void 0) {
    delete res.locals["user"];
    res.status(401).json({ code: "LoginRequired" });
    return;
  }
  ensureType<User>(userData);

  userData.profile.name = body.name;
  db.write();

  res.json(
    ensureTypeOf<EditProfileNameResponse>({
      profile: {
        id: userId,
        name: body.name,
      },
    })
  );
  return;
});

console.log(`\x1b[35mBackend API server running at http://localhost:8007`);
app.listen(8007);

/* ========
 * WS App
 * ======== */

const wss = new WebSocketServer({
  port: 8008,
});
const decodeMessage: (raw: WebSocket.RawData) =>
  | {
      result: "Succeed";
      value: WsRequest;
    }
  | {
      result: "InvalidJSON";
      value: string;
    } = (raw) => {
  const rawStr = raw.toString();
  try {
    const rawJson: object = JSON.parse(rawStr);
    const resMessage = WsRequest().safeParse(rawJson);
    if (!resMessage.success) {
      return {
        result: "InvalidJSON",
        value: rawStr,
      };
    }
    return {
      result: "Succeed",
      value: resMessage.data,
    };
  } catch {
    return {
      result: "InvalidJSON",
      value: rawStr,
    };
  }
};

const wsClients: Map<WebSocket, ChatProfile> = new Map();
wss.on("connection", (ws, req) => {
  const cookies = cookie.parse(req.headers.cookie ?? "");
  const userId: string | undefined = cookies[AUTH_TOKEN_KEY];
  if (userId === void 0) {
    ws.send(
      JSON.stringify(
        ensureTypeOf<WsEvent>({
          event: "ConnectionError",
          error: "LoginRequired",
        })
      )
    );
    ws.close();
    return;
  }
  ensureType<string>(userId);
  const user: User | undefined = db.data?.users[userId];
  if (user === void 0) {
    ws.send(
      JSON.stringify(
        ensureTypeOf<WsEvent>({
          event: "ConnectionError",
          error: "LoginRequired",
        })
      )
    );
    ws.close();
    return;
  }
  ensureType<User>(user);
  const chatProfile: ChatProfile = {
    "display-name": user.profile.name,
    color: user.color,
  };

  for (const [client] of wsClients) {
    if (
      client.readyState === WebSocket.CLOSING ||
      client.readyState === WebSocket.CLOSED
    ) {
      wsClients.delete(client);
    }
  }
  wsClients.set(ws, chatProfile);

  // Handle client messages
  ws.on("message", (raw: WebSocket.RawData) => {
    const resDecode = decodeMessage(raw);
    if (resDecode.result === "InvalidJSON") {
      return;
    }
    const body: WsRequest = resDecode.value;

    if (body.action === "PushMessage") {
      for (const client of wss.clients) {
        if (client.readyState !== WebSocket.OPEN) continue;
        if (client === ws) {
          client.send(
            JSON.stringify(
              ensureTypeOf<types.PushMessageResponse>({
                response: "PushMessage",
                status: "OK",
                id: body.id,
                body: {
                  user: chatProfile,
                  value: body.message,
                },
              })
            )
          );
          continue;
        }
        client.send(
          JSON.stringify(
            ensureTypeOf<WsEvent>({
              event: "UserMessage",
              user: chatProfile,
              value: body.message,
            })
          )
        );
      }
      return;
    }

    ensureType<never>(body.action);
  });

  ws.on("close", () => {
    for (const client of wss.clients) {
      if (client === ws || client.readyState !== WebSocket.OPEN) continue;
      const profile = wsClients.get(ws);
      wsClients.delete(ws);
      if (profile !== void 0) {
        client.send(
          JSON.stringify(
            ensureTypeOf<WsEvent>({
              event: "UserLeft",
              user: profile,
              "active-users": Array.from(wsClients.values()),
            })
          )
        );
      }
    }
  });

  // On connect
  for (const client of wss.clients) {
    if (client.readyState !== WebSocket.OPEN) continue;
    client.send(
      JSON.stringify(
        ensureTypeOf<WsEvent>({
          event: "UserEntered",
          user: chatProfile,
          "active-users": Array.from(wsClients.values()),
        })
      )
    );
  }
});

console.log(`\x1b[35mBackend WebSocket server running at ws://localhost:8008`);

// eslint-disable-next-line @typescript-eslint/no-empty-function
function ensureType<T>(_: T) {}
function ensureTypeOf<T>(a: T) {
  return a;
}
