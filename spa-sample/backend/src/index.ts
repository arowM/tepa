import express from "express";
import WebSocket, { WebSocketServer } from "ws";
import morgan from "morgan";
import cookieParser from "cookie-parser";
import z from "zod";
import { join, dirname } from "node:path";
import { fileURLToPath } from "node:url";

import { Low } from "lowdb";
import { JSONFile } from "lowdb/node";

/* =============
 * Set up DB
 * ============= */

// File path
const __dirname = dirname(fileURLToPath(import.meta.url));
const file = join(__dirname, "../db.json");

// Configure lowdb to write to JSONFile
type Data = {
  users: Record<
    string,
    {
      profile: Profile;
    }
  >;
};
type Profile = {
  name?: string;
};
const adapter = new JSONFile<Data>(file);
const db = new Low(adapter);

// Read data from JSON file, this will set db.data content
await db.read();

/* ===========
 *  Migration
 * =========== */

if (db.data === null) {
  console.log("Migrate DB...");
  db.data = {
    users: {
      guest: {
        profile: {
          name: "guest",
        },
      },
    },
  };
  db.write();
}

/* ========
 * App
 * ======== */

const app = express();
app.use(cookieParser());
app.use(express.json());
app.use(morgan("combined"));

app.post("/api/login", (req, res) => {
  if (req.body.id === "guest" && req.body.pass === "guestPass") {
    const user = "guest";
    // Just for demo. DO NOT use this logic in production.
    res.cookie("auth_token", "authenticated", {
      httpOnly: true,
      secure: true,
    });
    const profile: Profile | undefined = db.data?.users[user]?.profile;
    if (profile === void 0) {
      res.status(500).json({ code: "InternalError" });
      return;
    }
    ensureType<Profile>(profile);
    res.json({
      profile: {
        id: user,
        ...profile,
      },
    });
    return;
  } else {
    res.status(401).json({ code: "IncorrectIdOrPassword" });
    return;
  }
});

app.use((req, res, next) => {
  const authToken = req.cookies["auth_token"];
  // Just for demo. DO NOT use this logic in production.
  const isAuthorized = authToken !== void 0 && authToken === "authenticated";
  if (isAuthorized) {
    res.locals["user"] = "guest";
    next();
  } else {
    res.status(401).json({ code: "LoginRequired" });
  }
});

app.get("/api/profile", (_req, res) => {
  const userRes = z.string().safeParse(res.locals["user"]);
  if (!userRes.success) {
    res.status(401).json({ code: "LoginRequired" });
    return;
  }
  ensureType<true>(userRes.success);
  const user: string = userRes.data;
  const profile: Profile | undefined = db.data?.users[user]?.profile;
  if (profile === void 0) {
    res.status(500).json({ code: "InternalError" });
    return;
  }
  ensureType<Profile>(profile);
  res.json({
    profile: {
      id: user,
      ...profile,
    },
  });
  return;
});

app.post("/api/edit-profile-name", (req, res) => {
  const RequestBody = z.object({
    name: z.string(),
  });
  type RequestBody = z.infer<typeof RequestBody>;
  const requestBodyRes = RequestBody.safeParse(req.body);
  if (!requestBodyRes.success) {
    res.status(400).json({ code: "InvalidRequestBody" });
    return;
  }
  ensureType<true>(requestBodyRes.success);
  const requestBody: RequestBody = requestBodyRes.data;
  const userRes = z.string().safeParse(res.locals["user"]);
  if (!userRes.success) {
    res.status(401).json({ code: "LoginRequired" });
    return;
  }
  ensureType<true>(userRes.success);
  const user: string = userRes.data;

  const userData = db.data?.users[user];
  if (userData === void 0) {
    res.status(500).json({ code: "InternalError" });
    return;
  }
  userData.profile.name = requestBody.name;
  db.write();

  res.json({
    profile: {
      id: user,
      ...userData.profile,
    },
  });
  return;
});

console.log(`\x1b[35mBackend API server running at http://localhost:8007`);
app.listen(8007);

/* ========
 * WS App
 * ======== */

type ChatProfile = {
  displayName: string;
};

const ChatProfile: () => z.ZodType<ChatProfile> = () =>
  z.object({
    displayName: z.string(),
  });

type WsReceive =
  | {
      action: "push-message";
      message: string;
    }
  | {
      action: "init";
      profile: ChatProfile;
    };

const WsReceive: z.ZodType<WsReceive> = z.union([
  z.object({
    action: z.literal("push-message"),
    message: z.string(),
  }),
  z.object({
    action: z.literal("init"),
    profile: ChatProfile(),
  }),
]);

type WsSend =
  | {
      message: "connected";
      users: ChatProfile[];
    }
  | {
      message: "new-message";
      user: ChatProfile;
      value: string;
    }
  | {
      message: "new-user";
      user: ChatProfile;
      users: ChatProfile[];
    }
  | {
      message: "user-left";
      user: ChatProfile | undefined;
      users: ChatProfile[];
    };

const wss = new WebSocketServer({
  port: 8008,
});
const decodeMessage: (raw: WebSocket.RawData) =>
  | {
      result: "Succeed";
      value: WsReceive;
    }
  | {
      result: "InvalidJSON";
      value: string;
    }
  | {
      result: "InvalidMessage";
      value: object;
    } = (raw) => {
  const rawStr = raw.toString();
  try {
    const rawJson = JSON.parse(rawStr);
    const resMessage = WsReceive.safeParse(rawJson);
    if (!resMessage.success) {
      return {
        result: "InvalidMessage",
        value: rawJson,
      };
    }
    return {
      result: "Succeed",
      value: resMessage.data,
    };
  } catch (e) {
    return {
      result: "InvalidJSON",
      value: rawStr,
    };
  }
};

const wsClients: Map<WebSocket, ChatProfile> = new Map();
wss.on("connection", (ws) => {
  ws.on("message", (raw: WebSocket.RawData) => {
    const resDecode = decodeMessage(raw);
    if (
      resDecode.result === "InvalidJSON" ||
      resDecode.result === "InvalidMessage"
    ) {
      return;
    }
    const message: WsReceive = resDecode.value;

    if (message.action === "init") {
      wsClients.set(ws, message.profile);
      for (const client of wss.clients) {
        const prof: ChatProfile | undefined = wsClients.get(client);
        if (prof === void 0) {
          ws.close();
          continue;
        }
        if (client === ws || client.readyState !== WebSocket.OPEN) continue;
        client.send(
          JSON.stringify(
            ensureTypeOf<WsSend>({
              message: "new-user",
              user: message.profile,
              users: Array.from(wsClients.values()),
            })
          )
        );
      }
      ws.send(
        JSON.stringify(
          ensureTypeOf<WsSend>({
            message: "connected",
            users: Array.from(wsClients.values()),
          })
        )
      );
    }
    if (message.action === "push-message") {
      const profile = wsClients.get(ws);
      if (profile === void 0) {
        ws.close();
        return;
      }
      for (const client of wss.clients) {
        if (client.readyState !== WebSocket.OPEN) continue;
        client.send(
          JSON.stringify(
            ensureTypeOf<WsSend>({
              message: "new-message",
              user: profile,
              value: message.message,
            })
          )
        );
      }
    }
  });
  ws.on("close", () => {
    wsClients.delete(ws);
    for (const client of wss.clients) {
      if (client === ws || client.readyState !== WebSocket.OPEN) continue;
      const profile = wsClients.get(ws);
      wsClients.delete(ws);
      client.send(
        JSON.stringify(
          ensureTypeOf<WsSend>({
            message: "user-left",
            user: profile,
            users: Array.from(wsClients.values()),
          })
        )
      );
    }
  });
});
console.log(
  `\x1b[35mBackend WebSocket server running at http://localhost:8008`
);

// eslint-disable-next-line @typescript-eslint/no-empty-function
function ensureType<T>(_: T) {}
function ensureTypeOf<T>(a: T) {
  return a;
}
