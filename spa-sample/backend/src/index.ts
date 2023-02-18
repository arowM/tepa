import express from "express";
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

console.log(`\x1b[35mBackend server running at http://localhost:8007`);
app.listen(8007);

// eslint-disable-next-line @typescript-eslint/no-empty-function
function ensureType<T>(_: T) {}
