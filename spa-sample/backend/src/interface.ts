import z from "zod";

export type WsRequest = PushMessageRequest;
export const WsRequest: () => z.ZodType<WsRequest> = () => PushMessageRequest();

export type WsResponse = PushMessageResponse;
export const WsResponse: () => z.ZodType<WsResponse> = () =>
  PushMessageResponse();

export type WsEvent =
  | {
      event: "ConnectionError";
      error: "LoginRequired";
    }
  | {
      event: "UserMessage";
      user: ChatProfile;
      value: string;
    }
  | {
      event: "UserEntered";
      user: ChatProfile;
      "active-users": ChatProfile[];
    }
  | {
      event: "UserLeft";
      user: ChatProfile;
      "active-users": ChatProfile[];
    };

export type ChatProfile = {
  "display-name": string;
  color: string;
};
const ChatProfile: () => z.ZodType<ChatProfile> = () =>
  z.object({
    "display-name": z.string(),
    color: z.string(),
  });

/* =============
 *  PushMessage
 * ============= */
export const pushMessageKey = "PushMessage";
export type PushMessageRequest = {
  action: typeof pushMessageKey;
  message: string;
  id: string;
};
export const PushMessageRequest: () => z.ZodType<PushMessageRequest> = () =>
  z.object({
    action: z.literal(pushMessageKey),
    message: z.string(),
    id: z.string(),
  });
export type PushMessageResponse = {
  response: typeof pushMessageKey;
  status: "OK";
  id: string;
  body: {
    user: ChatProfile;
    value: string;
  };
};
export const PushMessageResponse: () => z.ZodType<PushMessageResponse> = () =>
  z.object({
    response: z.literal(pushMessageKey),
    status: z.literal("OK"),
    id: z.string(),
    body: z.object({
      user: ChatProfile(),
      value: z.string(),
    }),
  });
