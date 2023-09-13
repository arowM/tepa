// @ts-ignore
import { Elm } from "../App.elm";
import * as backend from "../../backend/src/interface";

const app: Elm = Elm.App.init({
  node: document.body.appendChild(document.createElement("div")),
  flags: {},
});

app.ports["app_dom_overwrite_value_request"].subscribe(
  (req: { id: string; body: { id: string; value: string } }) => {
    try {
      const target: HTMLElement | null = document.getElementById(req.body.id);
      if (target === null) {
        app.ports["app_dom_overwrite_value_response"].send({
          id: req.id,
          body: {
            result: "ElementNotFound",
          },
        });
        return;
      }
      // Just ignore for elements without "value" property.
      if ("value" in target) {
        target.value = req.body.value;
        app.ports["app_dom_overwrite_value_response"].send({
          id: req.id,
          body: {
            result: "Success",
          },
        });
        return;
      }
    } catch (e) {
      console.error(e);
    }
  },
);

let ws: WebSocket;
app.ports["page_chat_chatServer_events_request"].subscribe(
  (req: { id: string }) => {
    try {
      const protocol = window.location.protocol === "https" ? "wss" : "ws";
      ws = new WebSocket(`${protocol}://${window.location.host}/ws/`);
      ws.addEventListener("message", (event) => {
        try {
          const payload: backend.WsEvent = JSON.parse(event.data);
          if (payload.event === "ConnectionError") {
            app.ports["page_chat_chatServer_events_response"].send({
              id: req.id,
              body: {
                message: "ConnectionError",
                error: payload.error,
              },
            });
            return;
          }
          app.ports["page_chat_chatServer_events_response"].send({
            id: req.id,
            body: {
              message: "ReceiveMessage",
              payload: payload,
            },
          });
        } catch {
          console.warn(`Unknown data: ${JSON.stringify(event.data)}`);
          return;
        }
      });
      ws.addEventListener("close", () => {
        app.ports["page_chat_chatServer_events_response"].send({
          id: req.id,
          body: {
            message: "Disconnected",
          },
        });
      });
      ws.addEventListener("error", (event) => {
        console.error(event);
        if (ws.readyState === WebSocket.CONNECTING) {
          app.ports["page_chat_chatServer_events_response"].send({
            id: req.id,
            body: {
              message: "FatalError",
            },
          });
          ws.close();
        }
      });
    } catch (e) {
      console.error(e);
      app.ports["page_chat_chatServer_events_response"].send({
        id: req.id,
        body: {
          result: "FatalError",
        },
      });
    }
  },
);

app.ports["page_chat_chatServer_close_request"].subscribe(
  (req: { id: string }) => {
    try {
      ws.close();
      ws.addEventListener("close", () => {
        app.ports["page_chat_chatServer_close_response"].send({
          id: req.id,
          body: {
            result: "Success",
          },
        });
      });
    } catch (e) {
      console.error(e);
      app.ports["page_chat_chatServer_close_response"].send({
        id: req.id,
        body: {
          result: "FatalError",
        },
      });
    }
  },
);

app.ports["page_chat_newMessage_request"].subscribe(
  (req: { id: string; body: { message: string } }) => {
    try {
      ws.addEventListener("message", (event) => {
        let payload: object;
        try {
          payload = JSON.parse(event.data);
        } catch {
          console.warn(`Unknown data: ${JSON.stringify(event.data)}`);
          return;
        }
        const resMessage = backend.PushMessageResponse().safeParse(payload);
        if (!resMessage.success) {
          return;
        }
        const message: backend.PushMessageResponse = resMessage.data;
        if (message.response === "PushMessage") {
          if (message.id !== req.id) {
            return;
          }
          app.ports["page_chat_newMessage_response"].send({
            id: req.id,
            body: {
              result: "Success",
              body: message.body,
            },
          });
          return;
        }
        ensureType<never>(message.response);
      });
      ws.addEventListener("close", () => {
        app.ports["page_chat_newMessage_response"].send({
          id: req.id,
          body: {
            result: "Disconnected",
          },
        });
      });
      ws.send(
        JSON.stringify(
          ensureTypeOf<backend.PushMessageRequest>({
            action: "PushMessage",
            message: req.body.message,
            id: req.id,
          }),
        ),
      );
    } catch (e) {
      console.error(e);
      app.ports["page_chat_newMessage_response"].send({
        id: req.id,
        body: {
          result: "FatalError",
        },
      });
    }
  },
);

function ensureTypeOf<T>(a: T) {
  return a;
}
// eslint-disable-next-line @typescript-eslint/no-empty-function
function ensureType<T>(_: T) {}
