import sqlite3InitModule from "../node_modules/@sqlite.org/sqlite-wasm/dist/index.mjs";
import { SqliteBridgeState } from "../sqlite-bridge-runtime.mjs";

const statePromise = sqlite3InitModule().then((sqlite3) => new SqliteBridgeState(sqlite3));

statePromise.then(
  () => self.postMessage({ type: "ready" }),
  (err) => self.postMessage({ type: "init-error", error: err instanceof Error ? err.message : String(err) }),
);

self.addEventListener("message", async (event) => {
  const msg = event.data;
  if (!msg || msg.type !== "request") return;
  try {
    const state = await statePromise;
    const value = state[msg.op](...msg.args);
    self.postMessage({ type: "response", id: msg.id, ok: true, value });
  } catch (err) {
    self.postMessage({
      type: "response",
      id: msg.id,
      ok: false,
      error: err instanceof Error ? err.message : String(err),
    });
  }
});
