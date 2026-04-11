import { createResource } from "../sqlite-bridge-runtime.mjs";

class WorkerRpcClient {
  constructor(workerUrl) {
    this.worker = new Worker(workerUrl, { type: "module" });
    this.nextRequestId = 1;
    this.pending = new Map();
    this.ready = new Promise((resolve, reject) => {
      const onMessage = (event) => {
        const msg = event.data;
        if (msg?.type === "ready") {
          this.worker.removeEventListener("message", onMessage);
          this.worker.removeEventListener("error", onError);
          resolve();
        } else if (msg?.type === "init-error") {
          this.worker.removeEventListener("message", onMessage);
          this.worker.removeEventListener("error", onError);
          reject(new Error(msg.error));
        }
      };
      const onError = (event) => {
        this.worker.removeEventListener("message", onMessage);
        this.worker.removeEventListener("error", onError);
        reject(event.error ?? new Error(event.message));
      };
      this.worker.addEventListener("message", onMessage);
      this.worker.addEventListener("error", onError);
    });

    this.worker.addEventListener("message", (event) => {
      const msg = event.data;
      if (!msg || msg.type !== "response") return;
      const entry = this.pending.get(msg.id);
      if (!entry) return;
      this.pending.delete(msg.id);
      if (msg.ok) entry.resolve(msg.value);
      else entry.reject(new Error(msg.error));
    });
  }

  async call(op, ...args) {
    await this.ready;
    return await new Promise((resolve, reject) => {
      const id = this.nextRequestId++;
      this.pending.set(id, { resolve, reject });
      this.worker.postMessage({ type: "request", id, op, args });
    });
  }

  close() {
    this.worker.terminate();
  }
}

export async function createBrowserSqliteBridgeClient(workerUrl) {
  const rpc = new WorkerRpcClient(workerUrl);
  await rpc.ready;

  return {
    exampleSqliteBridge: {
      dbDrop: async (db) => {
        await rpc.call("dbDrop", db.id);
      },
      dbErrmsg: async (db) => await rpc.call("dbErrmsg", db.id),
      dbExec: async (db, sql) => await rpc.call("dbExec", db.id, sql),
      dbPrepare: async (db, sql) => {
        const out = await rpc.call("dbPrepare", db.id, sql);
        return out.tag === "ok" ? { tag: "ok", val: createResource("stmt", "own", out.val) } : out;
      },
      open: async (path) => {
        const out = await rpc.call("open", path);
        return out.tag === "ok" ? { tag: "ok", val: createResource("db", "own", out.val) } : out;
      },
      stmtBindBlob: async (stmt, index, value) => await rpc.call("stmtBindBlob", stmt.id, index, value),
      stmtBindText: async (stmt, index, value) => await rpc.call("stmtBindText", stmt.id, index, value),
      stmtColumnBlob: async (stmt, index) => await rpc.call("stmtColumnBlob", stmt.id, index),
      stmtColumnInt64: async (stmt, index) => await rpc.call("stmtColumnInt64", stmt.id, index),
      stmtColumnText: async (stmt, index) => await rpc.call("stmtColumnText", stmt.id, index),
      stmtDrop: async (stmt) => {
        await rpc.call("stmtDrop", stmt.id);
      },
      stmtReset: async (stmt) => await rpc.call("stmtReset", stmt.id),
      stmtStep: async (stmt) => await rpc.call("stmtStep", stmt.id),
    },
    close: () => rpc.close(),
  };
}
