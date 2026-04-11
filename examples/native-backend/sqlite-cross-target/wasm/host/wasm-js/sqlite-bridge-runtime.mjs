function ownResource(resource, id) {
  return { __resource: resource, __ownership: "own", id };
}

function notFound(kind, id) {
  throw new Error(`missing sqlite ${kind} handle: ${id.toString()}`);
}

export class SqliteBridgeState {
  constructor(sqlite3) {
    this.sqlite3 = sqlite3;
    this.nextId = 1n;
    this.dbs = new Map();
    this.stmts = new Map();
  }

  resourceCounts() {
    return { dbs: this.dbs.size, stmts: this.stmts.size };
  }

  open(path) {
    try {
      const db = this.#openDb(path);
      const id = this.#allocId();
      this.dbs.set(id, { db, lastError: "" });
      return { tag: "ok", val: id };
    } catch (err) {
      return { tag: "err", val: this.#statusFromError(err) };
    }
  }

  dbDrop(id) {
    const doomed = [];
    for (const [stmtId, record] of this.stmts.entries()) {
      if (record.dbId === id) doomed.push(stmtId);
    }
    for (const stmtId of doomed) {
      this.stmtDrop(stmtId);
    }
    const record = this.dbs.get(id);
    if (!record) return;
    try {
      record.db.close();
    } finally {
      this.dbs.delete(id);
    }
  }

  dbErrmsg(id) {
    return this.#dbRecord(id).lastError;
  }

  dbExec(id, sql) {
    const record = this.#dbRecord(id);
    try {
      record.db.exec(sql);
      return this.sqlite3.capi.SQLITE_OK;
    } catch (err) {
      return this.#captureDbError(id, err);
    }
  }

  dbPrepare(id, sql) {
    const record = this.#dbRecord(id);
    try {
      const stmt = record.db.prepare(sql);
      const stmtId = this.#allocId();
      this.stmts.set(stmtId, { stmt, dbId: id });
      return { tag: "ok", val: stmtId };
    } catch (err) {
      return { tag: "err", val: this.#captureDbError(id, err) };
    }
  }

  stmtBindText(id, index, value) {
    const record = this.#stmtRecord(id);
    try {
      record.stmt.bind(index, value);
      return this.sqlite3.capi.SQLITE_OK;
    } catch (err) {
      return this.#captureDbError(record.dbId, err);
    }
  }

  stmtBindBlob(id, index, value) {
    const record = this.#stmtRecord(id);
    const bytes = value instanceof Uint8Array ? value : Uint8Array.from(value);
    try {
      record.stmt.bindAsBlob(index, bytes);
      return this.sqlite3.capi.SQLITE_OK;
    } catch (err) {
      return this.#captureDbError(record.dbId, err);
    }
  }

  stmtStep(id) {
    const record = this.#stmtRecord(id);
    try {
      return record.stmt.step() ? this.sqlite3.capi.SQLITE_ROW : this.sqlite3.capi.SQLITE_DONE;
    } catch (err) {
      return this.#captureDbError(record.dbId, err);
    }
  }

  stmtReset(id) {
    const record = this.#stmtRecord(id);
    try {
      record.stmt.reset();
      return this.sqlite3.capi.SQLITE_OK;
    } catch (err) {
      return this.#captureDbError(record.dbId, err);
    }
  }

  stmtColumnInt64(id, index) {
    const value = this.#stmtRecord(id).stmt.get(index);
    if (typeof value === "bigint") return value;
    if (typeof value === "number") return BigInt(value);
    if (value == null) return 0n;
    throw new Error(`expected integer column, got ${typeof value}`);
  }

  stmtColumnText(id, index) {
    return this.#stmtRecord(id).stmt.getString(index) ?? "";
  }

  stmtColumnBlob(id, index) {
    return this.#stmtRecord(id).stmt.getBlob(index) ?? new Uint8Array();
  }

  stmtDrop(id) {
    const record = this.stmts.get(id);
    if (!record) return;
    try {
      record.stmt.finalize();
    } finally {
      this.stmts.delete(id);
    }
  }

  makeImplementations() {
    return {
      exampleSqliteBridge: {
        dbDrop: async (db) => this.dbDrop(db.id),
        dbErrmsg: async (db) => this.dbErrmsg(db.id),
        dbExec: async (db, sql) => this.dbExec(db.id, sql),
        dbPrepare: async (db, sql) => {
          const out = this.dbPrepare(db.id, sql);
          return out.tag === "ok" ? { tag: "ok", val: ownResource("stmt", out.val) } : out;
        },
        open: async (path) => {
          const out = this.open(path);
          return out.tag === "ok" ? { tag: "ok", val: ownResource("db", out.val) } : out;
        },
        stmtBindBlob: async (stmt, index, value) => this.stmtBindBlob(stmt.id, index, value),
        stmtBindText: async (stmt, index, value) => this.stmtBindText(stmt.id, index, value),
        stmtColumnBlob: async (stmt, index) => this.stmtColumnBlob(stmt.id, index),
        stmtColumnInt64: async (stmt, index) => this.stmtColumnInt64(stmt.id, index),
        stmtColumnText: async (stmt, index) => this.stmtColumnText(stmt.id, index),
        stmtDrop: async (stmt) => this.stmtDrop(stmt.id),
        stmtReset: async (stmt) => this.stmtReset(stmt.id),
        stmtStep: async (stmt) => this.stmtStep(stmt.id),
      },
    };
  }

  #allocId() {
    const id = this.nextId;
    this.nextId += 1n;
    return id;
  }

  #dbRecord(id) {
    return this.dbs.get(id) ?? notFound("db", id);
  }

  #stmtRecord(id) {
    return this.stmts.get(id) ?? notFound("stmt", id);
  }

  #captureDbError(dbId, err) {
    const code = this.#statusFromError(err);
    const record = this.#dbRecord(dbId);
    record.lastError = err instanceof Error ? err.message : String(err);
    return code;
  }

  #statusFromError(err) {
    return typeof err?.resultCode === "number" ? err.resultCode : this.sqlite3.capi.SQLITE_ERROR;
  }

  #openDb(path) {
    if (path === ":memory:" || path === "") {
      return new this.sqlite3.oo1.DB(path || ":memory:", "ct");
    }
    if ("opfs" in this.sqlite3 && typeof this.sqlite3.oo1.OpfsDb === "function") {
      const filename = path.startsWith("/") || path.startsWith("file:") ? path : `/${path}`;
      return new this.sqlite3.oo1.OpfsDb(filename);
    }
    return new this.sqlite3.oo1.DB(path, "ct");
  }
}

export function createResource(resource, ownership, id) {
  return { __resource: resource, __ownership: ownership, id };
}
