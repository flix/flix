use anyhow::{Context, Result};
use flix_wit_effect_bindings::{
    bindings,
    effects::{AsyncResult, EffectManifest},
    runner::FlixRunner,
    ExampleSqliteBridge,
    ExampleSqliteBridgeDb,
    ExampleSqliteBridgeStmt,
    WitEffectHandler,
};
use libsqlite3_sys as ffi;
use serde::Deserialize;
use std::{
    collections::HashMap,
    ffi::{CStr, CString, c_char},
    fs,
    path::PathBuf,
    ptr,
};
use wasmtime::{
    Config, Engine, Store,
    component::{Component, HasSelf, Linker},
};

use bindings::flix::sys::sys::{Capability, Host as SysHost, LogLevel};

#[derive(Default)]
struct State;

impl SysHost for State {
    fn log(&mut self, _level: LogLevel, _msg: String) {}

    fn time_now_ms(&mut self) -> i64 {
        0
    }

    fn random_bytes(&mut self, len: u32) -> Vec<u8> {
        (0..len).map(|i| i as u8).collect()
    }

    fn get_args(&mut self) -> Vec<String> {
        Vec::new()
    }

    fn has_capability(&mut self, _cap: Capability) -> bool {
        false
    }
}

#[derive(Deserialize)]
struct ExportsManifest {
    defs: Vec<ExportedDef>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct ExportedDef {
    def_id: u64,
    symbol: String,
}

struct DbState {
    db: *mut ffi::sqlite3,
    last_error: String,
}

struct StmtState {
    stmt: *mut ffi::sqlite3_stmt,
    db_id: i64,
}

struct SqliteHost {
    next_id: i64,
    dbs: HashMap<i64, DbState>,
    stmts: HashMap<i64, StmtState>,
}

impl SqliteHost {
    fn new() -> Self {
        Self {
            next_id: 1,
            dbs: HashMap::new(),
            stmts: HashMap::new(),
        }
    }

    fn alloc_id(&mut self) -> i64 {
        let id = self.next_id;
        self.next_id += 1;
        id
    }

    fn db_ptr(&self, id: i64) -> Result<*mut ffi::sqlite3> {
        Ok(self.dbs.get(&id).context("missing sqlite db handle")?.db)
    }

    fn stmt_ptr(&self, id: i64) -> Result<*mut ffi::sqlite3_stmt> {
        Ok(self.stmts.get(&id).context("missing sqlite stmt handle")?.stmt)
    }

    fn stmt_db_id(&self, id: i64) -> Result<i64> {
        Ok(self.stmts.get(&id).context("missing sqlite stmt handle")?.db_id)
    }

    fn set_last_error(&mut self, db_id: i64, message: String) -> i32 {
        if let Some(db) = self.dbs.get_mut(&db_id) {
            db.last_error = message;
        }
        ffi::SQLITE_ERROR
    }

    fn capture_sqlite_error(&mut self, db_id: i64, code: i32) -> i32 {
        let message = self
            .dbs
            .get(&db_id)
            .map(|db| unsafe { c_string(ffi::sqlite3_errmsg(db.db)) })
            .unwrap_or_else(|| unsafe { c_string(ffi::sqlite3_errstr(code)) });
        if let Some(db) = self.dbs.get_mut(&db_id) {
            db.last_error = message;
        }
        code
    }

    fn drop_db(&mut self, db_id: i64) {
        let doomed: Vec<i64> = self
            .stmts
            .iter()
            .filter_map(|(stmt_id, stmt)| (stmt.db_id == db_id).then_some(*stmt_id))
            .collect();
        for stmt_id in doomed {
            self.drop_stmt(stmt_id);
        }
        if let Some(db) = self.dbs.remove(&db_id) {
            unsafe {
                let _ = ffi::sqlite3_close(db.db);
            }
        }
    }

    fn drop_stmt(&mut self, stmt_id: i64) {
        if let Some(stmt) = self.stmts.remove(&stmt_id) {
            unsafe {
                let _ = ffi::sqlite3_finalize(stmt.stmt);
            }
        }
    }
}

impl Drop for SqliteHost {
    fn drop(&mut self) {
        let stmt_ids: Vec<i64> = self.stmts.keys().copied().collect();
        for stmt_id in stmt_ids {
            self.drop_stmt(stmt_id);
        }
        let db_ids: Vec<i64> = self.dbs.keys().copied().collect();
        for db_id in db_ids {
            self.drop_db(db_id);
        }
    }
}

impl ExampleSqliteBridge for SqliteHost {
    fn db_drop(&mut self, db: ExampleSqliteBridgeDb) -> Result<AsyncResult<()>> {
        self.drop_db(db.0);
        Ok(AsyncResult::Ready(()))
    }

    fn db_errmsg(&mut self, db: ExampleSqliteBridgeDb) -> Result<AsyncResult<String>> {
        let msg = self
            .dbs
            .get(&db.0)
            .context("missing sqlite db handle")?
            .last_error
            .clone();
        Ok(AsyncResult::Ready(msg))
    }

    fn db_exec(&mut self, db: ExampleSqliteBridgeDb, sql: String) -> Result<AsyncResult<i32>> {
        let c_sql = match CString::new(sql) {
            Ok(sql) => sql,
            Err(_) => return Ok(AsyncResult::Ready(self.set_last_error(db.0, "SQL string contains NUL byte".to_string()))),
        };
        let db_ptr = self.db_ptr(db.0)?;
        let rc = unsafe { ffi::sqlite3_exec(db_ptr, c_sql.as_ptr(), None, ptr::null_mut(), ptr::null_mut()) };
        let status = if rc == ffi::SQLITE_OK {
            ffi::SQLITE_OK
        } else {
            self.capture_sqlite_error(db.0, rc)
        };
        Ok(AsyncResult::Ready(status))
    }

    fn db_prepare(&mut self, db: ExampleSqliteBridgeDb, sql: String) -> Result<AsyncResult<std::result::Result<ExampleSqliteBridgeStmt, i32>>> {
        let c_sql = match CString::new(sql) {
            Ok(sql) => sql,
            Err(_) => return Ok(AsyncResult::Ready(Err(self.set_last_error(db.0, "SQL string contains NUL byte".to_string())))),
        };
        let db_ptr = self.db_ptr(db.0)?;
        let mut stmt = ptr::null_mut();
        let rc = unsafe { ffi::sqlite3_prepare_v2(db_ptr, c_sql.as_ptr(), -1, &mut stmt, ptr::null_mut()) };
        if rc == ffi::SQLITE_OK {
            let stmt_id = self.alloc_id();
            self.stmts.insert(stmt_id, StmtState { stmt, db_id: db.0 });
            Ok(AsyncResult::Ready(Ok(ExampleSqliteBridgeStmt(stmt_id))))
        } else {
            Ok(AsyncResult::Ready(Err(self.capture_sqlite_error(db.0, rc))))
        }
    }

    fn open(&mut self, path: String) -> Result<AsyncResult<std::result::Result<ExampleSqliteBridgeDb, i32>>> {
        let c_path = match CString::new(path) {
            Ok(path) => path,
            Err(_) => return Ok(AsyncResult::Ready(Err(ffi::SQLITE_MISUSE))),
        };
        let mut db = ptr::null_mut();
        let flags = ffi::SQLITE_OPEN_READWRITE | ffi::SQLITE_OPEN_CREATE | ffi::SQLITE_OPEN_URI;
        let rc = unsafe { ffi::sqlite3_open_v2(c_path.as_ptr(), &mut db, flags, ptr::null()) };
        if rc == ffi::SQLITE_OK {
            let db_id = self.alloc_id();
            self.dbs.insert(db_id, DbState { db, last_error: String::new() });
            Ok(AsyncResult::Ready(Ok(ExampleSqliteBridgeDb(db_id))))
        } else {
            if !db.is_null() {
                unsafe {
                    let _ = ffi::sqlite3_close(db);
                }
            }
            Ok(AsyncResult::Ready(Err(rc)))
        }
    }

    fn stmt_bind_blob(&mut self, stmt: ExampleSqliteBridgeStmt, index: i32, value: Vec<u8>) -> Result<AsyncResult<i32>> {
        let stmt_ptr = self.stmt_ptr(stmt.0)?;
        let db_id = self.stmt_db_id(stmt.0)?;
        let ptr = if value.is_empty() { ptr::null() } else { value.as_ptr().cast() };
        let rc = unsafe {
            ffi::sqlite3_bind_blob(stmt_ptr, index, ptr, value.len() as i32, ffi::SQLITE_TRANSIENT())
        };
        let status = if rc == ffi::SQLITE_OK {
            ffi::SQLITE_OK
        } else {
            self.capture_sqlite_error(db_id, rc)
        };
        Ok(AsyncResult::Ready(status))
    }

    fn stmt_bind_text(&mut self, stmt: ExampleSqliteBridgeStmt, index: i32, value: String) -> Result<AsyncResult<i32>> {
        let c_value = match CString::new(value) {
            Ok(value) => value,
            Err(_) => return Ok(AsyncResult::Ready(self.set_last_error(self.stmt_db_id(stmt.0)?, "bound text contains NUL byte".to_string()))),
        };
        let stmt_ptr = self.stmt_ptr(stmt.0)?;
        let db_id = self.stmt_db_id(stmt.0)?;
        let rc = unsafe { ffi::sqlite3_bind_text(stmt_ptr, index, c_value.as_ptr(), -1, ffi::SQLITE_TRANSIENT()) };
        let status = if rc == ffi::SQLITE_OK {
            ffi::SQLITE_OK
        } else {
            self.capture_sqlite_error(db_id, rc)
        };
        Ok(AsyncResult::Ready(status))
    }

    fn stmt_column_blob(&mut self, stmt: ExampleSqliteBridgeStmt, index: i32) -> Result<AsyncResult<Vec<u8>>> {
        let stmt_ptr = self.stmt_ptr(stmt.0)?;
        let len = unsafe { ffi::sqlite3_column_bytes(stmt_ptr, index) };
        let ptr = unsafe { ffi::sqlite3_column_blob(stmt_ptr, index) } as *const u8;
        let value = if ptr.is_null() || len <= 0 {
            Vec::new()
        } else {
            unsafe { std::slice::from_raw_parts(ptr, len as usize).to_vec() }
        };
        Ok(AsyncResult::Ready(value))
    }

    fn stmt_column_int64(&mut self, stmt: ExampleSqliteBridgeStmt, index: i32) -> Result<AsyncResult<i64>> {
        let value = unsafe { ffi::sqlite3_column_int64(self.stmt_ptr(stmt.0)?, index) };
        Ok(AsyncResult::Ready(value))
    }

    fn stmt_column_text(&mut self, stmt: ExampleSqliteBridgeStmt, index: i32) -> Result<AsyncResult<String>> {
        let text_ptr = unsafe { ffi::sqlite3_column_text(self.stmt_ptr(stmt.0)?, index) };
        let value = if text_ptr.is_null() {
            String::new()
        } else {
            c_string(text_ptr.cast::<c_char>())
        };
        Ok(AsyncResult::Ready(value))
    }

    fn stmt_drop(&mut self, stmt: ExampleSqliteBridgeStmt) -> Result<AsyncResult<()>> {
        self.drop_stmt(stmt.0);
        Ok(AsyncResult::Ready(()))
    }

    fn stmt_reset(&mut self, stmt: ExampleSqliteBridgeStmt) -> Result<AsyncResult<i32>> {
        let stmt_ptr = self.stmt_ptr(stmt.0)?;
        let db_id = self.stmt_db_id(stmt.0)?;
        let rc = unsafe { ffi::sqlite3_reset(stmt_ptr) };
        let status = if rc == ffi::SQLITE_OK {
            ffi::SQLITE_OK
        } else {
            self.capture_sqlite_error(db_id, rc)
        };
        Ok(AsyncResult::Ready(status))
    }

    fn stmt_step(&mut self, stmt: ExampleSqliteBridgeStmt) -> Result<AsyncResult<i32>> {
        let stmt_ptr = self.stmt_ptr(stmt.0)?;
        let db_id = self.stmt_db_id(stmt.0)?;
        let rc = unsafe { ffi::sqlite3_step(stmt_ptr) };
        let status = match rc {
            x if x == ffi::SQLITE_ROW => ffi::SQLITE_ROW,
            x if x == ffi::SQLITE_DONE => ffi::SQLITE_DONE,
            _ => self.capture_sqlite_error(db_id, rc),
        };
        Ok(AsyncResult::Ready(status))
    }
}

fn c_string(ptr: *const c_char) -> String {
    unsafe { CStr::from_ptr(ptr).to_string_lossy().into_owned() }
}

fn main() -> Result<()> {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..");
    let component_path = root.join("build/wasm/llvm/wasm/sqlite-wasm.component.wasm");
    let exports_manifest_path = root.join("build/wasm/llvm/sqlite-wasm.exports.json");
    let effects_manifest_path = root.join("build/wasm/llvm/sqlite-wasm.effects.json");

    let mut config = Config::new();
    config.wasm_component_model(true);
    let engine = Engine::new(&config)?;

    let component = Component::from_file(&engine, &component_path)
        .with_context(|| format!("failed to load component: {}", component_path.display()))?;

    let mut linker = Linker::<State>::new(&engine);
    bindings::flix::sys::sys::add_to_linker::<_, HasSelf<_>>(&mut linker, |s| s)?;

    let mut store = Store::new(&engine, State::default());
    let flix = bindings::Flix::instantiate(&mut store, &component, &linker)?;
    let rt = flix.flix_runtime_runtime();

    let exports: ExportsManifest = serde_json::from_str(&fs::read_to_string(&exports_manifest_path)?)
        .with_context(|| format!("failed to parse exports manifest: {}", exports_manifest_path.display()))?;
    let main_def = exports
        .defs
        .into_iter()
        .find(|d| d.symbol == "Api.runDemo")
        .context("missing Api.runDemo export")?;

    let manifest = EffectManifest::from_path(&effects_manifest_path)?;
    let mut handler = WitEffectHandler::new(manifest, SqliteHost::new());

    let ctx = rt.call_new_ctx(&mut store)?;
    let task_id = rt.call_start_task(&mut store, ctx, main_def.def_id, &[])?;

    let runner = FlixRunner { budget: 100 };
    let outcome = runner.run_task_to_completion(&mut store, rt, ctx, task_id, &mut handler)?;
    match outcome {
        bindings::exports::flix::runtime::runtime::TaskOutcome::Ok(v) => {
            let result = rt.call_unbox_string(&mut store, ctx, v)?;
            anyhow::ensure!(result == "1:hello:4", "bad result: {}", result);
            let _ = v.resource_drop(&mut store);
        }
        other => anyhow::bail!("unexpected task outcome: {:?}", other),
    }

    anyhow::ensure!(handler.example_sqlite_bridge.dbs.is_empty(), "db leak detected");
    anyhow::ensure!(handler.example_sqlite_bridge.stmts.is_empty(), "stmt leak detected");
    println!("OK");
    Ok(())
}
