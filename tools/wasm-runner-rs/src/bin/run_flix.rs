use anyhow::{Context, Result};
use flix_wasm_runner::{bindings, host::StdHostHandlers, runner::FlixRunner};
use serde::Deserialize;
use std::fs;
use std::path::{Path, PathBuf};
use wasmtime::{
    Config, Engine, Store,
    component::{Component, HasSelf, Linker},
};

use bindings::flix::sys::sys::{Capability, Host as SysHost, LogLevel};
use bindings::exports::flix::runtime::runtime::Value;

#[derive(Default)]
struct State {
    argv: Vec<String>,
}

impl SysHost for State {
    fn log(&mut self, level: LogLevel, msg: String) {
        match level {
            LogLevel::Info => println!("{msg}"),
            _ => eprintln!("[flix:{level:?}] {msg}"),
        }
    }

    fn time_now_ms(&mut self) -> i64 {
        use std::time::{SystemTime, UNIX_EPOCH};
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default();
        now.as_millis().min(i64::MAX as u128) as i64
    }

    fn random_bytes(&mut self, len: u32) -> Vec<u8> {
        // Deterministic bytes (non-cryptographic) for reproducible tests.
        (0..len).map(|i| (i as u8).wrapping_mul(31)).collect()
    }

    fn get_args(&mut self) -> Vec<String> {
        self.argv.clone()
    }

    fn has_capability(&mut self, cap: Capability) -> bool {
        // Be conservative: only claim capabilities that we implement with portable semantics.
        //
        // HTTP parity across hosts is still subtle, so keep it disabled here until we intentionally
        // add a real HTTP implementation + test coverage.
        matches!(cap, Capability::Filesystem | Capability::Process | Capability::Sockets)
    }
}

#[derive(Deserialize)]
struct ExportsManifest {
    schema: String,
    defs: Vec<ExportedDef>,
}

#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct ExportedDef {
    def_id: u64,
    symbol: String,
    is_main: bool,
    arity: u64,
    params: Vec<String>,
}

fn usage() -> ! {
    eprintln!(
        "usage: run_flix <component.wasm> --exports <flix_wasm_exports.json> [--rootDir <dir>] [--budget <u32>] [--argv <arg>]... [--arg <param>]..."
    );
    std::process::exit(2);
}

fn parse_args(args: &[String]) -> (PathBuf, PathBuf, PathBuf, u32, Vec<String>, Vec<String>) {
    let mut component: Option<PathBuf> = None;
    let mut exports: Option<PathBuf> = None;
    let mut root_dir: PathBuf = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
    let mut budget: u32 = 100;
    let mut argv: Vec<String> = Vec::new();
    let mut def_args: Vec<String> = Vec::new();

    let mut i = 0usize;
    while i < args.len() {
        let a = &args[i];
        if !a.starts_with("--") && component.is_none() {
            component = Some(PathBuf::from(a));
            i += 1;
            continue;
        }

        match a.as_str() {
            "--exports" => {
                let v = args.get(i + 1).unwrap_or_else(|| usage());
                exports = Some(PathBuf::from(v));
                i += 2;
            }
            "--rootDir" => {
                let v = args.get(i + 1).unwrap_or_else(|| usage());
                root_dir = PathBuf::from(v);
                i += 2;
            }
            "--budget" => {
                let v = args.get(i + 1).unwrap_or_else(|| usage());
                budget = v.parse::<u32>().unwrap_or_else(|_| usage());
                i += 2;
            }
            "--argv" => {
                let v = args.get(i + 1).unwrap_or_else(|| usage());
                argv.push(v.clone());
                i += 2;
            }
            "--arg" => {
                let v = args.get(i + 1).unwrap_or_else(|| usage());
                def_args.push(v.clone());
                i += 2;
            }
            _ => usage(),
        }
    }

    let component = component.unwrap_or_else(|| usage());
    let exports = exports.unwrap_or_else(|| usage());
    (component, exports, root_dir, budget, argv, def_args)
}

fn find_main_def(exports_path: &Path) -> Result<ExportedDef> {
    let txt = fs::read_to_string(exports_path)
        .with_context(|| format!("failed to read exports manifest: {}", exports_path.display()))?;
    let m: ExportsManifest =
        serde_json::from_str(&txt).context("failed to parse exports manifest JSON")?;
    anyhow::ensure!(
        m.schema == "flix-llvm-wasm-exports-v0",
        "unsupported exports manifest schema: {}",
        m.schema
    );

    let hit = m
        .defs
        .into_iter()
        .find(|d| d.is_main)
        .context("no main def found in exports manifest")?;
    anyhow::ensure!(
        hit.params.len() as u64 == hit.arity,
        "exports manifest entry has inconsistent arity/params for {}",
        hit.symbol
    );
    Ok(hit)
}

fn box_arg<T>(store: &mut Store<T>, rt: &bindings::exports::flix::runtime::runtime::Guest, ctx: bindings::exports::flix::runtime::runtime::Ctx, tpe: &str, raw: &str) -> Result<Value> {
    match tpe {
        "Unit" => Ok(rt.call_box_i32(store, ctx, 0)?),
        "String" => Ok(rt.call_box_string(store, ctx, raw)?),
        "Int8" => {
            let n = raw
                .parse::<i8>()
                .with_context(|| format!("invalid Int8 arg: {raw}"))?;
            Ok(rt.call_box_i8(store, ctx, n)?)
        }
        "Int16" => {
            let n = raw
                .parse::<i16>()
                .with_context(|| format!("invalid Int16 arg: {raw}"))?;
            Ok(rt.call_box_i16(store, ctx, n)?)
        }
        "Int32" => {
            let n = raw
                .parse::<i32>()
                .with_context(|| format!("invalid Int32 arg: {raw}"))?;
            Ok(rt.call_box_i32(store, ctx, n)?)
        }
        "Int64" => {
            let n = raw
                .parse::<i64>()
                .with_context(|| format!("invalid Int64 arg: {raw}"))?;
            Ok(rt.call_box_i64(store, ctx, n)?)
        }
        "Float32" => {
            let n = raw
                .parse::<f32>()
                .with_context(|| format!("invalid Float32 arg: {raw}"))?;
            Ok(rt.call_box_f32(store, ctx, n)?)
        }
        "Float64" => {
            let n = raw
                .parse::<f64>()
                .with_context(|| format!("invalid Float64 arg: {raw}"))?;
            Ok(rt.call_box_f64(store, ctx, n)?)
        }
        "Bool" => match raw {
            "true" => Ok(rt.call_box_bool(store, ctx, true)?),
            "false" => Ok(rt.call_box_bool(store, ctx, false)?),
            _ => anyhow::bail!("invalid Bool arg: {raw} (expected 'true' or 'false')"),
        },
        _ => anyhow::bail!("unsupported param type: {tpe}"),
    }
}

fn main() -> Result<()> {
    let argv0: Vec<String> = std::env::args().skip(1).collect();
    let (component_path, exports_path, root_dir, budget, argv, def_args) = parse_args(&argv0);

    let main = find_main_def(&exports_path)?;

    let mut config = Config::new();
    config.wasm_component_model(true);
    let engine = Engine::new(&config)?;

    let component = Component::from_file(&engine, &component_path)
        .with_context(|| format!("failed to load component: {}", component_path.display()))?;

    let mut linker = Linker::<State>::new(&engine);
    bindings::flix::sys::sys::add_to_linker::<_, HasSelf<_>>(&mut linker, |s| s)?;

    let mut store = Store::new(
        &engine,
        State {
            argv,
        },
    );

    let flix = bindings::Flix::instantiate(&mut store, &component, &linker)?;
    let rt = flix.flix_runtime_runtime();

    let ctx = rt.call_new_ctx(&mut store)?;

    let runner = FlixRunner { budget };
    let mut handlers = StdHostHandlers::new(root_dir);

    let raw_args: Vec<String> = def_args;
    let boxed_args: Vec<Value> = if raw_args.is_empty() && main.params.iter().all(|t| t == "Unit") {
        let mut out = Vec::new();
        for _ in 0..main.params.len() {
            out.push(box_arg(&mut store, rt, ctx, "Unit", "")?);
        }
        out
    } else if raw_args.len() == main.params.len() {
        let mut out = Vec::new();
        for (tpe, raw) in main.params.iter().zip(raw_args.iter()) {
            out.push(box_arg(&mut store, rt, ctx, tpe, raw)?);
        }
        out
    } else {
        anyhow::bail!(
            "wrong arity for {} (def-id={}): expected {} args ({}), got {}",
            main.symbol,
            main.def_id,
            main.params.len(),
            main.params.join(", "),
            raw_args.len()
        );
    };

    let task_id = rt.call_start_task(&mut store, ctx, main.def_id, &boxed_args)?;
    for v in boxed_args {
        let _ = v.resource_drop(&mut store);
    }
    let out = runner.run_task_to_completion(&mut store, rt, ctx, task_id, &mut handlers)?;

    match out {
        bindings::exports::flix::runtime::runtime::TaskOutcome::Ok(v) => {
            let _ = v.resource_drop(&mut store);
            Ok(())
        }
        bindings::exports::flix::runtime::runtime::TaskOutcome::Thrown(v) => {
            let _ = v.resource_drop(&mut store);
            anyhow::bail!(
                "guest threw while executing {} (def-id={})",
                main.symbol,
                main.def_id
            );
        }
    }
}
