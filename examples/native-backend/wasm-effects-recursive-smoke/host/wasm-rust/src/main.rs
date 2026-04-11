use anyhow::{Context, Result};
use flix_wit_effect_bindings::{
    bindings,
    effects::{AsyncResult, EffectManifest},
    runner::FlixRunner,
};
use serde::Deserialize;
use std::{fs, path::PathBuf};
use wasmtime::{
    Config, Engine, Store,
    component::{Component, HasSelf, Linker},
};

use bindings::flix::sys::sys::{Capability, Host as SysHost, LogLevel};
use flix_wit_effect_bindings::{HostDemoUsers, WitEffectHandler};

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

struct HostUsers;

impl HostDemoUsers for HostUsers {
    fn pair_users(&mut self, users: Vec<(String, i32)>) -> Result<AsyncResult<Vec<(String, i32)>>> {
        Ok(AsyncResult::Ready(
            users
                .into_iter()
                .map(|(name, score)| (name.to_uppercase(), score + 10))
                .collect(),
        ))
    }

    fn promote(&mut self, user: Option<(String, i32)>) -> Result<AsyncResult<std::result::Result<(String, i32), i32>>> {
        Ok(AsyncResult::Ready(match user {
            None => Err(0),
            Some((name, score)) => Ok((format!("{}!", name), score + 1)),
        }))
    }
}

fn main() -> Result<()> {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..");
    let component_path = root.join("build/wasm/llvm/wasm/wasm-effects-recursive-smoke.component.wasm");
    let exports_manifest_path = root.join("build/wasm/llvm/wasm-effects-recursive-smoke.exports.json");
    let effects_manifest_path = root.join("build/wasm/llvm/wasm-effects-recursive-smoke.effects.json");

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
        .find(|d| d.symbol == "Api.callUsers")
        .context("missing Api.callUsers export")?;

    let manifest = EffectManifest::from_path(&effects_manifest_path)?;
    let mut handler = WitEffectHandler::new(manifest, HostUsers);

    let ctx = rt.call_new_ctx(&mut store)?;
    let arg = rt.call_box_i32(&mut store, ctx, 10)?;
    let task_id = rt.call_start_task(&mut store, ctx, main_def.def_id, &[arg])?;
    let _ = arg.resource_drop(&mut store);

    let runner = FlixRunner { budget: 100 };
    let outcome = runner.run_task_to_completion(&mut store, rt, ctx, task_id, &mut handler)?;
    match outcome {
        bindings::exports::flix::runtime::runtime::TaskOutcome::Ok(v) => {
            let result = rt.call_unbox_string(&mut store, ctx, v)?;
            anyhow::ensure!(result == "alice!:11|ALICE:11,BOB:12", "bad result: {}", result);
            let _ = v.resource_drop(&mut store);
        }
        other => anyhow::bail!("unexpected task outcome: {:?}", other),
    }

    println!("OK");
    Ok(())
}
