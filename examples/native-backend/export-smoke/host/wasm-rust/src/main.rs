use anyhow::{bail, Context, Result};
use std::path::PathBuf;
use wasmtime::{
    Config, Engine, Store,
    component::{Component, HasSelf, Linker},
};

mod bindings {
    wasmtime::component::bindgen!({
        path: "../../build/wasm/sdk/wit",
        world: "flix",
    });
}

use bindings::exports::flix::exports::api;
use bindings::flix::sys::sys::{Capability, Host as SysHost, LogLevel};

#[derive(Default)]
struct State;

impl SysHost for State {
    fn log(&mut self, _level: LogLevel, _msg: String) {}

    fn time_now_ms(&mut self) -> i64 {
        0
    }

    fn random_bytes(&mut self, len: u32) -> Vec<u8> {
        (0..len).map(|i| (i as u8).wrapping_mul(31)).collect()
    }

    fn get_args(&mut self) -> Vec<String> {
        Vec::new()
    }

    fn has_capability(&mut self, _cap: Capability) -> bool {
        false
    }
}

fn main() -> Result<()> {
    let default_component = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../build/wasm/sdk/component/export-smoke.exports.component.wasm");
    let component_path = std::env::args()
        .nth(1)
        .map(PathBuf::from)
        .unwrap_or(default_component);

    let mut config = Config::new();
    config.wasm_component_model(true);
    let engine = Engine::new(&config)?;

    let component = Component::from_file(&engine, &component_path)
        .with_context(|| format!("failed to load component: {}", component_path.display()))?;

    let mut linker = Linker::<State>::new(&engine);
    bindings::flix::sys::sys::add_to_linker::<_, HasSelf<_>>(&mut linker, |s| s)?;

    let mut store = Store::new(&engine, State::default());
    let flix = bindings::Flix::instantiate(&mut store, &component, &linker)?;
    let api_exports = flix.flix_exports_api();
    let ctx_api = api_exports.ctx();
    let ctx = ctx_api.call_constructor(&mut store)?;

    match ctx_api.call_api_add(&mut store, ctx, 1, 2)? {
        api::ExecInt32::Ok(v) if v == 3 => {}
        other => bail!("bad add result: {:?}", other),
    }

    match ctx_api.call_api_echo(&mut store, ctx, "hello")? {
        api::ExecString::Ok(v) if v == "hello" => {}
        other => bail!("bad echo result: {:?}", other),
    }

    let data = vec![0u8, 1, 2, 255];
    match ctx_api.call_api_bytesid(&mut store, ctx, &data)? {
        api::ExecBytes::Ok(v) if v == data => {}
        other => bail!("bad bytes result: {:?}", other),
    }

    let some_in = api::OptionInt32 { is_some: true, val: 41 };
    match ctx_api.call_api_maybesucc(&mut store, ctx, some_in)? {
        api::ExecOptionInt32::Ok(v) if v.is_some && v.val == 42 => {}
        other => bail!("bad maybeSucc(Some): {:?}", other),
    }

    let none_in = api::OptionInt32 { is_some: false, val: 0 };
    match ctx_api.call_api_maybesucc(&mut store, ctx, none_in)? {
        api::ExecOptionInt32::Ok(v) if !v.is_some => {}
        other => bail!("bad maybeSucc(None): {:?}", other),
    }

    let pair_in = api::Tuple2Int32String {
        f0: 7,
        f1: "hello".to_string(),
    };
    match ctx_api.call_api_flippair(&mut store, ctx, &pair_in)? {
        api::ExecTuple2StringInt32::Ok(v) if v.f0 == "hello" && v.f1 == 7 => {}
        other => bail!("bad flipPair result: {:?}", other),
    }

    match ctx_api.call_api_halfeven(&mut store, ctx, 8)? {
        api::ExecResultInt32String::Ok(v) if v.is_ok && v.ok == 4 => {}
        other => bail!("bad halfEven(8): {:?}", other),
    }

    match ctx_api.call_api_halfeven(&mut store, ctx, 7)? {
        api::ExecResultInt32String::Ok(v) if !v.is_ok && v.err == "odd" => {}
        other => bail!("bad halfEven(7): {:?}", other),
    }

    let badge_in = api::RecordNameStringScoreInt32 {
        name: "hello".to_string(),
        score: 41,
    };
    match ctx_api.call_api_badge(&mut store, ctx, &badge_in)? {
        api::ExecRecordLabelStringScoreInt32::Ok(v) if v.label == "hello" && v.score == 42 => {}
        other => bail!("bad badge result: {:?}", other),
    }

    let ints = vec![1i32, 2, 3];
    match ctx_api.call_api_prependanswer(&mut store, ctx, &ints)? {
        api::ExecListInt32::Ok(v) if v == vec![42, 1, 2, 3] => {}
        other => bail!("bad prependAnswer result: {:?}", other),
    }

    match ctx_api.call_api_echoints(&mut store, ctx, &ints)? {
        api::ExecArrayInt32::Ok(v) if v == vec![1, 2, 3] => {}
        other => bail!("bad echoInts result: {:?}", other),
    }

    let names = vec!["hello".to_string(), "world".to_string()];
    match ctx_api.call_api_echonames(&mut store, ctx, &names)? {
        api::ExecArrayString::Ok(v) if v == names => {}
        other => bail!("bad echoNames result: {:?}", other),
    }

    let users = vec![
        api::RecordNameStringScoreInt32 {
            name: "hello".to_string(),
            score: 41,
        },
        api::RecordNameStringScoreInt32 {
            name: "world".to_string(),
            score: 9,
        },
    ];
    match ctx_api.call_api_promoteusers(&mut store, ctx, &users)? {
        api::ExecListRecordLabelStringScoreInt32::Ok(v)
            if v.len() == 2
                && v[0].label == "hello"
                && v[0].score == 42
                && v[1].label == "world"
                && v[1].score == 10 => {}
        other => bail!("bad promoteUsers result: {:?}", other),
    }

    match ctx_api.call_api_echouserarray(&mut store, ctx, &users)? {
        api::ExecArrayRecordNameStringScoreInt32::Ok(v)
            if v.len() == 2
                && v[0].name == "hello"
                && v[0].score == 41
                && v[1].name == "world"
                && v[1].score == 9 => {}
        other => bail!("bad echoUserArray result: {:?}", other),
    }

    let pairs = vec![
        api::Tuple2Int32String {
            f0: 7,
            f1: "hello".to_string(),
        },
        api::Tuple2Int32String {
            f0: 9,
            f1: "world".to_string(),
        },
    ];
    match ctx_api.call_api_flippairs(&mut store, ctx, &pairs)? {
        api::ExecListTuple2StringInt32::Ok(v)
            if v.len() == 2
                && v[0].f0 == "hello"
                && v[0].f1 == 7
                && v[1].f0 == "world"
                && v[1].f1 == 9 => {}
        other => bail!("bad flipPairs result: {:?}", other),
    }

    let maybe_ints = vec![
        api::OptionInt32 {
            is_some: true,
            val: 41,
        },
        api::OptionInt32 {
            is_some: false,
            val: 0,
        },
        api::OptionInt32 {
            is_some: true,
            val: 9,
        },
    ];
    match ctx_api.call_api_echomaybeints(&mut store, ctx, &maybe_ints)? {
        api::ExecArrayOptionInt32::Ok(v)
            if v.len() == 3
                && v[0].is_some
                && v[0].val == 41
                && !v[1].is_some
                && v[2].is_some
                && v[2].val == 9 => {}
        other => bail!("bad echoMaybeInts result: {:?}", other),
    }

    let susp = match ctx_api.call_api_suspendecho(&mut store, ctx, "hello")? {
        api::ExecString::Suspended(s) => s,
        other => bail!("bad suspend result: {:?}", other),
    };

    let req = ctx_api.call_request_api_suspendecho(&mut store, ctx, susp)?;
    if req.arg0 != "hello" {
        bail!("bad suspension arg: {}", req.arg0);
    }

    match ctx_api.call_resume_api_suspendecho(&mut store, ctx, susp, "ok")? {
        api::ExecString::Ok(v) if v == "ok" => {}
        other => bail!("bad resume result: {:?}", other),
    }
    ctx.resource_drop(&mut store)?;

    println!("OK");
    Ok(())
}
