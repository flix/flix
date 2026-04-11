pub mod bindings {
    wasmtime::component::bindgen!({
        path: "../../runtime/wit/flix-bindings",
        world: "flix",
    });
}

pub mod effects;
pub mod host;
pub mod runner;
