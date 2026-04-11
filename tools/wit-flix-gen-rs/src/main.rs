use anyhow::{bail, Context, Result};
use serde::Serialize;
use std::env;
use std::path::PathBuf;
use wit_parser::{
    Function, FunctionKind, Handle, Resolve, Result_, Type, TypeDefKind, TypeId, WorldId,
    WorldItem, WorldKey,
};

#[derive(Debug, Serialize)]
struct WorldIr {
    schema: &'static str,
    world: String,
    imports: Vec<ImportInterface>,
}

#[derive(Debug, Serialize)]
struct ImportInterface {
    kind: &'static str,
    interface: String,
    namespace: String,
    package_name: String,
    interface_name: String,
    version: String,
    resources: Vec<ImportResource>,
    functions: Vec<ImportFunction>,
}

#[derive(Debug, Serialize)]
struct ImportResource {
    name: String,
}

#[derive(Debug, Serialize)]
struct ImportFunction {
    name: String,
    kind: &'static str,
    resource: Option<String>,
    params: Vec<ImportParam>,
    result: AbiType,
}

#[derive(Debug, Serialize)]
struct ImportParam {
    name: String,
    tpe: AbiType,
}

#[derive(Debug, Serialize)]
#[serde(tag = "kind")]
enum AbiType {
    #[serde(rename = "unit")]
    Unit,
    #[serde(rename = "bool")]
    Bool,
    #[serde(rename = "int8")]
    Int8,
    #[serde(rename = "int16")]
    Int16,
    #[serde(rename = "int32")]
    Int32,
    #[serde(rename = "int64")]
    Int64,
    #[serde(rename = "float32")]
    Float32,
    #[serde(rename = "float64")]
    Float64,
    #[serde(rename = "string")]
    String,
    #[serde(rename = "bytes")]
    Bytes,
    #[serde(rename = "list")]
    List { element: Box<AbiType> },
    #[serde(rename = "tuple")]
    Tuple { elements: Vec<AbiType> },
    #[serde(rename = "option")]
    Option { element: Box<AbiType> },
    #[serde(rename = "result")]
    Result {
        ok: Box<AbiType>,
        err: Box<AbiType>,
    },
    #[serde(rename = "record")]
    Record {
        name: Option<String>,
        fields: Vec<RecordField>,
    },
    #[serde(rename = "resource")]
    Resource {
        resource: String,
        ownership: &'static str,
    },
}

#[derive(Debug, Serialize)]
struct RecordField {
    label: String,
    tpe: AbiType,
}

fn main() -> Result<()> {
    let mut wit_dir: Option<PathBuf> = None;
    let mut world_name: Option<String> = None;

    let mut args = env::args().skip(1);
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--wit" => {
                let Some(v) = args.next() else {
                    bail!("missing value for --wit");
                };
                wit_dir = Some(PathBuf::from(v));
            }
            "--world" => {
                let Some(v) = args.next() else {
                    bail!("missing value for --world");
                };
                world_name = Some(v);
            }
            other => bail!("unrecognized argument: {}", other),
        }
    }

    let wit_dir = wit_dir.context("missing --wit")?;
    let world_name = world_name.context("missing --world")?;

    let mut resolve = Resolve::default();
    let (pkg_id, _files) = resolve
        .push_dir(&wit_dir)
        .with_context(|| format!("failed to parse WIT package at {}", wit_dir.display()))?;
    let world = resolve
        .select_world(&[pkg_id], Some(&world_name))
        .with_context(|| format!("failed to select WIT world '{}'", world_name))?;

    let ir = lower_world(&resolve, world)?;
    println!("{}", serde_json::to_string_pretty(&ir)?);
    Ok(())
}

fn lower_world(resolve: &Resolve, world_id: WorldId) -> Result<WorldIr> {
    let world = &resolve.worlds[world_id];
    let Some(pkg_id) = world.package else {
        bail!("inline WIT worlds are not supported in v0");
    };
    let pkg = &resolve.packages[pkg_id];
    let ns = pkg.name.namespace.to_string();
    let pkg_name = pkg.name.name.to_string();
    let version = pkg
        .name
        .version
        .as_ref()
        .map(|v| v.to_string())
        .unwrap_or_else(|| "0.0.0".to_string());
    let mut imports = Vec::new();
    let mut world_resources = Vec::new();
    let mut world_functions = Vec::new();

    for (key, item) in world.imports.iter() {
        match item {
            WorldItem::Interface { id, .. } => {
                imports.push(lower_interface(resolve, key, *id)?);
            }
            WorldItem::Function(func) => {
                world_functions.push(lower_world_function(resolve, key, func)?);
            }
            WorldItem::Type { id, .. } => {
                if matches!(resolve.types[*id].kind, TypeDefKind::Resource) {
                    let WorldKey::Name(name) = key else {
                        bail!("unnamed imported world-level resource is not supported in v0");
                    };
                    world_resources.push(ImportResource {
                        name: name.to_string(),
                    });
                }
            }
        }
    }

    if !world_functions.is_empty() || !world_resources.is_empty() {
        world_resources.sort_by(|a, b| a.name.cmp(&b.name));
        world_functions.sort_by(|a, b| a.name.cmp(&b.name));
        imports.push(ImportInterface {
            kind: "world",
            interface: format!("{}:{}/{}@{}", ns, pkg_name, world.name, version),
            namespace: ns,
            package_name: pkg_name,
            interface_name: world.name.to_string(),
            version,
            resources: world_resources,
            functions: world_functions,
        });
    }

    imports.sort_by(|a, b| a.interface.cmp(&b.interface));

    Ok(WorldIr {
        schema: "flix-wit-world-v0",
        world: world.name.to_string(),
        imports,
    })
}

fn lower_interface(resolve: &Resolve, key: &WorldKey, interface_id: wit_parser::InterfaceId) -> Result<ImportInterface> {
    let iface = &resolve.interfaces[interface_id];
    let Some(pkg_id) = iface.package else {
        bail!("inline WIT interfaces are not supported in v0");
    };
    let pkg = &resolve.packages[pkg_id];
    let ns = pkg.name.namespace.to_string();
    let pkg_name = pkg.name.name.to_string();
    let version = pkg
        .name
        .version
        .as_ref()
        .map(|v| v.to_string())
        .unwrap_or_else(|| "0.0.0".to_string());

    let interface_name = match key {
        WorldKey::Name(name) => name.to_string(),
        WorldKey::Interface(id) => {
            let name = resolve.interfaces[*id]
                .name
                .as_ref()
                .context("unnamed imported interface is not supported in v0")?;
            name.to_string()
        }
    };

    let mut resources = iface
        .types
        .iter()
        .filter_map(|(name, id)| match &resolve.types[*id].kind {
            TypeDefKind::Resource => Some(ImportResource {
                name: name.to_string(),
            }),
            _ => None,
        })
        .collect::<Vec<_>>();
    resources.sort_by(|a, b| a.name.cmp(&b.name));

    let mut functions = iface
        .functions
        .values()
        .map(|func| lower_function(resolve, func))
        .collect::<Result<Vec<_>>>()?;
    functions.sort_by(|a, b| a.name.cmp(&b.name));

    Ok(ImportInterface {
        kind: "interface",
        interface: format!("{}:{}/{}@{}", ns, pkg_name, interface_name, version),
        namespace: ns,
        package_name: pkg_name,
        interface_name,
        version,
        resources,
        functions,
    })
}

fn lower_world_function(resolve: &Resolve, key: &WorldKey, func: &Function) -> Result<ImportFunction> {
    let (kind, resource_name) = match func.kind {
        FunctionKind::Freestanding | FunctionKind::AsyncFreestanding => ("freestanding", None),
        FunctionKind::Method(id) | FunctionKind::AsyncMethod(id) => {
            ("method", Some(resource_type_name(resolve, id)?))
        }
        FunctionKind::Static(id) | FunctionKind::AsyncStatic(id) => {
            ("static", Some(resource_type_name(resolve, id)?))
        }
        FunctionKind::Constructor(id) => ("constructor", Some(resource_type_name(resolve, id)?)),
    };

    let lowered_name = match (kind, resource_name.as_deref()) {
        ("freestanding", _) => match key {
            WorldKey::Name(name) => name.to_string(),
            WorldKey::Interface(_) => bail!("unexpected interface key for world-level function"),
        },
        ("constructor", Some(resource)) => format!("{}-new", resource),
        ("method", Some(resource)) | ("static", Some(resource)) => {
            format!("{}-{}", resource, func.item_name())
        }
        _ => bail!("unexpected world-level resource function metadata"),
    };

    let params = match kind {
        "method" => func.params.iter().skip(1).collect::<Vec<_>>(),
        _ => func.params.iter().collect::<Vec<_>>(),
    }
    .into_iter()
    .map(|param| {
        Ok(ImportParam {
            name: param.name.to_string(),
            tpe: lower_type(resolve, param.ty, true)?,
        })
    })
    .collect::<Result<Vec<_>>>()?;
    let result = match kind {
        "constructor" => AbiType::Resource {
            resource: resource_name.clone().context("missing constructor resource name")?,
            ownership: "own",
        },
        _ => lower_results(resolve, func.result)?,
    };

    Ok(ImportFunction {
        name: lowered_name,
        kind,
        resource: resource_name,
        params,
        result,
    })
}

fn lower_function(resolve: &Resolve, func: &Function) -> Result<ImportFunction> {
    let (kind, resource_name) = match func.kind {
        FunctionKind::Freestanding | FunctionKind::AsyncFreestanding => ("freestanding", None),
        FunctionKind::Method(id) | FunctionKind::AsyncMethod(id) => {
            ("method", Some(resource_type_name(resolve, id)?))
        }
        FunctionKind::Static(id) | FunctionKind::AsyncStatic(id) => {
            ("static", Some(resource_type_name(resolve, id)?))
        }
        FunctionKind::Constructor(id) => ("constructor", Some(resource_type_name(resolve, id)?)),
    };

    let lowered_name = match (kind, resource_name.as_deref()) {
        ("freestanding", _) => func.item_name().to_string(),
        ("constructor", Some(resource)) => format!("{}-new", resource),
        ("method", Some(resource)) | ("static", Some(resource)) => {
            format!("{}-{}", resource, func.item_name())
        }
        _ => bail!("unexpected resource function metadata"),
    };

    let params = match kind {
        "method" => func.params.iter().skip(1).collect::<Vec<_>>(),
        _ => func.params.iter().collect::<Vec<_>>(),
    }
        .into_iter()
        .map(|param| {
            Ok(ImportParam {
                name: param.name.to_string(),
                tpe: lower_type(resolve, param.ty, true)?,
            })
        })
        .collect::<Result<Vec<_>>>()?;
    let result = match kind {
        "constructor" => AbiType::Resource {
            resource: resource_name.clone().context("missing constructor resource name")?,
            ownership: "own",
        },
        _ => lower_results(resolve, func.result)?,
    };

    Ok(ImportFunction {
        name: lowered_name,
        kind,
        resource: resource_name,
        params,
        result,
    })
}

fn lower_results(resolve: &Resolve, result: Option<Type>) -> Result<AbiType> {
    match result {
        Some(tpe) => lower_type(resolve, tpe, true),
        None => Ok(AbiType::Unit),
    }
}

fn lower_type(resolve: &Resolve, tpe: Type, allow_resource: bool) -> Result<AbiType> {
    match tpe {
        Type::Bool => Ok(AbiType::Bool),
        Type::S8 => Ok(AbiType::Int8),
        Type::S16 => Ok(AbiType::Int16),
        Type::S32 => Ok(AbiType::Int32),
        Type::S64 => Ok(AbiType::Int64),
        Type::F32 => Ok(AbiType::Float32),
        Type::F64 => Ok(AbiType::Float64),
        Type::String => Ok(AbiType::String),
        Type::U8 => bail!("unsupported WIT type 'u8' outside list<u8> bytes"),
        Type::U16 => bail!("unsupported WIT type 'u16' in v0"),
        Type::U32 => bail!("unsupported WIT type 'u32' in v0"),
        Type::U64 => bail!("unsupported WIT type 'u64' in v0"),
        Type::Char => bail!("unsupported WIT type 'char' in v0"),
        Type::ErrorContext => bail!("unsupported WIT type 'error-context' in v0"),
        Type::Id(id) => lower_type_id(resolve, id, allow_resource),
    }
}

fn lower_type_id(resolve: &Resolve, id: TypeId, allow_resource: bool) -> Result<AbiType> {
    let tdef = &resolve.types[id];
    match &tdef.kind {
        TypeDefKind::Type(tpe) => lower_type(resolve, *tpe, allow_resource),
        TypeDefKind::List(tpe) => match tpe {
            Type::U8 => Ok(AbiType::Bytes),
            _ => Ok(AbiType::List {
                element: Box::new(lower_type(resolve, *tpe, allow_resource)?),
            }),
        },
        TypeDefKind::Tuple(tup) => Ok(AbiType::Tuple {
            elements: tup
                .types
                .iter()
                .map(|tpe| lower_type(resolve, *tpe, allow_resource))
                .collect::<Result<Vec<_>>>()?,
        }),
        TypeDefKind::Option(tpe) => Ok(AbiType::Option {
            element: Box::new(lower_type(resolve, *tpe, allow_resource)?),
        }),
        TypeDefKind::Result(Result_ { ok, err }) => Ok(AbiType::Result {
            ok: Box::new(match ok {
                Some(tpe) => lower_type(resolve, *tpe, allow_resource)?,
                None => AbiType::Unit,
            }),
            err: Box::new(match err {
                Some(tpe) => lower_type(resolve, *tpe, allow_resource)?,
                None => AbiType::Unit,
            }),
        }),
        TypeDefKind::Record(record) => Ok(AbiType::Record {
            name: tdef.name.clone(),
            fields: record
                .fields
                .iter()
                .map(|field| {
                    Ok(RecordField {
                        label: field.name.to_string(),
                        tpe: lower_type(resolve, field.ty, allow_resource)?,
                    })
                })
                .collect::<Result<Vec<_>>>()?,
        }),
        TypeDefKind::Handle(Handle::Own(id)) => {
            if !allow_resource {
                bail!("nested owned WIT resource handles are not supported in async wasm effect bindings v0");
            }
            Ok(AbiType::Resource {
                resource: resource_type_name(resolve, *id)?,
                ownership: "own",
            })
        }
        TypeDefKind::Handle(Handle::Borrow(id)) => {
            if !allow_resource {
                bail!("nested borrowed WIT resource handles are not supported in async wasm effect bindings v0");
            }
            Ok(AbiType::Resource {
                resource: resource_type_name(resolve, *id)?,
                ownership: "borrow",
            })
        }
        TypeDefKind::Resource => bail!("bare WIT resources are not valid parameter/result types"),
        TypeDefKind::Flags(_) => bail!("WIT flags are not supported in async wasm effect bindings v0"),
        TypeDefKind::Enum(_) => bail!("WIT enums are not supported in async wasm effect bindings v0"),
        TypeDefKind::Variant(_) => bail!("WIT variants are not supported in async wasm effect bindings v0"),
        TypeDefKind::Map(_, _) => bail!("WIT maps are not supported in async wasm effect bindings v0"),
        TypeDefKind::FixedLengthList(_, _) => {
            bail!("WIT fixed-length lists are not supported in async wasm effect bindings v0")
        }
        TypeDefKind::Future(_) => bail!("WIT future types are not supported in async wasm effect bindings v0"),
        TypeDefKind::Stream(_) => bail!("WIT stream types are not supported in async wasm effect bindings v0"),
        TypeDefKind::Unknown => bail!("unresolved WIT type encountered"),
    }
}

fn resource_type_name(resolve: &Resolve, id: TypeId) -> Result<String> {
    let tdef = &resolve.types[id];
    tdef.name
        .clone()
        .context("unnamed WIT resource is not supported in async wasm effect bindings v0")
}
