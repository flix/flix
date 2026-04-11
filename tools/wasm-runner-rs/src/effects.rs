use anyhow::{Context, Result};
use serde::Deserialize;
use std::fs;
use std::path::Path;
use wasmtime::Store;

use crate::bindings::exports::flix::runtime::runtime::{Ctx, Guest, Suspension, Value};

#[derive(Debug, Clone, Deserialize)]
pub struct EffectManifest {
    pub schema: String,
    pub ops: Vec<EffectOp>,
}

impl EffectManifest {
    pub fn from_path(path: &Path) -> Result<Self> {
        let txt = fs::read_to_string(path)
            .with_context(|| format!("failed to read effect manifest: {}", path.display()))?;
        let manifest: Self =
            serde_json::from_str(&txt).context("failed to parse effect manifest JSON")?;
        anyhow::ensure!(
            manifest.schema == "flix-llvm-wasm-effects-v0",
            "unsupported effect manifest schema: {}",
            manifest.schema
        );
        Ok(manifest)
    }

    pub fn lookup(&self, eff_sym_id: u64, op_index: u32) -> Option<&EffectOp> {
        self.ops
            .iter()
            .find(|op| op.eff_sym_id == eff_sym_id && op.op_index == op_index)
    }
}

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct EffectOp {
    pub effect: String,
    pub eff_sym_id: u64,
    pub op: String,
    pub op_symbol: String,
    pub op_index: u32,
    pub params: Vec<AbiType>,
    pub result: AbiType,
}

#[derive(Debug, Clone, Deserialize)]
pub struct RecordField {
    pub label: String,
    #[serde(rename = "type")]
    pub tpe: AbiType,
}

#[derive(Debug, Clone, Deserialize)]
pub struct ListRepr {
    #[serde(rename = "nilTagId")]
    pub nil_tag_id: u64,
    #[serde(rename = "consTagId")]
    pub cons_tag_id: u64,
}

#[derive(Debug, Clone, Deserialize)]
pub struct OptionRepr {
    #[serde(rename = "noneTagId")]
    pub none_tag_id: u64,
    #[serde(rename = "someTagId")]
    pub some_tag_id: u64,
}

#[derive(Debug, Clone, Deserialize)]
pub struct ResultRepr {
    #[serde(rename = "errTagId")]
    pub err_tag_id: u64,
    #[serde(rename = "okTagId")]
    pub ok_tag_id: u64,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(tag = "kind")]
pub enum AbiType {
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
    List {
        element: Box<AbiType>,
        repr: Option<ListRepr>,
    },
    #[serde(rename = "array")]
    Array { element: Box<AbiType> },
    #[serde(rename = "tuple")]
    Tuple { elements: Vec<AbiType> },
    #[serde(rename = "option")]
    Option {
        element: Box<AbiType>,
        repr: Option<OptionRepr>,
    },
    #[serde(rename = "result")]
    Result {
        ok: Box<AbiType>,
        err: Box<AbiType>,
        repr: Option<ResultRepr>,
    },
    #[serde(rename = "record")]
    Record { fields: Vec<RecordField> },
}

impl AbiType {
    pub fn is_leaf(&self) -> bool {
        matches!(
            self,
            AbiType::Unit
                | AbiType::Bool
                | AbiType::Int8
                | AbiType::Int16
                | AbiType::Int32
                | AbiType::Int64
                | AbiType::Float32
                | AbiType::Float64
                | AbiType::String
                | AbiType::Bytes
        )
    }

    pub fn is_pointer_like(&self) -> bool {
        matches!(
            self,
            AbiType::String
                | AbiType::Bytes
                | AbiType::List { .. }
                | AbiType::Array { .. }
                | AbiType::Tuple { .. }
                | AbiType::Option { .. }
                | AbiType::Result { .. }
                | AbiType::Record { .. }
        )
    }

    pub fn kind_name(&self) -> &'static str {
        match self {
            AbiType::Unit => "unit",
            AbiType::Bool => "bool",
            AbiType::Int8 => "int8",
            AbiType::Int16 => "int16",
            AbiType::Int32 => "int32",
            AbiType::Int64 => "int64",
            AbiType::Float32 => "float32",
            AbiType::Float64 => "float64",
            AbiType::String => "string",
            AbiType::Bytes => "bytes",
            AbiType::List { .. } => "list",
            AbiType::Array { .. } => "array",
            AbiType::Tuple { .. } => "tuple",
            AbiType::Option { .. } => "option",
            AbiType::Result { .. } => "result",
            AbiType::Record { .. } => "record",
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LeafValue {
    Unit,
    Bool(bool),
    Int8(i8),
    Int16(i16),
    Int32(i32),
    Int64(i64),
    Float32(f32),
    Float64(f64),
    String(String),
    Bytes(Vec<u8>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValueData {
    Unit,
    Bool(bool),
    Int8(i8),
    Int16(i16),
    Int32(i32),
    Int64(i64),
    Float32(f32),
    Float64(f64),
    String(String),
    Bytes(Vec<u8>),
    List(Vec<ValueData>),
    Array(Vec<ValueData>),
    Tuple(Vec<ValueData>),
    Option(Option<Box<ValueData>>),
    Result(std::result::Result<Box<ValueData>, Box<ValueData>>),
    Record(Vec<(String, ValueData)>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AsyncResult<T> {
    Ready(T),
    Pending,
}

pub fn decode_leaf_args<T>(
    store: &mut Store<T>,
    rt: &Guest,
    ctx: Ctx,
    suspension: Suspension,
    op: &EffectOp,
) -> Result<Vec<LeafValue>> {
    decode_args(store, rt, ctx, suspension, op)?
        .into_iter()
        .map(expect_leaf)
        .collect()
}

pub fn decode_leaf_arg<T>(
    store: &mut Store<T>,
    rt: &Guest,
    ctx: Ctx,
    suspension: Suspension,
    idx: u32,
    tpe: &AbiType,
) -> Result<LeafValue> {
    expect_leaf(decode_arg(store, rt, ctx, suspension, idx, tpe)?)
}

pub fn decode_args<T>(
    store: &mut Store<T>,
    rt: &Guest,
    ctx: Ctx,
    suspension: Suspension,
    op: &EffectOp,
) -> Result<Vec<ValueData>> {
    let count = rt
        .call_suspension_arg_count(&mut *store, ctx, suspension)
        .context("suspension-arg-count failed")?;
    anyhow::ensure!(
        count as usize == op.params.len(),
        "bad suspension arity for {}: expected {}, got {}",
        op.op_symbol,
        op.params.len(),
        count
    );

    op.params
        .iter()
        .enumerate()
        .map(|(idx, tpe)| decode_arg(store, rt, ctx, suspension, idx as u32, tpe))
        .collect()
}

pub fn decode_arg<T>(
    store: &mut Store<T>,
    rt: &Guest,
    ctx: Ctx,
    suspension: Suspension,
    idx: u32,
    tpe: &AbiType,
) -> Result<ValueData> {
    let value = if tpe.is_pointer_like() {
        rt.call_suspension_arg_as_ptr(&mut *store, ctx, suspension, idx)?
    } else {
        rt.call_suspension_arg_as_i64(&mut *store, ctx, suspension, idx)?
    };
    decode_owned_value(store, rt, ctx, value, tpe)
}

fn tag_field_for_type<T>(
    store: &mut Store<T>,
    rt: &Guest,
    ctx: Ctx,
    value: Value,
    idx: u32,
    tpe: &AbiType,
) -> Result<Value> {
    if tpe.is_pointer_like() {
        rt.call_tag_field_ptr(store, ctx, value, idx)
    } else {
        rt.call_tag_field_i64(store, ctx, value, idx)
    }
}

pub fn box_leaf_value<T>(
    store: &mut Store<T>,
    rt: &Guest,
    ctx: Ctx,
    tpe: &AbiType,
    value: &LeafValue,
) -> Result<Value> {
    let data = match value {
        LeafValue::Unit => ValueData::Unit,
        LeafValue::Bool(x) => ValueData::Bool(*x),
        LeafValue::Int8(x) => ValueData::Int8(*x),
        LeafValue::Int16(x) => ValueData::Int16(*x),
        LeafValue::Int32(x) => ValueData::Int32(*x),
        LeafValue::Int64(x) => ValueData::Int64(*x),
        LeafValue::Float32(x) => ValueData::Float32(*x),
        LeafValue::Float64(x) => ValueData::Float64(*x),
        LeafValue::String(x) => ValueData::String(x.clone()),
        LeafValue::Bytes(x) => ValueData::Bytes(x.clone()),
    };
    box_value(store, rt, ctx, tpe, &data)
}

pub fn box_value<T>(
    store: &mut Store<T>,
    rt: &Guest,
    ctx: Ctx,
    tpe: &AbiType,
    value: &ValueData,
) -> Result<Value> {
    match (tpe, value) {
        (AbiType::Unit, _) => Ok(rt.call_box_i32(store, ctx, 0)?),
        (AbiType::Bool, ValueData::Bool(x)) => Ok(rt.call_box_bool(store, ctx, *x)?),
        (AbiType::Int8, ValueData::Int8(x)) => Ok(rt.call_box_i8(store, ctx, *x)?),
        (AbiType::Int16, ValueData::Int16(x)) => Ok(rt.call_box_i16(store, ctx, *x)?),
        (AbiType::Int32, ValueData::Int32(x)) => Ok(rt.call_box_i32(store, ctx, *x)?),
        (AbiType::Int64, ValueData::Int64(x)) => Ok(rt.call_box_i64(store, ctx, *x)?),
        (AbiType::Float32, ValueData::Float32(x)) => Ok(rt.call_box_f32(store, ctx, *x)?),
        (AbiType::Float64, ValueData::Float64(x)) => Ok(rt.call_box_f64(store, ctx, *x)?),
        (AbiType::String, ValueData::String(x)) => Ok(rt.call_box_string(store, ctx, x)?),
        (AbiType::Bytes, ValueData::Bytes(x)) => Ok(rt.call_box_bytes(store, ctx, x)?),

        (AbiType::Tuple { elements }, ValueData::Tuple(values)) => {
            anyhow::ensure!(
                elements.len() == values.len(),
                "bad tuple value: expected arity {}, got {}",
                elements.len(),
                values.len()
            );
            let handles = box_child_values(store, rt, ctx, elements.iter().zip(values.iter()))?;
            let out = rt.call_tuple_new(&mut *store, ctx, &handles)?;
            drop_values(store, handles);
            Ok(out)
        }

        (AbiType::Record { fields }, ValueData::Record(values)) => {
            anyhow::ensure!(
                fields.len() == values.len(),
                "bad record value: expected {} fields, got {}",
                fields.len(),
                values.len()
            );
            let mut handles = Vec::with_capacity(fields.len());
            for (field, value_field) in fields.iter().zip(values.iter()) {
                anyhow::ensure!(
                    field.label == value_field.0,
                    "bad record value: expected field '{}', got '{}'",
                    field.label,
                    value_field.0
                );
                handles.push(box_value(store, rt, ctx, &field.tpe, &value_field.1)?);
            }
            let out = rt.call_tuple_new(&mut *store, ctx, &handles)?;
            drop_values(store, handles);
            Ok(out)
        }

        (AbiType::Option { element: _, repr }, ValueData::Option(None)) => {
            let repr = option_repr(repr.as_ref());
            Ok(rt.call_tag_new(store, ctx, repr.none_tag_id, &[])?)
        }
        (AbiType::Option { element, repr }, ValueData::Option(Some(v))) => {
            let repr = option_repr(repr.as_ref());
            let field = box_value(store, rt, ctx, element, v)?;
            let out = rt.call_tag_new(&mut *store, ctx, repr.some_tag_id, &[field])?;
            drop_value(store, field);
            Ok(out)
        }

        (AbiType::Result { ok, err: _, repr }, ValueData::Result(Ok(v))) => {
            let repr = result_repr(repr.as_ref());
            let field = box_value(store, rt, ctx, ok, v)?;
            let out = rt.call_tag_new(&mut *store, ctx, repr.ok_tag_id, &[field])?;
            drop_value(store, field);
            Ok(out)
        }
        (AbiType::Result { ok: _, err, repr }, ValueData::Result(Err(v))) => {
            let repr = result_repr(repr.as_ref());
            let field = box_value(store, rt, ctx, err, v)?;
            let out = rt.call_tag_new(&mut *store, ctx, repr.err_tag_id, &[field])?;
            drop_value(store, field);
            Ok(out)
        }

        (AbiType::Array { element }, ValueData::Array(values)) => {
            let handles = box_child_values(
                store,
                rt,
                ctx,
                values.iter().map(|v| (&**element, v)),
            )?;
            let out = rt.call_array_new(&mut *store, ctx, element.is_pointer_like(), &handles)?;
            drop_values(store, handles);
            Ok(out)
        }

        (AbiType::List { element, repr }, ValueData::List(values)) => {
            let repr = list_repr(repr.as_ref());
            let mut current = rt.call_tag_new(&mut *store, ctx, repr.nil_tag_id, &[])?;
            for value in values.iter().rev() {
                let head = box_value(store, rt, ctx, element, value)?;
                let next = rt.call_tag_new(&mut *store, ctx, repr.cons_tag_id, &[head, current])?;
                drop_value(store, head);
                drop_value(store, current);
                current = next;
            }
            Ok(current)
        }

        (other_tpe, other_value) => anyhow::bail!(
            "mismatched async effect value for {}: {:?}",
            other_tpe.kind_name(),
            other_value
        ),
    }
}

pub fn resume_throw_string<T>(
    store: &mut Store<T>,
    rt: &Guest,
    ctx: Ctx,
    suspension: Suspension,
    msg: impl AsRef<str>,
) -> Result<()> {
    let v = rt.call_box_string(&mut *store, ctx, msg.as_ref())?;
    rt.call_resume_throw(&mut *store, ctx, suspension, v)?;
    drop_value(store, v);
    Ok(())
}

fn decode_owned_value<T>(
    store: &mut Store<T>,
    rt: &Guest,
    ctx: Ctx,
    value: Value,
    tpe: &AbiType,
) -> Result<ValueData> {
    let decoded = decode_value(store, rt, ctx, value, tpe)?;
    drop_value(store, value);
    Ok(decoded)
}

fn decode_value<T>(
    store: &mut Store<T>,
    rt: &Guest,
    ctx: Ctx,
    value: Value,
    tpe: &AbiType,
) -> Result<ValueData> {
    match tpe {
        AbiType::Unit => Ok(ValueData::Unit),
        AbiType::Bool => Ok(ValueData::Bool(rt.call_unbox_bool(store, ctx, value)?)),
        AbiType::Int8 => Ok(ValueData::Int8(rt.call_unbox_i8(store, ctx, value)?)),
        AbiType::Int16 => Ok(ValueData::Int16(rt.call_unbox_i16(store, ctx, value)?)),
        AbiType::Int32 => Ok(ValueData::Int32(rt.call_unbox_i32(store, ctx, value)?)),
        AbiType::Int64 => Ok(ValueData::Int64(rt.call_unbox_i64(store, ctx, value)?)),
        AbiType::Float32 => Ok(ValueData::Float32(rt.call_unbox_f32(store, ctx, value)?)),
        AbiType::Float64 => Ok(ValueData::Float64(rt.call_unbox_f64(store, ctx, value)?)),
        AbiType::String => Ok(ValueData::String(rt.call_unbox_string(store, ctx, value)?)),
        AbiType::Bytes => Ok(ValueData::Bytes(rt.call_unbox_bytes(store, ctx, value)?)),

        AbiType::Tuple { elements } => {
            let mut out = Vec::with_capacity(elements.len());
            for (idx, elm) in elements.iter().enumerate() {
                let field = rt.call_tuple_field(&mut *store, ctx, value, idx as u32)?;
                out.push(decode_owned_value(store, rt, ctx, field, elm)?);
            }
            Ok(ValueData::Tuple(out))
        }

        AbiType::Record { fields } => {
            let mut out = Vec::with_capacity(fields.len());
            for (idx, field) in fields.iter().enumerate() {
                let handle = rt.call_tuple_field(&mut *store, ctx, value, idx as u32)?;
                let decoded = decode_owned_value(store, rt, ctx, handle, &field.tpe)?;
                out.push((field.label.clone(), decoded));
            }
            Ok(ValueData::Record(out))
        }

        AbiType::Option { element, repr } => {
            let repr = option_repr(repr.as_ref());
            let tag_id = rt.call_tag_id(&mut *store, ctx, value)?;
            if tag_id == repr.none_tag_id {
                Ok(ValueData::Option(None))
            } else if tag_id == repr.some_tag_id {
                let field = tag_field_for_type(store, rt, ctx, value, 0, element)?;
                let decoded = decode_owned_value(store, rt, ctx, field, element)?;
                Ok(ValueData::Option(Some(Box::new(decoded))))
            } else {
                anyhow::bail!(
                    "bad async effect option tag id: expected {}/{} got {}",
                    repr.none_tag_id,
                    repr.some_tag_id,
                    tag_id
                )
            }
        }

        AbiType::Result { ok, err, repr } => {
            let repr = result_repr(repr.as_ref());
            let tag_id = rt.call_tag_id(&mut *store, ctx, value)?;
            if tag_id == repr.ok_tag_id {
                let field = tag_field_for_type(store, rt, ctx, value, 0, ok)?;
                let decoded = decode_owned_value(store, rt, ctx, field, ok)?;
                Ok(ValueData::Result(Ok(Box::new(decoded))))
            } else if tag_id == repr.err_tag_id {
                let field = tag_field_for_type(store, rt, ctx, value, 0, err)?;
                let decoded = decode_owned_value(store, rt, ctx, field, err)?;
                Ok(ValueData::Result(Err(Box::new(decoded))))
            } else {
                anyhow::bail!(
                    "bad async effect result tag id: expected {}/{} got {}",
                    repr.err_tag_id,
                    repr.ok_tag_id,
                    tag_id
                )
            }
        }

        AbiType::Array { element } => {
            let len = rt.call_array_len(&mut *store, ctx, value)?;
            let mut out = Vec::with_capacity(len as usize);
            for idx in 0..len {
                let elm = rt.call_array_elem(&mut *store, ctx, value, idx)?;
                out.push(decode_owned_value(store, rt, ctx, elm, element)?);
            }
            Ok(ValueData::Array(out))
        }

        AbiType::List { element, repr } => {
            let repr = list_repr(repr.as_ref());
            let len = rt.call_list_len(&mut *store, ctx, value, repr.nil_tag_id, repr.cons_tag_id)?;
            let mut out = Vec::with_capacity(len as usize);
            let mut current = value;
            let mut owns_current = false;
            for _ in 0..len {
                let head = tag_field_for_type(store, rt, ctx, current, 0, element)?;
                let next = rt.call_tag_field_ptr(&mut *store, ctx, current, 1)?;
                out.push(decode_owned_value(store, rt, ctx, head, element)?);
                if owns_current {
                    drop_value(store, current);
                }
                current = next;
                owns_current = true;
            }
            if owns_current {
                drop_value(store, current);
            }
            Ok(ValueData::List(out))
        }
    }
}

fn expect_leaf(value: ValueData) -> Result<LeafValue> {
    match value {
        ValueData::Unit => Ok(LeafValue::Unit),
        ValueData::Bool(x) => Ok(LeafValue::Bool(x)),
        ValueData::Int8(x) => Ok(LeafValue::Int8(x)),
        ValueData::Int16(x) => Ok(LeafValue::Int16(x)),
        ValueData::Int32(x) => Ok(LeafValue::Int32(x)),
        ValueData::Int64(x) => Ok(LeafValue::Int64(x)),
        ValueData::Float32(x) => Ok(LeafValue::Float32(x)),
        ValueData::Float64(x) => Ok(LeafValue::Float64(x)),
        ValueData::String(x) => Ok(LeafValue::String(x)),
        ValueData::Bytes(x) => Ok(LeafValue::Bytes(x)),
        other => anyhow::bail!("expected leaf async effect value, got {:?}", other),
    }
}

fn drop_value<T>(store: &mut Store<T>, value: Value) {
    let _ = value.resource_drop(store);
}

fn drop_values<T>(store: &mut Store<T>, values: Vec<Value>) {
    for value in values {
        drop_value(store, value);
    }
}

fn box_child_values<'a, T, I>(
    store: &mut Store<T>,
    rt: &Guest,
    ctx: Ctx,
    it: I,
) -> Result<Vec<Value>>
where
    I: IntoIterator<Item = (&'a AbiType, &'a ValueData)>,
{
    let mut out = Vec::new();
    for (tpe, value) in it {
        out.push(box_value(store, rt, ctx, tpe, value)?);
    }
    Ok(out)
}

fn list_repr(repr: Option<&ListRepr>) -> ListRepr {
    repr.cloned().unwrap_or(ListRepr {
        nil_tag_id: 1,
        cons_tag_id: 0,
    })
}

fn option_repr(repr: Option<&OptionRepr>) -> OptionRepr {
    repr.cloned().unwrap_or(OptionRepr {
        none_tag_id: 0,
        some_tag_id: 1,
    })
}

fn result_repr(repr: Option<&ResultRepr>) -> ResultRepr {
    repr.cloned().unwrap_or(ResultRepr {
        err_tag_id: 0,
        ok_tag_id: 1,
    })
}
