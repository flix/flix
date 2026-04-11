const disposeSym = Symbol.dispose ?? Symbol.for("dispose");

function maybeDispose(x) {
  try {
    const fn = x?.[disposeSym];
    if (typeof fn === "function") fn.call(x);
  } catch {
    // Best-effort cleanup for host-side tooling.
  }
}

function isScalarAbiType(tpe) {
  switch (tpe?.kind) {
    case "unit":
    case "bool":
    case "int8":
    case "int16":
    case "int32":
    case "int64":
    case "float32":
    case "float64":
      return true;
    default:
      return false;
  }
}

function isPointerLikeAbiType(tpe) {
  switch (tpe?.kind) {
    case "string":
    case "bytes":
    case "list":
    case "array":
    case "tuple":
    case "option":
    case "result":
    case "record":
      return true;
    default:
      return false;
  }
}

function listRepr(tpe) {
  const repr = tpe?.repr ?? {};
  return {
    nilTagId: repr.nilTagId ?? 1,
    consTagId: repr.consTagId ?? 0,
  };
}

function optionRepr(tpe) {
  const repr = tpe?.repr ?? {};
  return {
    noneTagId: repr.noneTagId ?? 0,
    someTagId: repr.someTagId ?? 1,
  };
}

function resultRepr(tpe) {
  const repr = tpe?.repr ?? {};
  return {
    errTagId: repr.errTagId ?? 0,
    okTagId: repr.okTagId ?? 1,
  };
}

function normalizeTagId(tagId) {
  return typeof tagId === "bigint" ? tagId : BigInt(tagId);
}

function sameTagId(lhs, rhs) {
  return normalizeTagId(lhs) === normalizeTagId(rhs);
}

function tagField(runtime, ctx, valueHandle, idx, tpe) {
  return isPointerLikeAbiType(tpe)
    ? runtime.tagFieldPtr(ctx, valueHandle, idx)
    : runtime.tagFieldI64(ctx, valueHandle, idx);
}

function flattenHandlers(handlers) {
  const out = new Map();
  if (!handlers || typeof handlers !== "object") return out;

  for (const [key, value] of Object.entries(handlers)) {
    if (typeof value === "function") {
      out.set(key, value);
      continue;
    }
    if (value && typeof value === "object") {
      for (const [op, fn] of Object.entries(value)) {
        if (typeof fn === "function") {
          out.set(`${key}.${op}`, fn);
        }
      }
    }
  }

  return out;
}

function indexManifest(manifest) {
  if (manifest?.schema !== "flix-llvm-wasm-effects-v0") {
    throw new Error(`unsupported effect manifest schema: ${manifest?.schema ?? "<missing>"}`);
  }

  const out = new Map();
  const ops = Array.isArray(manifest.ops) ? manifest.ops : [];
  for (const entry of ops) {
    out.set(`${entry.effSymId}:${entry.opIndex}`, entry);
  }
  return out;
}

function resolveHandler(handlerMap, entry) {
  return handlerMap.get(entry.opSymbol) ?? handlerMap.get(`${entry.effect}.${entry.op}`) ?? null;
}

function decodeLeafValue(runtime, ctx, valueHandle, tpe) {
  switch (tpe.kind) {
    case "unit":
      return undefined;
    case "bool":
      return runtime.unboxBool(ctx, valueHandle);
    case "int8":
      return runtime.unboxI8(ctx, valueHandle);
    case "int16":
      return runtime.unboxI16(ctx, valueHandle);
    case "int32":
      return runtime.unboxI32(ctx, valueHandle);
    case "int64":
      return runtime.unboxI64(ctx, valueHandle);
    case "float32":
      return runtime.unboxF32(ctx, valueHandle);
    case "float64":
      return runtime.unboxF64(ctx, valueHandle);
    case "string":
      return runtime.unboxString(ctx, valueHandle);
    case "bytes":
      return runtime.unboxBytes(ctx, valueHandle);
    default:
      throw new Error(`unsupported async effect ABI type: ${tpe?.kind ?? "<missing>"}`);
  }
}

function decodeValue(runtime, ctx, valueHandle, tpe) {
  switch (tpe?.kind) {
    case "unit":
    case "bool":
    case "int8":
    case "int16":
    case "int32":
    case "int64":
    case "float32":
    case "float64":
    case "string":
    case "bytes":
      return decodeLeafValue(runtime, ctx, valueHandle, tpe);

    case "tuple": {
      return tpe.elements.map((elm, idx) => {
        const field = runtime.tupleField(ctx, valueHandle, idx);
        try {
          return decodeValue(runtime, ctx, field, elm);
        } finally {
          maybeDispose(field);
        }
      });
    }

    case "record": {
      const out = {};
      for (const [idx, fieldTpe] of tpe.fields.entries()) {
        const field = runtime.tupleField(ctx, valueHandle, idx);
        try {
          out[fieldTpe.label] = decodeValue(runtime, ctx, field, fieldTpe.type);
        } finally {
          maybeDispose(field);
        }
      }
      return out;
    }

    case "option": {
      const repr = optionRepr(tpe);
      const tagId = runtime.tagId(ctx, valueHandle);
      if (sameTagId(tagId, repr.noneTagId)) return null;
      if (!sameTagId(tagId, repr.someTagId)) {
        throw new Error(`bad async effect option tag id: expected ${repr.noneTagId}/${repr.someTagId}, got ${tagId}`);
      }
      const field = tagField(runtime, ctx, valueHandle, 0, tpe.element);
      try {
        return decodeValue(runtime, ctx, field, tpe.element);
      } finally {
        maybeDispose(field);
      }
    }

    case "result": {
      const repr = resultRepr(tpe);
      const tagId = runtime.tagId(ctx, valueHandle);
      if (sameTagId(tagId, repr.okTagId)) {
        const field = tagField(runtime, ctx, valueHandle, 0, tpe.ok);
        try {
          return { tag: "ok", val: decodeValue(runtime, ctx, field, tpe.ok) };
        } finally {
          maybeDispose(field);
        }
      }
      if (sameTagId(tagId, repr.errTagId)) {
        const field = tagField(runtime, ctx, valueHandle, 0, tpe.err);
        try {
          return { tag: "err", val: decodeValue(runtime, ctx, field, tpe.err) };
        } finally {
          maybeDispose(field);
        }
      }
      throw new Error(`bad async effect result tag id: expected ${repr.errTagId}/${repr.okTagId}, got ${tagId}`);
    }

    case "array": {
      const len = runtime.arrayLen(ctx, valueHandle);
      const out = new Array(len);
      for (let i = 0; i < len; i++) {
        const elm = runtime.arrayElem(ctx, valueHandle, i);
        try {
          out[i] = decodeValue(runtime, ctx, elm, tpe.element);
        } finally {
          maybeDispose(elm);
        }
      }
      return out;
    }

    case "list": {
      const repr = listRepr(tpe);
      const len = runtime.listLen(ctx, valueHandle, repr.nilTagId, repr.consTagId);
      const out = new Array(len);
      let current = valueHandle;
      let ownsCurrent = false;
      for (let i = 0; i < len; i++) {
        const head = tagField(runtime, ctx, current, 0, tpe.element);
        const next = runtime.tagFieldPtr(ctx, current, 1);
        try {
          out[i] = decodeValue(runtime, ctx, head, tpe.element);
        } finally {
          maybeDispose(head);
          if (ownsCurrent) maybeDispose(current);
        }
        current = next;
        ownsCurrent = true;
      }
      if (ownsCurrent) maybeDispose(current);
      return out;
    }

    default:
      throw new Error(`unsupported async effect ABI type: ${tpe?.kind ?? "<missing>"}`);
  }
}

function decodeArg(runtime, ctx, suspension, idx, tpe) {
  const valueHandle = isPointerLikeAbiType(tpe)
    ? runtime.suspensionArgAsPtr(ctx, suspension, idx)
    : runtime.suspensionArgAsI64(ctx, suspension, idx);
  try {
    return decodeValue(runtime, ctx, valueHandle, tpe);
  } finally {
    maybeDispose(valueHandle);
  }
}

function boxLeafValue(runtime, ctx, tpe, value) {
  switch (tpe.kind) {
    case "unit":
      return runtime.boxI32(ctx, 0);
    case "bool":
      return runtime.boxBool(ctx, Boolean(value));
    case "int8":
      return runtime.boxI8(ctx, value);
    case "int16":
      return runtime.boxI16(ctx, value);
    case "int32":
      return runtime.boxI32(ctx, value);
    case "int64":
      return runtime.boxI64(ctx, value);
    case "float32":
      return runtime.boxF32(ctx, value);
    case "float64":
      return runtime.boxF64(ctx, value);
    case "string":
      return runtime.boxString(ctx, value);
    case "bytes":
      return runtime.boxBytes(ctx, value);
    default:
      throw new Error(`unsupported async effect result ABI type: ${tpe?.kind ?? "<missing>"}`);
  }
}

function boxValue(runtime, ctx, tpe, value) {
  switch (tpe?.kind) {
    case "unit":
    case "bool":
    case "int8":
    case "int16":
    case "int32":
    case "int64":
    case "float32":
    case "float64":
    case "string":
    case "bytes":
      return boxLeafValue(runtime, ctx, tpe, value);

    case "tuple": {
      if (!Array.isArray(value) || value.length !== tpe.elements.length) {
        throw new Error(`bad async effect tuple value: expected arity ${tpe.elements.length}`);
      }
      const handles = tpe.elements.map((elm, idx) => boxValue(runtime, ctx, elm, value[idx]));
      try {
        return runtime.tupleNew(ctx, handles);
      } finally {
        handles.forEach(maybeDispose);
      }
    }

    case "record": {
      if (!value || typeof value !== "object" || Array.isArray(value)) {
        throw new Error("bad async effect record value: expected object");
      }
      const handles = tpe.fields.map((field) => boxValue(runtime, ctx, field.type, value[field.label]));
      try {
        return runtime.tupleNew(ctx, handles);
      } finally {
        handles.forEach(maybeDispose);
      }
    }

    case "option": {
      const repr = optionRepr(tpe);
      if (value == null) {
        return runtime.tagNew(ctx, repr.noneTagId, []);
      }
      const field = boxValue(runtime, ctx, tpe.element, value);
      try {
        return runtime.tagNew(ctx, repr.someTagId, [field]);
      } finally {
        maybeDispose(field);
      }
    }

    case "result": {
      const repr = resultRepr(tpe);
      if (!value || typeof value !== "object" || (value.tag !== "ok" && value.tag !== "err")) {
        throw new Error('bad async effect result value: expected { tag: "ok" | "err", val: ... }');
      }
      if (value.tag === "ok") {
        const field = boxValue(runtime, ctx, tpe.ok, value.val);
        try {
          return runtime.tagNew(ctx, repr.okTagId, [field]);
        } finally {
          maybeDispose(field);
        }
      }
      const field = boxValue(runtime, ctx, tpe.err, value.val);
      try {
        return runtime.tagNew(ctx, repr.errTagId, [field]);
      } finally {
        maybeDispose(field);
      }
    }

    case "array": {
      if (!Array.isArray(value)) {
        throw new Error("bad async effect array value: expected array");
      }
      const handles = value.map((elm) => boxValue(runtime, ctx, tpe.element, elm));
      try {
        return runtime.arrayNew(ctx, isPointerLikeAbiType(tpe.element), handles);
      } finally {
        handles.forEach(maybeDispose);
      }
    }

    case "list": {
      if (!Array.isArray(value)) {
        throw new Error("bad async effect list value: expected array");
      }
      const repr = listRepr(tpe);
      let current = runtime.tagNew(ctx, repr.nilTagId, []);
      try {
        for (let i = value.length - 1; i >= 0; i--) {
          const head = boxValue(runtime, ctx, tpe.element, value[i]);
          try {
            const next = runtime.tagNew(ctx, repr.consTagId, [head, current]);
            maybeDispose(current);
            current = next;
          } finally {
            maybeDispose(head);
          }
        }
        return current;
      } catch (e) {
        maybeDispose(current);
        throw e;
      }
    }

    default:
      throw new Error(`unsupported async effect result ABI type: ${tpe?.kind ?? "<missing>"}`);
  }
}

function resumeThrowString(runtime, ctx, suspension, msg) {
  const v = runtime.boxString(ctx, String(msg));
  try {
    runtime.resumeThrow(ctx, suspension, v);
  } finally {
    maybeDispose(v);
  }
}

export async function loadEffectManifest(source) {
  if (source && typeof source === "object" && source.schema) {
    return source;
  }

  if (source instanceof URL && source.protocol === "file:") {
    const fs = await import("node:fs/promises");
    const { fileURLToPath } = await import("node:url");
    const txt = await fs.readFile(fileURLToPath(source), { encoding: "utf8" });
    return JSON.parse(txt);
  }

  if (source instanceof URL) {
    source = source.toString();
  }

  if (typeof source !== "string") {
    throw new Error("loadEffectManifest expects a parsed manifest object, filesystem path, or URL");
  }

  if (source.startsWith("file:")) {
    const fs = await import("node:fs/promises");
    const { fileURLToPath } = await import("node:url");
    const txt = await fs.readFile(fileURLToPath(source), { encoding: "utf8" });
    return JSON.parse(txt);
  }

  if (typeof fetch === "function" && /^[a-zA-Z][a-zA-Z0-9+.-]*:/.test(source)) {
    const res = await fetch(source);
    if (!res.ok) {
      throw new Error(`failed to load effect manifest: ${res.status} ${res.statusText}`);
    }
    return await res.json();
  }

  const fs = await import("node:fs/promises");
  const txt = await fs.readFile(source, { encoding: "utf8" });
  return JSON.parse(txt);
}

/**
 * Builds a handler for `unknown(effId, opId)` suspensions using compiler-emitted effect metadata.
 */
export function makeUnknownEffectHandler(manifest, handlers = {}) {
  const entries = indexManifest(manifest);
  const handlerMap = flattenHandlers(handlers);

  return async ({ runtime, ctx, suspension, request }) => {
    const entry = entries.get(`${request.effId}:${request.opId}`);
    if (!entry) {
      resumeThrowString(runtime, ctx, suspension, `unsupported effect op: effId=${request.effId} opId=${request.opId}`);
      return;
    }

    const handler = resolveHandler(handlerMap, entry);
    if (typeof handler !== "function") {
      resumeThrowString(runtime, ctx, suspension, `missing host effect handler for ${entry.opSymbol}`);
      return;
    }

    try {
      const argCount = runtime.suspensionArgCount(ctx, suspension);
      if (argCount !== entry.params.length) {
        resumeThrowString(
          runtime,
          ctx,
          suspension,
          `bad suspension arity for ${entry.opSymbol}: expected ${entry.params.length}, got ${argCount}`
        );
        return;
      }

      const args = entry.params.map((tpe, idx) => decodeArg(runtime, ctx, suspension, idx, tpe));
      const result = await handler(...args);
      const boxed = boxValue(runtime, ctx, entry.result, result);
      try {
        runtime.resumeOk(ctx, suspension, boxed);
      } finally {
        maybeDispose(boxed);
      }
    } catch (e) {
      const msg = e instanceof Error ? e.message : String(e);
      resumeThrowString(runtime, ctx, suspension, msg);
    }
  };
}
