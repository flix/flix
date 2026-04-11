import { loadEffectManifest, makeUnknownEffectHandler } from "./effect-handlers.mjs";

function toCamelCase(name) {
  const parts = String(name).split("-").filter((x) => x.length > 0);
  if (parts.length === 0) return "";
  return parts[0] + parts.slice(1).map((s) => s[0].toUpperCase() + s.slice(1)).join("");
}

export async function loadWitEffectBindings(source) {
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
    throw new Error("loadWitEffectBindings expects a parsed manifest object, filesystem path, or URL");
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
      throw new Error(`failed to load WIT effect bindings: ${res.status} ${res.statusText}`);
    }
    return await res.json();
  }

  const fs = await import("node:fs/promises");
  const txt = await fs.readFile(source, { encoding: "utf8" });
  return JSON.parse(txt);
}

function resolveInterfaceImpl(implementations, binding) {
  if (!implementations || typeof implementations !== "object") {
    return null;
  }

  return (
    implementations[binding.interface] ??
    implementations[binding.interfaceKey] ??
    null
  );
}

function resolveFunc(implementations, binding) {
  const iface = resolveInterfaceImpl(implementations, binding);
  if (!iface || typeof iface !== "object") {
    return null;
  }

  const fn = iface[binding.func] ?? iface[toCamelCase(binding.func)];
  return typeof fn === "function" ? fn : null;
}

function isResourcePublicType(tpe) {
  return tpe?.kind === "resource";
}

function toPublicBindingValue(value, tpe) {
  switch (tpe?.kind) {
    case "resource":
      if (typeof value !== "bigint") {
        throw new Error(`bad raw WIT resource handle for ${tpe.resource}: expected bigint`);
      }
      return {
        __resource: tpe.resource,
        __ownership: tpe.ownership,
        id: value,
      };
    case "list":
    case "array":
      return Array.isArray(value) ? value.map((x) => toPublicBindingValue(x, tpe.element)) : value;
    case "tuple":
      return Array.isArray(value) ? value.map((x, i) => toPublicBindingValue(x, tpe.elements?.[i])) : value;
    case "option":
      return value == null ? value : toPublicBindingValue(value, tpe.element);
    case "result":
      if (!value || typeof value !== "object") return value;
      if (value.tag === "ok") return { tag: "ok", val: toPublicBindingValue(value.val, tpe.ok) };
      if (value.tag === "err") return { tag: "err", val: toPublicBindingValue(value.val, tpe.err) };
      return value;
    case "record":
      if (!value || typeof value !== "object") return value;
      return Object.fromEntries((tpe.fields ?? []).map((field) => [field.label, toPublicBindingValue(value[field.label], field.type)]));
    default:
      return value;
  }
}

function fromPublicBindingValue(value, tpe) {
  switch (tpe?.kind) {
    case "resource": {
      if (!value || typeof value !== "object") {
        throw new Error(`bad WIT resource value for ${tpe.resource}: expected object handle`);
      }
      const id = value.id;
      if (typeof id !== "bigint") {
        throw new Error(`bad WIT resource value for ${tpe.resource}: expected bigint id`);
      }
      return id;
    }
    case "list":
    case "array":
      return Array.isArray(value) ? value.map((x) => fromPublicBindingValue(x, tpe.element)) : value;
    case "tuple":
      return Array.isArray(value) ? value.map((x, i) => fromPublicBindingValue(x, tpe.elements?.[i])) : value;
    case "option":
      return value == null ? value : fromPublicBindingValue(value, tpe.element);
    case "result":
      if (!value || typeof value !== "object") return value;
      if (value.tag === "ok") return { tag: "ok", val: fromPublicBindingValue(value.val, tpe.ok) };
      if (value.tag === "err") return { tag: "err", val: fromPublicBindingValue(value.val, tpe.err) };
      return value;
    case "record":
      if (!value || typeof value !== "object") return value;
      return Object.fromEntries((tpe.fields ?? []).map((field) => [field.label, fromPublicBindingValue(value[field.label], field.type)]));
    default:
      return value;
  }
}

function mkHandlers(bindings, implementations) {
  if (bindings?.schema !== "flix-wasm-effect-bindings-v0") {
    throw new Error(`unsupported WIT effect bindings schema: ${bindings?.schema ?? "<missing>"}`);
  }

  const out = Object.create(null);
  const ops = Array.isArray(bindings.ops) ? bindings.ops : [];
  for (const binding of ops) {
    out[binding.opSymbol] = async (...args) => {
      const fn = resolveFunc(implementations, binding);
      if (typeof fn !== "function") {
        throw new Error(`missing WIT host implementation for ${binding.interface}#${binding.func}`);
      }
      const publicArgs = Array.isArray(binding.params)
        ? binding.params.map((tpe, idx) => toPublicBindingValue(args[idx], tpe))
        : args;
      const result = await fn(...publicArgs);
      return fromPublicBindingValue(result, binding.result);
    };
  }
  return out;
}

export async function makeWitEffectUnknownHandler(effectManifestSource, bindingsSource, implementations) {
  const [effectManifest, bindings] = await Promise.all([
    loadEffectManifest(effectManifestSource),
    loadWitEffectBindings(bindingsSource),
  ]);
  return makeUnknownEffectHandler(effectManifest, mkHandlers(bindings, implementations));
}

export function makeWitEffectHandlers(bindings, implementations) {
  return mkHandlers(bindings, implementations);
}
