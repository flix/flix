const KIND = {
  AlreadyExists: 0,
  ConnectionFailed: 1,
  Interrupted: 2,
  InvalidPath: 3,
  InvalidInput: 4,
  InvalidData: 5,
  NotFound: 6,
  NotFile: 7,
  NotDirectory: 8,
  PermissionDenied: 9,
  Timeout: 10,
  UnexpectedEof: 11,
  Unsupported: 12,
  UnknownHost: 13,
  Other: 14,
};

function toPosixPath(p) {
  return p.replaceAll("\\", "/");
}

function normalizeSandboxedPath(requestedPath) {
  if (typeof requestedPath !== "string") {
    return { tag: "err", err: { kindCode: KIND.InvalidPath, msg: "path must be a string" } };
  }

  if (requestedPath.includes("\0")) {
    return { tag: "err", err: { kindCode: KIND.InvalidPath, msg: "path contains NUL" } };
  }

  const posix = toPosixPath(requestedPath);
  if (posix.length === 0) {
    return { tag: "err", err: { kindCode: KIND.InvalidPath, msg: "empty path" } };
  }

  if (posix.startsWith("/")) {
    return { tag: "err", err: { kindCode: KIND.InvalidPath, msg: "absolute paths are not allowed" } };
  }

  const stack = [];
  for (const part of posix.split("/")) {
    if (part.length === 0 || part === ".") continue;
    if (part === "..") {
      if (stack.length === 0) {
        return { tag: "err", err: { kindCode: KIND.InvalidPath, msg: "path escapes sandbox" } };
      }
      stack.pop();
      continue;
    }
    stack.push(part);
  }

  if (stack.length === 0) {
    return { tag: "err", err: { kindCode: KIND.InvalidPath, msg: "empty path" } };
  }

  return { tag: "ok", segments: stack, posix: stack.join("/") };
}

function toOtherIoError(err) {
  if (!err || typeof err !== "object") return { kindCode: KIND.Other, msg: String(err) };
  const msg = typeof err.message === "string" ? err.message : String(err);
  return { kindCode: KIND.Other, msg };
}

function isDomExceptionNamed(err, name) {
  return !!err && typeof err === "object" && err.name === name;
}

function hasOpfs() {
  return typeof navigator?.storage?.getDirectory === "function";
}

async function getParentDir(rootDirHandle, segments, create) {
  let dir = rootDirHandle;
  for (const s of segments) {
    dir = await dir.getDirectoryHandle(s, { create });
  }
  return dir;
}

async function tryGetFileHandle(dir, name) {
  try {
    const fh = await dir.getFileHandle(name, { create: false });
    return { tag: "ok", fh };
  } catch (e) {
    return { tag: "err", err: e };
  }
}

async function tryGetDirectoryHandle(dir, name) {
  try {
    const dh = await dir.getDirectoryHandle(name, { create: false });
    return { tag: "ok", dh };
  } catch (e) {
    return { tag: "err", err: e };
  }
}

async function entryExists(rootDirHandle, segments) {
  const parentSegs = segments.slice(0, -1);
  const name = segments[segments.length - 1];

  let parent;
  try {
    parent = await getParentDir(rootDirHandle, parentSegs, false);
  } catch (_e) {
    return false;
  }

  const f = await tryGetFileHandle(parent, name);
  if (f.tag === "ok") return true;
  if (isDomExceptionNamed(f.err, "TypeMismatchError")) {
    // Not a file; may still be a directory.
  } else if (!isDomExceptionNamed(f.err, "NotFoundError")) {
    return false;
  }

  const d = await tryGetDirectoryHandle(parent, name);
  if (d.tag === "ok") return true;
  return false;
}

async function openFile(rootDirHandle, segments) {
  const parentSegs = segments.slice(0, -1);
  const name = segments[segments.length - 1];

  let parent;
  try {
    parent = await getParentDir(rootDirHandle, parentSegs, false);
  } catch (e) {
    return { tag: "err", err: e };
  }

  let fh;
  try {
    fh = await parent.getFileHandle(name, { create: false });
  } catch (e) {
    return { tag: "err", err: e };
  }

  try {
    const file = await fh.getFile();
    return { tag: "ok", fh, file };
  } catch (e) {
    return { tag: "err", err: e };
  }
}

async function writeWholeFile(fh, bytes) {
  const w = await fh.createWritable();
  try {
    await w.write(bytes);
  } finally {
    await w.close();
  }
}

async function appendBytes(fh, extraBytes) {
  // Fast path: keepExistingData + seek-to-end.
  try {
    const f = await fh.getFile();
    const w = await fh.createWritable({ keepExistingData: true });
    try {
      await w.seek(f.size);
      await w.write(extraBytes);
    } finally {
      await w.close();
    }
    return;
  } catch (_e) {
    // Fall back to read + rewrite.
  }

  const f = await fh.getFile();
  const existing = new Uint8Array(await f.arrayBuffer());
  const combined = new Uint8Array(existing.length + extraBytes.length);
  combined.set(existing, 0);
  combined.set(extraBytes, existing.length);
  await writeWholeFile(fh, combined);
}

/**
 * Returns a set of browser-backed handlers for the `file-*` ops in `flix:runtime@0.1.0`.
 *
 * Requires OPFS (Origin Private File System) via `navigator.storage.getDirectory()`.
 *
 * Notes:
 * - All paths are POSIX-style relative paths (no leading `/`).
 * - `file-*` errors follow JVM portable semantics (many failures map to `Other`).
 */
export function makeOpfsFsHandlers(options = {}) {
  const rootDirHandle = options.rootDirHandle;
  if (!rootDirHandle) throw new Error("makeOpfsFsHandlers: options.rootDirHandle is required");

  const tmpBaseName = options.tmpBaseName ?? "tmp";

  if (!hasOpfs()) {
    throw new Error("makeOpfsFsHandlers: OPFS is unavailable (navigator.storage.getDirectory missing)");
  }

  const resolvePath = (p) => normalizeSandboxedPath(p);

  return {
    "file-exists": async ({ runtime, ctx, suspension, request }) => {
      const rp = resolvePath(request.path);
      if (rp.tag === "err") return runtime.resumeFileExistsErr(ctx, suspension, rp.err);
      const exists = await entryExists(rootDirHandle, rp.segments);
      runtime.resumeFileExistsOk(ctx, suspension, exists);
    },

    "file-is-directory": async ({ runtime, ctx, suspension, request }) => {
      const rp = resolvePath(request.path);
      if (rp.tag === "err") return runtime.resumeFileIsDirectoryErr(ctx, suspension, rp.err);

      const parentSegs = rp.segments.slice(0, -1);
      const name = rp.segments[rp.segments.length - 1];

      try {
        const parent = await getParentDir(rootDirHandle, parentSegs, false);
        const d = await tryGetDirectoryHandle(parent, name);
        runtime.resumeFileIsDirectoryOk(ctx, suspension, d.tag === "ok");
      } catch (_e) {
        runtime.resumeFileIsDirectoryOk(ctx, suspension, false);
      }
    },

    "file-is-regular-file": async ({ runtime, ctx, suspension, request }) => {
      const rp = resolvePath(request.path);
      if (rp.tag === "err") return runtime.resumeFileIsRegularFileErr(ctx, suspension, rp.err);

      const parentSegs = rp.segments.slice(0, -1);
      const name = rp.segments[rp.segments.length - 1];

      try {
        const parent = await getParentDir(rootDirHandle, parentSegs, false);
        const f = await tryGetFileHandle(parent, name);
        runtime.resumeFileIsRegularFileOk(ctx, suspension, f.tag === "ok");
      } catch (_e) {
        runtime.resumeFileIsRegularFileOk(ctx, suspension, false);
      }
    },

    "file-is-readable": async ({ runtime, ctx, suspension, request }) => {
      const rp = resolvePath(request.path);
      if (rp.tag === "err") return runtime.resumeFileIsReadableErr(ctx, suspension, rp.err);
      runtime.resumeFileIsReadableOk(ctx, suspension, await entryExists(rootDirHandle, rp.segments));
    },

    "file-is-writable": async ({ runtime, ctx, suspension, request }) => {
      const rp = resolvePath(request.path);
      if (rp.tag === "err") return runtime.resumeFileIsWritableErr(ctx, suspension, rp.err);
      runtime.resumeFileIsWritableOk(ctx, suspension, await entryExists(rootDirHandle, rp.segments));
    },

    "file-is-executable": async ({ runtime, ctx, suspension, request }) => {
      const rp = resolvePath(request.path);
      if (rp.tag === "err") return runtime.resumeFileIsExecutableErr(ctx, suspension, rp.err);
      runtime.resumeFileIsExecutableOk(ctx, suspension, false);
    },

    "file-is-symbolic-link": async ({ runtime, ctx, suspension, request }) => {
      const rp = resolvePath(request.path);
      if (rp.tag === "err") return runtime.resumeFileIsSymbolicLinkErr(ctx, suspension, rp.err);
      runtime.resumeFileIsSymbolicLinkOk(ctx, suspension, false);
    },

    "file-access-time": async ({ runtime, ctx, suspension, request }) => {
      const rp = resolvePath(request.path);
      if (rp.tag === "err") return runtime.resumeFileAccessTimeErr(ctx, suspension, rp.err);

      const of = await openFile(rootDirHandle, rp.segments);
      if (of.tag === "err") return runtime.resumeFileAccessTimeErr(ctx, suspension, toOtherIoError(of.err));
      runtime.resumeFileAccessTimeErr(ctx, suspension, {
        kindCode: KIND.Unsupported,
        msg: "access-time unsupported in OPFS",
      });
    },

    "file-creation-time": async ({ runtime, ctx, suspension, request }) => {
      const rp = resolvePath(request.path);
      if (rp.tag === "err") return runtime.resumeFileCreationTimeErr(ctx, suspension, rp.err);

      const of = await openFile(rootDirHandle, rp.segments);
      if (of.tag === "err") return runtime.resumeFileCreationTimeErr(ctx, suspension, toOtherIoError(of.err));
      runtime.resumeFileCreationTimeErr(ctx, suspension, {
        kindCode: KIND.Unsupported,
        msg: "creation-time unsupported in OPFS",
      });
    },

    "file-modification-time": async ({ runtime, ctx, suspension, request }) => {
      const rp = resolvePath(request.path);
      if (rp.tag === "err") return runtime.resumeFileModificationTimeErr(ctx, suspension, rp.err);

      const of = await openFile(rootDirHandle, rp.segments);
      if (of.tag === "err") return runtime.resumeFileModificationTimeErr(ctx, suspension, toOtherIoError(of.err));

      runtime.resumeFileModificationTimeOk(ctx, suspension, BigInt(Math.trunc(of.file.lastModified)));
    },

    "file-size": async ({ runtime, ctx, suspension, request }) => {
      const rp = resolvePath(request.path);
      if (rp.tag === "err") return runtime.resumeFileSizeErr(ctx, suspension, rp.err);

      const of = await openFile(rootDirHandle, rp.segments);
      if (of.tag === "err") return runtime.resumeFileSizeErr(ctx, suspension, toOtherIoError(of.err));
      runtime.resumeFileSizeOk(ctx, suspension, BigInt(of.file.size));
    },

    "file-read": async ({ runtime, ctx, suspension, request }) => {
      const rp = resolvePath(request.path);
      if (rp.tag === "err") return runtime.resumeFileReadErr(ctx, suspension, rp.err);

      const of = await openFile(rootDirHandle, rp.segments);
      if (of.tag === "err") return runtime.resumeFileReadErr(ctx, suspension, toOtherIoError(of.err));

      try {
        runtime.resumeFileReadOk(ctx, suspension, await of.file.text());
      } catch (e) {
        runtime.resumeFileReadErr(ctx, suspension, toOtherIoError(e));
      }
    },

    "file-read-lines": async ({ runtime, ctx, suspension, request }) => {
      const rp = resolvePath(request.path);
      if (rp.tag === "err") return runtime.resumeFileReadLinesErr(ctx, suspension, rp.err);

      const of = await openFile(rootDirHandle, rp.segments);
      if (of.tag === "err") return runtime.resumeFileReadLinesErr(ctx, suspension, toOtherIoError(of.err));

      try {
        const data = await of.file.text();
        if (data.length === 0) {
          runtime.resumeFileReadLinesOk(ctx, suspension, []);
          return;
        }

        const lines = data.split(/\r\n|\n|\r/);
        if (lines.length > 0 && lines[lines.length - 1] === "") lines.pop();
        runtime.resumeFileReadLinesOk(ctx, suspension, lines);
      } catch (e) {
        runtime.resumeFileReadLinesErr(ctx, suspension, toOtherIoError(e));
      }
    },

    "file-read-bytes": async ({ runtime, ctx, suspension, request }) => {
      const rp = resolvePath(request.path);
      if (rp.tag === "err") return runtime.resumeFileReadBytesErr(ctx, suspension, rp.err);

      const of = await openFile(rootDirHandle, rp.segments);
      if (of.tag === "err") return runtime.resumeFileReadBytesErr(ctx, suspension, toOtherIoError(of.err));

      try {
        runtime.resumeFileReadBytesOk(ctx, suspension, new Uint8Array(await of.file.arrayBuffer()));
      } catch (e) {
        runtime.resumeFileReadBytesErr(ctx, suspension, toOtherIoError(e));
      }
    },

    "file-list": async ({ runtime, ctx, suspension, request }) => {
      const rp = resolvePath(request.path);
      if (rp.tag === "err") return runtime.resumeFileListErr(ctx, suspension, rp.err);

      const parentSegs = rp.segments.slice(0, -1);
      const name = rp.segments[rp.segments.length - 1];

      let dir;
      try {
        const parent = await getParentDir(rootDirHandle, parentSegs, false);
        const d = await tryGetDirectoryHandle(parent, name);
        if (d.tag === "err") {
          runtime.resumeFileListErr(ctx, suspension, { kindCode: KIND.NotDirectory, msg: "not a directory" });
          return;
        }
        dir = d.dh;
      } catch (_e) {
        runtime.resumeFileListErr(ctx, suspension, { kindCode: KIND.NotDirectory, msg: "not a directory" });
        return;
      }

      try {
        const names = [];
        for await (const [entryName] of dir.entries()) {
          names.push(entryName);
        }
        names.sort((a, b) => a.localeCompare(b));
        runtime.resumeFileListOk(ctx, suspension, names);
      } catch (_e) {
        runtime.resumeFileListErr(ctx, suspension, { kindCode: KIND.Other, msg: "I/O error" });
      }
    },

    "file-write": async ({ runtime, ctx, suspension, request }) => {
      const rp = resolvePath(request.path);
      if (rp.tag === "err") return runtime.resumeFileWriteErr(ctx, suspension, rp.err);

      const parentSegs = rp.segments.slice(0, -1);
      const name = rp.segments[rp.segments.length - 1];

      let parent;
      try {
        parent = await getParentDir(rootDirHandle, parentSegs, false);
      } catch (e) {
        runtime.resumeFileWriteErr(ctx, suspension, toOtherIoError(e));
        return;
      }

      try {
        const fh = await parent.getFileHandle(name, { create: true });
        await writeWholeFile(fh, request.data);
        runtime.resumeFileWriteOk(ctx, suspension);
      } catch (e) {
        runtime.resumeFileWriteErr(ctx, suspension, toOtherIoError(e));
      }
    },

    "file-write-bytes": async ({ runtime, ctx, suspension, request }) => {
      const rp = resolvePath(request.path);
      if (rp.tag === "err") return runtime.resumeFileWriteBytesErr(ctx, suspension, rp.err);

      const parentSegs = rp.segments.slice(0, -1);
      const name = rp.segments[rp.segments.length - 1];

      let parent;
      try {
        parent = await getParentDir(rootDirHandle, parentSegs, false);
      } catch (e) {
        runtime.resumeFileWriteBytesErr(ctx, suspension, toOtherIoError(e));
        return;
      }

      try {
        const fh = await parent.getFileHandle(name, { create: true });
        await writeWholeFile(fh, request.bytes);
        runtime.resumeFileWriteBytesOk(ctx, suspension);
      } catch (e) {
        runtime.resumeFileWriteBytesErr(ctx, suspension, toOtherIoError(e));
      }
    },

    "file-append": async ({ runtime, ctx, suspension, request }) => {
      const rp = resolvePath(request.path);
      if (rp.tag === "err") return runtime.resumeFileAppendErr(ctx, suspension, rp.err);

      const parentSegs = rp.segments.slice(0, -1);
      const name = rp.segments[rp.segments.length - 1];

      let parent;
      try {
        parent = await getParentDir(rootDirHandle, parentSegs, false);
      } catch (e) {
        runtime.resumeFileAppendErr(ctx, suspension, toOtherIoError(e));
        return;
      }

      try {
        const fh = await parent.getFileHandle(name, { create: true });
        const encoder = new TextEncoder();
        await appendBytes(fh, encoder.encode(request.data));
        runtime.resumeFileAppendOk(ctx, suspension);
      } catch (e) {
        runtime.resumeFileAppendErr(ctx, suspension, toOtherIoError(e));
      }
    },

    "file-append-bytes": async ({ runtime, ctx, suspension, request }) => {
      const rp = resolvePath(request.path);
      if (rp.tag === "err") return runtime.resumeFileAppendBytesErr(ctx, suspension, rp.err);

      const parentSegs = rp.segments.slice(0, -1);
      const name = rp.segments[rp.segments.length - 1];

      let parent;
      try {
        parent = await getParentDir(rootDirHandle, parentSegs, false);
      } catch (e) {
        runtime.resumeFileAppendBytesErr(ctx, suspension, toOtherIoError(e));
        return;
      }

      try {
        const fh = await parent.getFileHandle(name, { create: true });
        await appendBytes(fh, request.bytes);
        runtime.resumeFileAppendBytesOk(ctx, suspension);
      } catch (e) {
        runtime.resumeFileAppendBytesErr(ctx, suspension, toOtherIoError(e));
      }
    },

    "file-truncate": async ({ runtime, ctx, suspension, request }) => {
      const rp = resolvePath(request.path);
      if (rp.tag === "err") return runtime.resumeFileTruncateErr(ctx, suspension, rp.err);

      const of = await openFile(rootDirHandle, rp.segments);
      if (of.tag === "err") return runtime.resumeFileTruncateErr(ctx, suspension, toOtherIoError(of.err));

      try {
        const w = await of.fh.createWritable();
        try {
          await w.truncate(0);
        } finally {
          await w.close();
        }
        runtime.resumeFileTruncateOk(ctx, suspension);
      } catch (e) {
        runtime.resumeFileTruncateErr(ctx, suspension, toOtherIoError(e));
      }
    },

    "file-mkdir": async ({ runtime, ctx, suspension, request }) => {
      const rp = resolvePath(request.path);
      if (rp.tag === "err") return runtime.resumeFileMkdirErr(ctx, suspension, rp.err);

      const parentSegs = rp.segments.slice(0, -1);
      const name = rp.segments[rp.segments.length - 1];

      let parent;
      try {
        parent = await getParentDir(rootDirHandle, parentSegs, false);
      } catch (e) {
        runtime.resumeFileMkdirErr(ctx, suspension, toOtherIoError(e));
        return;
      }

      // If any entry already exists, report AlreadyExists.
      const existingDir = await tryGetDirectoryHandle(parent, name);
      if (existingDir.tag === "ok" || isDomExceptionNamed(existingDir.err, "TypeMismatchError")) {
        runtime.resumeFileMkdirErr(ctx, suspension, { kindCode: KIND.AlreadyExists, msg: "already exists" });
        return;
      }

      try {
        await parent.getDirectoryHandle(name, { create: true });
        runtime.resumeFileMkdirOk(ctx, suspension);
      } catch (e) {
        runtime.resumeFileMkdirErr(ctx, suspension, toOtherIoError(e));
      }
    },

    "file-mkdirs": async ({ runtime, ctx, suspension, request }) => {
      const rp = resolvePath(request.path);
      if (rp.tag === "err") return runtime.resumeFileMkdirsErr(ctx, suspension, rp.err);

      try {
        await getParentDir(rootDirHandle, rp.segments, true);
        runtime.resumeFileMkdirsOk(ctx, suspension);
      } catch (e) {
        if (isDomExceptionNamed(e, "TypeMismatchError")) {
          runtime.resumeFileMkdirsErr(ctx, suspension, { kindCode: KIND.AlreadyExists, msg: "already exists" });
          return;
        }
        runtime.resumeFileMkdirsErr(ctx, suspension, toOtherIoError(e));
      }
    },

    "file-mk-temp-dir": async ({ runtime, ctx, suspension, request }) => {
      const prefix = request.prefix?.length ? request.prefix : "tmp";
      if (prefix.length < 3) {
        runtime.resumeFileMkTempDirErr(ctx, suspension, { kindCode: KIND.InvalidPath, msg: "prefix must be at least 3 characters" });
        return;
      }

      let tmpDir;
      try {
        tmpDir = await rootDirHandle.getDirectoryHandle(tmpBaseName, { create: true });
      } catch (e) {
        runtime.resumeFileMkTempDirErr(ctx, suspension, toOtherIoError(e));
        return;
      }

      const cryptoObj = globalThis.crypto;
      const randomSuffix = () => {
        if (typeof cryptoObj?.randomUUID === "function") return cryptoObj.randomUUID();
        const bytes = new Uint8Array(8);
        cryptoObj?.getRandomValues?.(bytes);
        return Array.from(bytes, (b) => b.toString(16).padStart(2, "0")).join("");
      };

      for (let attempt = 0; attempt < 10; attempt++) {
        const name = `${prefix}-${randomSuffix()}`;
        try {
          await tmpDir.getDirectoryHandle(name, { create: true });
          runtime.resumeFileMkTempDirOk(ctx, suspension, `${tmpBaseName}/${name}`);
          return;
        } catch (e) {
          if (isDomExceptionNamed(e, "InvalidModificationError")) continue;
          runtime.resumeFileMkTempDirErr(ctx, suspension, toOtherIoError(e));
          return;
        }
      }

      runtime.resumeFileMkTempDirErr(ctx, suspension, { kindCode: KIND.Other, msg: "failed to allocate temp directory" });
    },
  };
}

export async function makeOpfsSandbox(prefix = "flix-wasm-sandbox-") {
  if (!hasOpfs()) throw new Error("makeOpfsSandbox: OPFS unavailable");

  const root = await navigator.storage.getDirectory();

  const cryptoObj = globalThis.crypto;
  const suffix =
    typeof cryptoObj?.randomUUID === "function"
      ? cryptoObj.randomUUID()
      : (() => {
          const bytes = new Uint8Array(8);
          cryptoObj?.getRandomValues?.(bytes);
          return Array.from(bytes, (b) => b.toString(16).padStart(2, "0")).join("");
        })();

  const name = `${prefix}${suffix}`;
  const dir = await root.getDirectoryHandle(name, { create: true });

  return {
    rootDirHandle: dir,
    async cleanup() {
      try {
        await root.removeEntry(name, { recursive: true });
      } catch {
        // Best-effort cleanup; ignore.
      }
    },
  };
}

