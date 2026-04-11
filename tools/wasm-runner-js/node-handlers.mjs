import { constants as FS_CONST } from "node:fs";
import * as fs from "node:fs/promises";
import * as os from "node:os";
import * as path from "node:path";

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

function fromNativePathToPosix(p) {
  return p.split(path.sep).join("/");
}

function isPathTraversal(rel) {
  return rel === ".." || rel.startsWith(`..${path.sep}`);
}

function resolveSandboxedPath(rootDir, requestedPath) {
  if (typeof requestedPath !== "string") {
    // Flix primops always pass a string; treat non-string as an invalid path.
    return { tag: "err", err: { kindCode: KIND.InvalidPath, msg: "path must be a string" } };
  }

  if (requestedPath.includes("\0")) {
    return { tag: "err", err: { kindCode: KIND.InvalidPath, msg: "path contains NUL" } };
  }

  const posix = path.posix.normalize(toPosixPath(requestedPath));
  if (posix.length === 0 || posix === ".") {
    return { tag: "err", err: { kindCode: KIND.InvalidPath, msg: "empty path" } };
  }

  if (path.posix.isAbsolute(posix)) {
    return { tag: "err", err: { kindCode: KIND.InvalidPath, msg: "absolute paths are not allowed" } };
  }

  const rootAbs = path.resolve(rootDir);
  const resolvedAbs = path.resolve(rootAbs, ...posix.split("/"));

  const rel = path.relative(rootAbs, resolvedAbs);
  if (isPathTraversal(rel) || path.isAbsolute(rel)) {
    return { tag: "err", err: { kindCode: KIND.InvalidPath, msg: "path escapes sandbox" } };
  }

  return { tag: "ok", path: resolvedAbs };
}

function toOtherIoError(err) {
  if (!err || typeof err !== "object") return { kindCode: KIND.Other, msg: String(err) };
  const msg = typeof err.message === "string" ? err.message : String(err);
  return { kindCode: KIND.Other, msg };
}

function toAlreadyExistsOrOtherIoError(err) {
  const code = typeof err?.code === "string" ? err.code : "";
  if (code === "EEXIST") {
    const msg = typeof err.message === "string" ? err.message : String(err);
    return { kindCode: KIND.AlreadyExists, msg };
  }
  return toOtherIoError(err);
}

async function safeStat(p) {
  try {
    return { tag: "ok", st: await fs.stat(p) };
  } catch (e) {
    return { tag: "err", err: e };
  }
}

/**
 * Returns a set of Node.js-backed handlers for the `file-*` ops in `flix:runtime@0.1.0`.
 *
 * By default, all paths are resolved relative to `rootDir` using POSIX-style separators (`/`),
 * and path traversal is rejected.
 */
export function makeNodeFsHandlers(options = {}) {
  const rootDir = options.rootDir;
  if (typeof rootDir !== "string" || rootDir.length === 0) {
    throw new Error("makeNodeFsHandlers: options.rootDir must be a non-empty string");
  }

  const tmpBaseDir = options.tmpBaseDir ?? path.join(rootDir, "tmp");

  const resolvePath = (requestedPath) => resolveSandboxedPath(rootDir, requestedPath);

  return {
    "file-exists": async ({ runtime, ctx, suspension, request }) => {
      const rp = resolvePath(request.path);
      if (rp.tag === "err") return runtime.resumeFileExistsErr(ctx, suspension, rp.err);

      const st = await safeStat(rp.path);
      if (st.tag === "ok") return runtime.resumeFileExistsOk(ctx, suspension, true);
      // Preserve JVM semantics: any I/O failure yields `false` (not an error).
      return runtime.resumeFileExistsOk(ctx, suspension, false);
    },

    "file-is-directory": async ({ runtime, ctx, suspension, request }) => {
      const rp = resolvePath(request.path);
      if (rp.tag === "err") return runtime.resumeFileIsDirectoryErr(ctx, suspension, rp.err);

      const st = await safeStat(rp.path);
      if (st.tag === "ok") return runtime.resumeFileIsDirectoryOk(ctx, suspension, st.st.isDirectory());
      return runtime.resumeFileIsDirectoryOk(ctx, suspension, false);
    },

    "file-is-regular-file": async ({ runtime, ctx, suspension, request }) => {
      const rp = resolvePath(request.path);
      if (rp.tag === "err") return runtime.resumeFileIsRegularFileErr(ctx, suspension, rp.err);

      const st = await safeStat(rp.path);
      if (st.tag === "ok") return runtime.resumeFileIsRegularFileOk(ctx, suspension, st.st.isFile());
      return runtime.resumeFileIsRegularFileOk(ctx, suspension, false);
    },

    "file-is-readable": async ({ runtime, ctx, suspension, request }) => {
      const rp = resolvePath(request.path);
      if (rp.tag === "err") return runtime.resumeFileIsReadableErr(ctx, suspension, rp.err);

      try {
        await fs.access(rp.path, FS_CONST.R_OK);
        return runtime.resumeFileIsReadableOk(ctx, suspension, true);
      } catch (_e) {
        return runtime.resumeFileIsReadableOk(ctx, suspension, false);
      }
    },

    "file-is-writable": async ({ runtime, ctx, suspension, request }) => {
      const rp = resolvePath(request.path);
      if (rp.tag === "err") return runtime.resumeFileIsWritableErr(ctx, suspension, rp.err);

      try {
        await fs.access(rp.path, FS_CONST.W_OK);
        return runtime.resumeFileIsWritableOk(ctx, suspension, true);
      } catch (_e) {
        return runtime.resumeFileIsWritableOk(ctx, suspension, false);
      }
    },

    "file-is-executable": async ({ runtime, ctx, suspension, request }) => {
      const rp = resolvePath(request.path);
      if (rp.tag === "err") return runtime.resumeFileIsExecutableErr(ctx, suspension, rp.err);

      // Best-effort: `X_OK` is POSIX-y; on Windows it doesn't have a meaningful execute bit.
      // We still implement it to keep the API shape consistent.
      try {
        await fs.access(rp.path, FS_CONST.X_OK);
        return runtime.resumeFileIsExecutableOk(ctx, suspension, true);
      } catch (_e) {
        return runtime.resumeFileIsExecutableOk(ctx, suspension, false);
      }
    },

    "file-is-symbolic-link": async ({ runtime, ctx, suspension, request }) => {
      const rp = resolvePath(request.path);
      if (rp.tag === "err") return runtime.resumeFileIsSymbolicLinkErr(ctx, suspension, rp.err);

      try {
        const st = await fs.lstat(rp.path);
        return runtime.resumeFileIsSymbolicLinkOk(ctx, suspension, st.isSymbolicLink());
      } catch (_e) {
        return runtime.resumeFileIsSymbolicLinkOk(ctx, suspension, false);
      }
    },

    "file-access-time": async ({ runtime, ctx, suspension, request }) => {
      const rp = resolvePath(request.path);
      if (rp.tag === "err") return runtime.resumeFileAccessTimeErr(ctx, suspension, rp.err);

      const st = await safeStat(rp.path);
      if (st.tag === "ok") return runtime.resumeFileAccessTimeOk(ctx, suspension, BigInt(Math.trunc(st.st.atimeMs)));
      return runtime.resumeFileAccessTimeErr(ctx, suspension, toOtherIoError(st.err));
    },

    "file-creation-time": async ({ runtime, ctx, suspension, request }) => {
      const rp = resolvePath(request.path);
      if (rp.tag === "err") return runtime.resumeFileCreationTimeErr(ctx, suspension, rp.err);

      const st = await safeStat(rp.path);
      // Node doesn't reliably expose true creation time cross-platform; `birthtimeMs` is best-effort.
      if (st.tag === "ok") {
        return runtime.resumeFileCreationTimeOk(ctx, suspension, BigInt(Math.trunc(st.st.birthtimeMs)));
      }
      return runtime.resumeFileCreationTimeErr(ctx, suspension, toOtherIoError(st.err));
    },

    "file-modification-time": async ({ runtime, ctx, suspension, request }) => {
      const rp = resolvePath(request.path);
      if (rp.tag === "err") return runtime.resumeFileModificationTimeErr(ctx, suspension, rp.err);

      const st = await safeStat(rp.path);
      if (st.tag === "ok") return runtime.resumeFileModificationTimeOk(ctx, suspension, BigInt(Math.trunc(st.st.mtimeMs)));
      return runtime.resumeFileModificationTimeErr(ctx, suspension, toOtherIoError(st.err));
    },

    "file-size": async ({ runtime, ctx, suspension, request }) => {
      const rp = resolvePath(request.path);
      if (rp.tag === "err") return runtime.resumeFileSizeErr(ctx, suspension, rp.err);

      const st = await safeStat(rp.path);
      if (st.tag === "ok") return runtime.resumeFileSizeOk(ctx, suspension, BigInt(st.st.size));
      return runtime.resumeFileSizeErr(ctx, suspension, toOtherIoError(st.err));
    },

    "file-read": async ({ runtime, ctx, suspension, request }) => {
      const rp = resolvePath(request.path);
      if (rp.tag === "err") return runtime.resumeFileReadErr(ctx, suspension, rp.err);

      try {
        const data = await fs.readFile(rp.path, { encoding: "utf8" });
        runtime.resumeFileReadOk(ctx, suspension, data);
      } catch (e) {
        runtime.resumeFileReadErr(ctx, suspension, toOtherIoError(e));
      }
    },

    "file-read-lines": async ({ runtime, ctx, suspension, request }) => {
      const rp = resolvePath(request.path);
      if (rp.tag === "err") return runtime.resumeFileReadLinesErr(ctx, suspension, rp.err);

      try {
        const data = await fs.readFile(rp.path, { encoding: "utf8" });
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

      try {
        const buf = await fs.readFile(rp.path);
        runtime.resumeFileReadBytesOk(ctx, suspension, new Uint8Array(buf.buffer, buf.byteOffset, buf.byteLength));
      } catch (e) {
        runtime.resumeFileReadBytesErr(ctx, suspension, toOtherIoError(e));
      }
    },

    "file-list": async ({ runtime, ctx, suspension, request }) => {
      const rp = resolvePath(request.path);
      if (rp.tag === "err") return runtime.resumeFileListErr(ctx, suspension, rp.err);

      try {
        const st = await fs.stat(rp.path);
        if (!st.isDirectory()) {
          runtime.resumeFileListErr(ctx, suspension, { kindCode: KIND.NotDirectory, msg: "not a directory" });
          return;
        }
      } catch (_e) {
        // Preserve JVM semantics: any failure in the directory predicate yields NotDirectory.
        runtime.resumeFileListErr(ctx, suspension, { kindCode: KIND.NotDirectory, msg: "not a directory" });
        return;
      }

      try {
        const names = await fs.readdir(rp.path);
        names.sort((a, b) => a.localeCompare(b));
        runtime.resumeFileListOk(ctx, suspension, names);
      } catch (_e) {
        runtime.resumeFileListErr(ctx, suspension, { kindCode: KIND.Other, msg: "I/O error" });
      }
    },

    "file-write": async ({ runtime, ctx, suspension, request }) => {
      const rp = resolvePath(request.path);
      if (rp.tag === "err") return runtime.resumeFileWriteErr(ctx, suspension, rp.err);

      try {
        await fs.writeFile(rp.path, request.data, { encoding: "utf8" });
        runtime.resumeFileWriteOk(ctx, suspension);
      } catch (e) {
        runtime.resumeFileWriteErr(ctx, suspension, toOtherIoError(e));
      }
    },

    "file-write-bytes": async ({ runtime, ctx, suspension, request }) => {
      const rp = resolvePath(request.path);
      if (rp.tag === "err") return runtime.resumeFileWriteBytesErr(ctx, suspension, rp.err);

      try {
        await fs.writeFile(rp.path, request.bytes);
        runtime.resumeFileWriteBytesOk(ctx, suspension);
      } catch (e) {
        runtime.resumeFileWriteBytesErr(ctx, suspension, toOtherIoError(e));
      }
    },

    "file-append": async ({ runtime, ctx, suspension, request }) => {
      const rp = resolvePath(request.path);
      if (rp.tag === "err") return runtime.resumeFileAppendErr(ctx, suspension, rp.err);

      try {
        await fs.appendFile(rp.path, request.data, { encoding: "utf8" });
        runtime.resumeFileAppendOk(ctx, suspension);
      } catch (e) {
        runtime.resumeFileAppendErr(ctx, suspension, toOtherIoError(e));
      }
    },

    "file-append-bytes": async ({ runtime, ctx, suspension, request }) => {
      const rp = resolvePath(request.path);
      if (rp.tag === "err") return runtime.resumeFileAppendBytesErr(ctx, suspension, rp.err);

      try {
        await fs.appendFile(rp.path, request.bytes);
        runtime.resumeFileAppendBytesOk(ctx, suspension);
      } catch (e) {
        runtime.resumeFileAppendBytesErr(ctx, suspension, toOtherIoError(e));
      }
    },

    "file-truncate": async ({ runtime, ctx, suspension, request }) => {
      const rp = resolvePath(request.path);
      if (rp.tag === "err") return runtime.resumeFileTruncateErr(ctx, suspension, rp.err);

      try {
        await fs.truncate(rp.path, 0);
        runtime.resumeFileTruncateOk(ctx, suspension);
      } catch (e) {
        runtime.resumeFileTruncateErr(ctx, suspension, toOtherIoError(e));
      }
    },

    "file-mkdir": async ({ runtime, ctx, suspension, request }) => {
      const rp = resolvePath(request.path);
      if (rp.tag === "err") return runtime.resumeFileMkdirErr(ctx, suspension, rp.err);

      try {
        await fs.mkdir(rp.path, { recursive: false });
        runtime.resumeFileMkdirOk(ctx, suspension);
      } catch (e) {
        runtime.resumeFileMkdirErr(ctx, suspension, toAlreadyExistsOrOtherIoError(e));
      }
    },

    "file-mkdirs": async ({ runtime, ctx, suspension, request }) => {
      const rp = resolvePath(request.path);
      if (rp.tag === "err") return runtime.resumeFileMkdirsErr(ctx, suspension, rp.err);

      try {
        await fs.mkdir(rp.path, { recursive: true });
        runtime.resumeFileMkdirsOk(ctx, suspension);
      } catch (e) {
        runtime.resumeFileMkdirsErr(ctx, suspension, toAlreadyExistsOrOtherIoError(e));
      }
    },

    "file-mk-temp-dir": async ({ runtime, ctx, suspension, request }) => {
      try {
        const prefix = request.prefix?.length ? request.prefix : "tmp";
        if (prefix.length < 3) {
          runtime.resumeFileMkTempDirErr(ctx, suspension, {
            kindCode: KIND.InvalidPath,
            msg: "prefix must be at least 3 characters",
          });
          return;
        }

        await fs.mkdir(tmpBaseDir, { recursive: true });
        const dir = await fs.mkdtemp(path.join(tmpBaseDir, `${prefix}-`));
        const rel = path.relative(path.resolve(rootDir), path.resolve(dir));
        runtime.resumeFileMkTempDirOk(ctx, suspension, fromNativePathToPosix(rel));
      } catch (e) {
        runtime.resumeFileMkTempDirErr(ctx, suspension, toOtherIoError(e));
      }
    },
  };
}

/**
 * Convenience helper for tests/examples.
 */
export async function makeTempSandbox(prefix = "flix-wasm-sandbox-") {
  const dir = await fs.mkdtemp(path.join(os.tmpdir(), prefix));
  return dir;
}
