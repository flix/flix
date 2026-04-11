use anyhow::{Context, Result};
use std::collections::{HashMap, VecDeque};
use std::ffi::CString;
use std::fs::{self, OpenOptions};
use std::io::{Read, Write};
use std::net::{IpAddr, Ipv4Addr, Ipv6Addr, SocketAddr, TcpListener, TcpStream};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};
use wasmtime::Store;

use crate::bindings::exports::flix::runtime::runtime::{
    Guest, HttpHeader, HttpResponse, IoError, OpRequest, Suspension,
};
use crate::runner::{OpHandler, PendingIo};

const KIND_ALREADY_EXISTS: i32 = 0;
const KIND_INVALID_PATH: i32 = 3;
const KIND_INVALID_INPUT: i32 = 4;
const KIND_NOT_DIRECTORY: i32 = 8;
const KIND_TIMEOUT: i32 = 10;
const KIND_OTHER: i32 = 14;

fn io_error(kind_code: i32, msg: impl Into<String>) -> IoError {
    IoError {
        kind_code,
        msg: msg.into(),
    }
}

fn to_other_io_error(e: impl std::fmt::Display) -> IoError {
    io_error(KIND_OTHER, e.to_string())
}

fn process_io_error_from(e: std::io::Error) -> IoError {
    match e.kind() {
        std::io::ErrorKind::InvalidInput => io_error(KIND_INVALID_INPUT, e.to_string()),
        _ => to_other_io_error(e),
    }
}

fn tcp_io_error_from(e: std::io::Error) -> IoError {
    match e.kind() {
        std::io::ErrorKind::TimedOut => io_error(KIND_TIMEOUT, e.to_string()),
        _ => to_other_io_error(e),
    }
}

fn invalid_process_handle() -> IoError {
    io_error(KIND_OTHER, "invalid process handle.")
}

fn clamp_ms(ms: i64) -> u64 {
    if ms <= 0 {
        return 0;
    }
    // Align with the Node runner: clamp to i32::MAX milliseconds.
    let max = i32::MAX as i64;
    ms.min(max) as u64
}

fn to_epoch_ms(t: SystemTime) -> i64 {
    match t.duration_since(UNIX_EPOCH) {
        Ok(d) => d.as_millis().min(i64::MAX as u128) as i64,
        Err(_) => 0,
    }
}

fn normalize_rel_posix_path(requested_path: &str) -> Result<Vec<String>, IoError> {
    if requested_path.contains('\0') {
        return Err(io_error(KIND_INVALID_PATH, "path contains NUL"));
    }

    let p = requested_path.replace('\\', "/");
    if p.is_empty() || p == "." {
        return Err(io_error(KIND_INVALID_PATH, "empty path"));
    }

    if p.starts_with('/') {
        return Err(io_error(
            KIND_INVALID_PATH,
            "absolute paths are not allowed",
        ));
    }

    let mut out = Vec::new();
    for part in p.split('/') {
        if part.is_empty() || part == "." {
            continue;
        }
        if part == ".." {
            if out.is_empty() {
                return Err(io_error(KIND_INVALID_PATH, "path escapes sandbox"));
            }
            out.pop();
            continue;
        }
        out.push(part.to_string());
    }

    if out.is_empty() {
        return Err(io_error(KIND_INVALID_PATH, "empty path"));
    }

    Ok(out)
}

fn resolve_sandboxed_path(root_dir: &Path, requested_path: &str) -> Result<PathBuf, IoError> {
    let parts = normalize_rel_posix_path(requested_path)?;
    let mut pb = PathBuf::from(root_dir);
    for p in parts {
        pb.push(p);
    }
    Ok(pb)
}

fn rel_posix_path(root_dir: &Path, abs: &Path) -> Result<String> {
    let rel = abs
        .strip_prefix(root_dir)
        .map_err(|_| anyhow::anyhow!("path escapes sandbox"))?;

    let mut out = String::new();
    for (i, c) in rel.components().enumerate() {
        if i > 0 {
            out.push('/');
        }
        out.push_str(&c.as_os_str().to_string_lossy());
    }
    Ok(out)
}

fn split_lines_portable(s: &str) -> Vec<String> {
    let bytes = s.as_bytes();
    let mut lines = Vec::new();
    let mut start = 0usize;
    let mut i = 0usize;

    while i < bytes.len() {
        match bytes[i] {
            b'\n' => {
                lines.push(s[start..i].to_string());
                i += 1;
                start = i;
            }
            b'\r' => {
                lines.push(s[start..i].to_string());
                i += 1;
                if i < bytes.len() && bytes[i] == b'\n' {
                    i += 1;
                }
                start = i;
            }
            _ => i += 1,
        }
    }

    if start < bytes.len() {
        lines.push(s[start..].to_string());
    }

    lines
}

#[cfg(unix)]
const ACCESS_READ: i32 = libc::R_OK;
#[cfg(not(unix))]
const ACCESS_READ: i32 = 4;

#[cfg(unix)]
const ACCESS_WRITE: i32 = libc::W_OK;
#[cfg(not(unix))]
const ACCESS_WRITE: i32 = 2;

#[cfg(unix)]
const ACCESS_EXEC: i32 = libc::X_OK;
#[cfg(not(unix))]
const ACCESS_EXEC: i32 = 1;

#[cfg(unix)]
fn access_ok(path: &Path, mode: i32) -> bool {
    use std::os::unix::ffi::OsStrExt;
    let Ok(cstr) = CString::new(path.as_os_str().as_bytes()) else {
        return false;
    };
    unsafe { libc::access(cstr.as_ptr(), mode) == 0 }
}

#[cfg(not(unix))]
fn access_ok(_path: &Path, _mode: i32) -> bool {
    // Best-effort fallback on non-Unix platforms.
    false
}

fn parse_ip(bytes: &[u8]) -> Result<IpAddr, IoError> {
    match bytes.len() {
        4 => Ok(IpAddr::V4(Ipv4Addr::new(bytes[0], bytes[1], bytes[2], bytes[3]))),
        16 => {
            let mut b = [0u8; 16];
            b.copy_from_slice(bytes);
            Ok(IpAddr::V6(Ipv6Addr::from(b)))
        }
        _ => Err(io_error(KIND_INVALID_INPUT, "invalid ip bytes")),
    }
}

struct TcpState {
    next_socket_id: u64,
    next_server_id: u64,
    sockets: HashMap<u64, TcpStream>,
    servers: HashMap<u64, TcpListener>,
}

impl TcpState {
    fn new() -> Self {
        Self {
            next_socket_id: 9000,
            next_server_id: 1000,
            sockets: HashMap::new(),
            servers: HashMap::new(),
        }
    }

    fn alloc_socket_id(&mut self) -> u64 {
        let id = self.next_socket_id;
        self.next_socket_id += 1;
        id
    }

    fn alloc_server_id(&mut self) -> u64 {
        let id = self.next_server_id;
        self.next_server_id += 1;
        id
    }
}

struct ProcessState {
    child: Child,
    exited: bool,
    exit_value: i32,
}

impl ProcessState {
    fn new(child: Child) -> Self {
        Self {
            child,
            exited: false,
            exit_value: 0,
        }
    }

    fn refresh_exit_status(&mut self) -> Result<(), std::io::Error> {
        if self.exited {
            return Ok(());
        }

        let Some(status) = self.child.try_wait()? else {
            return Ok(());
        };

        self.exited = true;
        self.exit_value = normalize_exit_code(status);
        Ok(())
    }
}

fn normalize_exit_code(status: std::process::ExitStatus) -> i32 {
    if let Some(code) = status.code() {
        return code;
    }

    #[cfg(unix)]
    {
        use std::os::unix::process::ExitStatusExt;
        if let Some(sig) = status.signal() {
            return 128 + sig;
        }
    }

    128
}

pub struct StdHostHandlers {
    fs_root: PathBuf,
    fs_tmp_base: PathBuf,
    stdin_lines: VecDeque<String>,
    next_process_id: u64,
    processes: HashMap<u64, ProcessState>,
    tcp: TcpState,
    tcp_connect_timeout: Duration,
    tcp_io_timeout: Duration,
}

impl StdHostHandlers {
    pub fn new(fs_root: PathBuf) -> Self {
        let fs_tmp_base = fs_root.join("tmp");
        Self {
            fs_root,
            fs_tmp_base,
            stdin_lines: VecDeque::new(),
            next_process_id: 9000,
            processes: HashMap::new(),
            tcp: TcpState::new(),
            tcp_connect_timeout: Duration::from_millis(2000),
            tcp_io_timeout: Duration::from_millis(2000),
        }
    }

    pub fn push_stdin_line(&mut self, line: String) {
        self.stdin_lines.push_back(line);
    }

    fn file_path(&self, requested: &str) -> Result<PathBuf, IoError> {
        resolve_sandboxed_path(&self.fs_root, requested)
    }

    fn write_all_or_err(path: &Path, bytes: &[u8]) -> Result<()> {
        fs::write(path, bytes).with_context(|| format!("write failed: {}", path.display()))
    }

    fn read_all_or_err(path: &Path) -> Result<Vec<u8>> {
        fs::read(path).with_context(|| format!("read failed: {}", path.display()))
    }
}

impl<T> OpHandler<T> for StdHostHandlers {
    fn handle_op(
        &mut self,
        store: &mut Store<T>,
        rt: &Guest,
        ctx: crate::bindings::exports::flix::runtime::runtime::Ctx,
        suspension: Suspension,
        req: OpRequest,
    ) -> Result<()> {
        match req {
            OpRequest::HttpRequest(r) => {
                // Deterministic harness behavior (mirrors JS runner smoke).
                if r.method != "GET" || r.url != "https://example.com/hello" {
                    rt.call_resume_http_err(
                        store,
                        ctx,
                        suspension,
                        &io_error(KIND_OTHER, "unexpected request"),
                    )?;
                    return Ok(());
                }

                rt.call_resume_http_ok(
                    store,
                    ctx,
                    suspension,
                    &HttpResponse {
                        status: 200,
                        headers: vec![HttpHeader {
                            name: "x-foo".to_string(),
                            value: "bar".to_string(),
                        }],
                        body: "hello".to_string(),
                    },
                )?;
                Ok(())
            }

            OpRequest::ConsoleReadln => {
                let mut line = self.stdin_lines.pop_front().unwrap_or_default();
                if line.ends_with('\n') {
                    line.pop();
                }
                if line.ends_with('\r') {
                    line.pop();
                }
                rt.call_resume_console_readln_ok(store, ctx, suspension, &line)?;
                Ok(())
            }

            // --- filesystem ---
            OpRequest::FileExists(r) => {
                let rp = match self.file_path(&r.path) {
                    Ok(p) => p,
                    Err(e) => {
                        rt.call_resume_file_exists_err(store, ctx, suspension, &e)?;
                        return Ok(());
                    }
                };

                let exists = fs::metadata(&rp).is_ok();
                rt.call_resume_file_exists_ok(store, ctx, suspension, exists)?;
                Ok(())
            }
            OpRequest::FileIsDirectory(r) => {
                let rp = match self.file_path(&r.path) {
                    Ok(p) => p,
                    Err(e) => {
                        rt.call_resume_file_is_directory_err(store, ctx, suspension, &e)?;
                        return Ok(());
                    }
                };

                let is_dir = fs::metadata(&rp).map(|m| m.is_dir()).unwrap_or(false);
                rt.call_resume_file_is_directory_ok(store, ctx, suspension, is_dir)?;
                Ok(())
            }
            OpRequest::FileIsRegularFile(r) => {
                let rp = match self.file_path(&r.path) {
                    Ok(p) => p,
                    Err(e) => {
                        rt.call_resume_file_is_regular_file_err(store, ctx, suspension, &e)?;
                        return Ok(());
                    }
                };

                let is_file = fs::metadata(&rp).map(|m| m.is_file()).unwrap_or(false);
                rt.call_resume_file_is_regular_file_ok(store, ctx, suspension, is_file)?;
                Ok(())
            }
            OpRequest::FileIsReadable(r) => {
                let rp = match self.file_path(&r.path) {
                    Ok(p) => p,
                    Err(e) => {
                        rt.call_resume_file_is_readable_err(store, ctx, suspension, &e)?;
                        return Ok(());
                    }
                };

                let ok = if access_ok(&rp, ACCESS_READ) {
                    true
                } else {
                    fs::File::open(&rp).is_ok()
                };
                rt.call_resume_file_is_readable_ok(store, ctx, suspension, ok)?;
                Ok(())
            }
            OpRequest::FileIsSymbolicLink(r) => {
                let rp = match self.file_path(&r.path) {
                    Ok(p) => p,
                    Err(e) => {
                        rt.call_resume_file_is_symbolic_link_err(store, ctx, suspension, &e)?;
                        return Ok(());
                    }
                };

                let is_link = fs::symlink_metadata(&rp)
                    .map(|m| m.file_type().is_symlink())
                    .unwrap_or(false);
                rt.call_resume_file_is_symbolic_link_ok(store, ctx, suspension, is_link)?;
                Ok(())
            }
            OpRequest::FileIsWritable(r) => {
                let rp = match self.file_path(&r.path) {
                    Ok(p) => p,
                    Err(e) => {
                        rt.call_resume_file_is_writable_err(store, ctx, suspension, &e)?;
                        return Ok(());
                    }
                };

                let ok = if access_ok(&rp, ACCESS_WRITE) {
                    true
                } else if fs::metadata(&rp).map(|m| m.is_dir()).unwrap_or(false) {
                    false
                } else {
                    OpenOptions::new().write(true).open(&rp).is_ok()
                };
                rt.call_resume_file_is_writable_ok(store, ctx, suspension, ok)?;
                Ok(())
            }
            OpRequest::FileIsExecutable(r) => {
                let rp = match self.file_path(&r.path) {
                    Ok(p) => p,
                    Err(e) => {
                        rt.call_resume_file_is_executable_err(store, ctx, suspension, &e)?;
                        return Ok(());
                    }
                };

                let ok = access_ok(&rp, ACCESS_EXEC);
                rt.call_resume_file_is_executable_ok(store, ctx, suspension, ok)?;
                Ok(())
            }
            OpRequest::FileAccessTime(r) => {
                let rp = match self.file_path(&r.path) {
                    Ok(p) => p,
                    Err(e) => {
                        rt.call_resume_file_access_time_err(store, ctx, suspension, &e)?;
                        return Ok(());
                    }
                };

                match fs::metadata(&rp)
                    .and_then(|m| m.accessed().map(|t| to_epoch_ms(t)))
                {
                    Ok(ms) => rt.call_resume_file_access_time_ok(store, ctx, suspension, ms)?,
                    Err(e) => rt.call_resume_file_access_time_err(store, ctx, suspension, &to_other_io_error(e))?,
                }
                Ok(())
            }
            OpRequest::FileCreationTime(r) => {
                let rp = match self.file_path(&r.path) {
                    Ok(p) => p,
                    Err(e) => {
                        rt.call_resume_file_creation_time_err(store, ctx, suspension, &e)?;
                        return Ok(());
                    }
                };

                match fs::metadata(&rp)
                    .and_then(|m| m.created().map(|t| to_epoch_ms(t)))
                {
                    Ok(ms) => rt.call_resume_file_creation_time_ok(store, ctx, suspension, ms)?,
                    Err(e) => rt.call_resume_file_creation_time_err(store, ctx, suspension, &to_other_io_error(e))?,
                }
                Ok(())
            }
            OpRequest::FileModificationTime(r) => {
                let rp = match self.file_path(&r.path) {
                    Ok(p) => p,
                    Err(e) => {
                        rt.call_resume_file_modification_time_err(store, ctx, suspension, &e)?;
                        return Ok(());
                    }
                };

                match fs::metadata(&rp)
                    .and_then(|m| m.modified().map(|t| to_epoch_ms(t)))
                {
                    Ok(ms) => rt.call_resume_file_modification_time_ok(store, ctx, suspension, ms)?,
                    Err(e) => rt.call_resume_file_modification_time_err(store, ctx, suspension, &to_other_io_error(e))?,
                }
                Ok(())
            }
            OpRequest::FileSize(r) => {
                let rp = match self.file_path(&r.path) {
                    Ok(p) => p,
                    Err(e) => {
                        rt.call_resume_file_size_err(store, ctx, suspension, &e)?;
                        return Ok(());
                    }
                };

                match fs::metadata(&rp) {
                    Ok(m) => rt.call_resume_file_size_ok(store, ctx, suspension, m.len() as i64)?,
                    Err(e) => rt.call_resume_file_size_err(store, ctx, suspension, &to_other_io_error(e))?,
                }
                Ok(())
            }
            OpRequest::FileRead(r) => {
                let rp = match self.file_path(&r.path) {
                    Ok(p) => p,
                    Err(e) => {
                        rt.call_resume_file_read_err(store, ctx, suspension, &e)?;
                        return Ok(());
                    }
                };

                match Self::read_all_or_err(&rp) {
                    Ok(bytes) => {
                        let s = String::from_utf8_lossy(&bytes).to_string();
                        rt.call_resume_file_read_ok(store, ctx, suspension, &s)?;
                    }
                    Err(e) => rt.call_resume_file_read_err(store, ctx, suspension, &to_other_io_error(e))?,
                }
                Ok(())
            }
            OpRequest::FileReadLines(r) => {
                let rp = match self.file_path(&r.path) {
                    Ok(p) => p,
                    Err(e) => {
                        rt.call_resume_file_read_lines_err(store, ctx, suspension, &e)?;
                        return Ok(());
                    }
                };

                match Self::read_all_or_err(&rp) {
                    Ok(bytes) => {
                        let s = String::from_utf8_lossy(&bytes).to_string();
                        let lines = split_lines_portable(&s);
                        rt.call_resume_file_read_lines_ok(store, ctx, suspension, &lines)?;
                    }
                    Err(e) => rt.call_resume_file_read_lines_err(store, ctx, suspension, &to_other_io_error(e))?,
                }
                Ok(())
            }
            OpRequest::FileReadBytes(r) => {
                let rp = match self.file_path(&r.path) {
                    Ok(p) => p,
                    Err(e) => {
                        rt.call_resume_file_read_bytes_err(store, ctx, suspension, &e)?;
                        return Ok(());
                    }
                };

                match Self::read_all_or_err(&rp) {
                    Ok(bytes) => rt.call_resume_file_read_bytes_ok(store, ctx, suspension, &bytes)?,
                    Err(e) => rt.call_resume_file_read_bytes_err(store, ctx, suspension, &to_other_io_error(e))?,
                }
                Ok(())
            }
            OpRequest::FileList(r) => {
                let rp = match self.file_path(&r.path) {
                    Ok(p) => p,
                    Err(e) => {
                        rt.call_resume_file_list_err(store, ctx, suspension, &e)?;
                        return Ok(());
                    }
                };

                match fs::metadata(&rp) {
                    Ok(m) if m.is_dir() => {}
                    _ => {
                        rt.call_resume_file_list_err(
                            store,
                            ctx,
                            suspension,
                            &io_error(KIND_NOT_DIRECTORY, "not a directory"),
                        )?;
                        return Ok(());
                    }
                }

                let mut names = Vec::new();
                match fs::read_dir(&rp) {
                    Ok(rd) => {
                        for ent in rd.flatten() {
                            names.push(ent.file_name().to_string_lossy().to_string());
                        }
                        names.sort();
                        rt.call_resume_file_list_ok(store, ctx, suspension, &names)?;
                    }
                    Err(_e) => {
                        rt.call_resume_file_list_err(
                            store,
                            ctx,
                            suspension,
                            &io_error(KIND_OTHER, "I/O error"),
                        )?;
                    }
                }
                Ok(())
            }
            OpRequest::FileWrite(r) => {
                let rp = match self.file_path(&r.path) {
                    Ok(p) => p,
                    Err(e) => {
                        rt.call_resume_file_write_err(store, ctx, suspension, &e)?;
                        return Ok(());
                    }
                };

                if let Err(e) = Self::write_all_or_err(&rp, r.data.as_bytes()) {
                    rt.call_resume_file_write_err(store, ctx, suspension, &to_other_io_error(e))?;
                } else {
                    rt.call_resume_file_write_ok(store, ctx, suspension)?;
                }
                Ok(())
            }
            OpRequest::FileWriteBytes(r) => {
                let rp = match self.file_path(&r.path) {
                    Ok(p) => p,
                    Err(e) => {
                        rt.call_resume_file_write_bytes_err(store, ctx, suspension, &e)?;
                        return Ok(());
                    }
                };

                if let Err(e) = Self::write_all_or_err(&rp, &r.bytes) {
                    rt.call_resume_file_write_bytes_err(store, ctx, suspension, &to_other_io_error(e))?;
                } else {
                    rt.call_resume_file_write_bytes_ok(store, ctx, suspension)?;
                }
                Ok(())
            }
            OpRequest::FileAppend(r) => {
                let rp = match self.file_path(&r.path) {
                    Ok(p) => p,
                    Err(e) => {
                        rt.call_resume_file_append_err(store, ctx, suspension, &e)?;
                        return Ok(());
                    }
                };

                let result = OpenOptions::new()
                    .create(true)
                    .append(true)
                    .open(&rp)
                    .and_then(|mut f| f.write_all(r.data.as_bytes()));
                match result {
                    Ok(()) => rt.call_resume_file_append_ok(store, ctx, suspension)?,
                    Err(e) => rt.call_resume_file_append_err(store, ctx, suspension, &to_other_io_error(e))?,
                }
                Ok(())
            }
            OpRequest::FileAppendBytes(r) => {
                let rp = match self.file_path(&r.path) {
                    Ok(p) => p,
                    Err(e) => {
                        rt.call_resume_file_append_bytes_err(store, ctx, suspension, &e)?;
                        return Ok(());
                    }
                };

                let result = OpenOptions::new()
                    .create(true)
                    .append(true)
                    .open(&rp)
                    .and_then(|mut f| f.write_all(&r.bytes));
                match result {
                    Ok(()) => rt.call_resume_file_append_bytes_ok(store, ctx, suspension)?,
                    Err(e) => {
                        rt.call_resume_file_append_bytes_err(store, ctx, suspension, &to_other_io_error(e))?
                    }
                }
                Ok(())
            }
            OpRequest::FileTruncate(r) => {
                let rp = match self.file_path(&r.path) {
                    Ok(p) => p,
                    Err(e) => {
                        rt.call_resume_file_truncate_err(store, ctx, suspension, &e)?;
                        return Ok(());
                    }
                };

                let result = OpenOptions::new().write(true).truncate(true).open(&rp);
                match result {
                    Ok(_) => rt.call_resume_file_truncate_ok(store, ctx, suspension)?,
                    Err(e) => rt.call_resume_file_truncate_err(store, ctx, suspension, &to_other_io_error(e))?,
                }
                Ok(())
            }
            OpRequest::FileMkdir(r) => {
                let rp = match self.file_path(&r.path) {
                    Ok(p) => p,
                    Err(e) => {
                        rt.call_resume_file_mkdir_err(store, ctx, suspension, &e)?;
                        return Ok(());
                    }
                };

                match fs::create_dir(&rp) {
                    Ok(()) => rt.call_resume_file_mkdir_ok(store, ctx, suspension)?,
                    Err(e) if e.kind() == std::io::ErrorKind::AlreadyExists => {
                        rt.call_resume_file_mkdir_err(
                            store,
                            ctx,
                            suspension,
                            &io_error(KIND_ALREADY_EXISTS, e.to_string()),
                        )?
                    }
                    Err(e) => rt.call_resume_file_mkdir_err(store, ctx, suspension, &to_other_io_error(e))?,
                }
                Ok(())
            }
            OpRequest::FileMkdirs(r) => {
                let rp = match self.file_path(&r.path) {
                    Ok(p) => p,
                    Err(e) => {
                        rt.call_resume_file_mkdirs_err(store, ctx, suspension, &e)?;
                        return Ok(());
                    }
                };

                match fs::create_dir_all(&rp) {
                    Ok(()) => rt.call_resume_file_mkdirs_ok(store, ctx, suspension)?,
                    Err(e) if e.kind() == std::io::ErrorKind::AlreadyExists => {
                        rt.call_resume_file_mkdirs_err(
                            store,
                            ctx,
                            suspension,
                            &io_error(KIND_ALREADY_EXISTS, e.to_string()),
                        )?
                    }
                    Err(e) => rt.call_resume_file_mkdirs_err(store, ctx, suspension, &to_other_io_error(e))?,
                }
                Ok(())
            }
            OpRequest::FileMkTempDir(r) => {
                let prefix = if r.prefix.is_empty() { "tmp" } else { r.prefix.as_str() };
                if prefix.len() < 3 {
                    rt.call_resume_file_mk_temp_dir_err(
                        store,
                        ctx,
                        suspension,
                        &io_error(KIND_INVALID_PATH, "prefix must be at least 3 characters"),
                    )?;
                    return Ok(());
                }

                if let Err(e) = fs::create_dir_all(&self.fs_tmp_base) {
                    rt.call_resume_file_mk_temp_dir_err(
                        store,
                        ctx,
                        suspension,
                        &to_other_io_error(anyhow::anyhow!(e).context(format!(
                            "mkdir tmp base: {}",
                            self.fs_tmp_base.display()
                        ))),
                    )?;
                    return Ok(());
                }

                for attempt in 0..10u32 {
                    let name = format!("{prefix}-{}-{}", std::process::id(), attempt);
                    let dir = self.fs_tmp_base.join(name);
                    match fs::create_dir(&dir) {
                        Ok(()) => {
                            let rel = match rel_posix_path(&self.fs_root, &dir) {
                                Ok(r) => r,
                                Err(e) => {
                                    rt.call_resume_file_mk_temp_dir_err(
                                        store,
                                        ctx,
                                        suspension,
                                        &to_other_io_error(e),
                                    )?;
                                    return Ok(());
                                }
                            };
                            rt.call_resume_file_mk_temp_dir_ok(store, ctx, suspension, &rel)?;
                            return Ok(());
                        }
                        Err(e) if e.kind() == std::io::ErrorKind::AlreadyExists => continue,
                        Err(e) => {
                            rt.call_resume_file_mk_temp_dir_err(
                                store,
                                ctx,
                                suspension,
                                &to_other_io_error(e),
                            )?;
                            return Ok(());
                        }
                    }
                }

                rt.call_resume_file_mk_temp_dir_err(
                    store,
                    ctx,
                    suspension,
                    &io_error(KIND_OTHER, "failed to allocate temp directory"),
                )?;
                Ok(())
            }

            // --- process ---
            OpRequest::ProcessExec(r) => {
                let argv = r.argv;
                if argv.is_empty() || argv[0].is_empty() {
                    rt.call_resume_process_exec_err(
                        store,
                        ctx,
                        suspension,
                        &io_error(KIND_INVALID_INPUT, "argv must be a non-empty list"),
                    )?;
                    return Ok(());
                }

                let mut cmd = Command::new(&argv[0]);
                if argv.len() > 1 {
                    cmd.args(&argv[1..]);
                }

                cmd.stdin(Stdio::piped())
                    .stdout(Stdio::piped())
                    .stderr(Stdio::piped());

                if let Some(cwd) = r.cwd.as_deref() {
                    let rp = match self.file_path(cwd) {
                        Ok(p) => p,
                        Err(e) => {
                            rt.call_resume_process_exec_err(store, ctx, suspension, &e)?;
                            return Ok(());
                        }
                    };
                    cmd.current_dir(rp);
                }

                for ev in r.env {
                    cmd.env(ev.key, ev.value);
                }

                match cmd.spawn() {
                    Ok(child) => {
                        let id = self.next_process_id;
                        self.next_process_id += 1;
                        self.processes.insert(id, ProcessState::new(child));
                        rt.call_resume_process_exec_ok(store, ctx, suspension, id)?;
                    }
                    Err(e) => {
                        rt.call_resume_process_exec_err(
                            store,
                            ctx,
                            suspension,
                            &process_io_error_from(e),
                        )?;
                    }
                }
                Ok(())
            }
            OpRequest::ProcessExitValue(r) => {
                let Some(state) = self.processes.get_mut(&r.process_id) else {
                    rt.call_resume_process_exit_value_err(store, ctx, suspension, &invalid_process_handle())?;
                    return Ok(());
                };

                if let Err(e) = state.refresh_exit_status() {
                    rt.call_resume_process_exit_value_err(store, ctx, suspension, &process_io_error_from(e))?;
                    return Ok(());
                }

                if !state.exited {
                    rt.call_resume_process_exit_value_err(
                        store,
                        ctx,
                        suspension,
                        &io_error(KIND_OTHER, "process has not exited"),
                    )?;
                    return Ok(());
                }

                rt.call_resume_process_exit_value_ok(store, ctx, suspension, state.exit_value)?;
                Ok(())
            }
            OpRequest::ProcessIsAlive(r) => {
                let Some(state) = self.processes.get_mut(&r.process_id) else {
                    rt.call_resume_process_is_alive_err(store, ctx, suspension, &invalid_process_handle())?;
                    return Ok(());
                };

                if let Err(e) = state.refresh_exit_status() {
                    rt.call_resume_process_is_alive_err(store, ctx, suspension, &process_io_error_from(e))?;
                    return Ok(());
                }

                rt.call_resume_process_is_alive_ok(store, ctx, suspension, !state.exited)?;
                Ok(())
            }
            OpRequest::ProcessPid(r) => {
                let Some(state) = self.processes.get_mut(&r.process_id) else {
                    rt.call_resume_process_pid_err(store, ctx, suspension, &invalid_process_handle())?;
                    return Ok(());
                };

                let pid = state.child.id();
                if pid == 0 {
                    rt.call_resume_process_pid_err(
                        store,
                        ctx,
                        suspension,
                        &io_error(KIND_OTHER, "pid unavailable"),
                    )?;
                    return Ok(());
                }

                rt.call_resume_process_pid_ok(store, ctx, suspension, pid as i64)?;
                Ok(())
            }
            OpRequest::ProcessStop(r) => {
                let Some(state) = self.processes.get_mut(&r.process_id) else {
                    rt.call_resume_process_stop_err(store, ctx, suspension, &invalid_process_handle())?;
                    return Ok(());
                };

                if let Err(e) = state.refresh_exit_status() {
                    rt.call_resume_process_stop_err(store, ctx, suspension, &process_io_error_from(e))?;
                    return Ok(());
                }

                if state.exited {
                    rt.call_resume_process_stop_ok(store, ctx, suspension)?;
                    return Ok(());
                }

                match state.child.kill() {
                    Ok(()) => rt.call_resume_process_stop_ok(store, ctx, suspension)?,
                    Err(e) => rt.call_resume_process_stop_err(store, ctx, suspension, &process_io_error_from(e))?,
                }
                Ok(())
            }
            OpRequest::ProcessWaitFor(r) => {
                let Some(state) = self.processes.get_mut(&r.process_id) else {
                    rt.call_resume_process_wait_for_err(store, ctx, suspension, &invalid_process_handle())?;
                    return Ok(());
                };

                if let Err(e) = state.refresh_exit_status() {
                    rt.call_resume_process_wait_for_err(store, ctx, suspension, &process_io_error_from(e))?;
                    return Ok(());
                }

                if state.exited {
                    rt.call_resume_process_wait_for_ok(store, ctx, suspension, state.exit_value)?;
                    return Ok(());
                }

                match state.child.wait() {
                    Ok(status) => {
                        state.exited = true;
                        state.exit_value = normalize_exit_code(status);
                        rt.call_resume_process_wait_for_ok(store, ctx, suspension, state.exit_value)?;
                    }
                    Err(e) => rt.call_resume_process_wait_for_err(store, ctx, suspension, &process_io_error_from(e))?,
                }
                Ok(())
            }
            OpRequest::ProcessWaitForTimeout(r) => {
                let Some(state) = self.processes.get_mut(&r.process_id) else {
                    rt.call_resume_process_wait_for_timeout_err(store, ctx, suspension, &invalid_process_handle())?;
                    return Ok(());
                };

                if let Err(e) = state.refresh_exit_status() {
                    rt.call_resume_process_wait_for_timeout_err(store, ctx, suspension, &process_io_error_from(e))?;
                    return Ok(());
                }

                if state.exited {
                    rt.call_resume_process_wait_for_timeout_ok(store, ctx, suspension, true)?;
                    return Ok(());
                }

                let ms = clamp_ms(r.timeout_ms);
                if ms == 0 {
                    rt.call_resume_process_wait_for_timeout_ok(store, ctx, suspension, false)?;
                    return Ok(());
                }

                let deadline = Instant::now() + Duration::from_millis(ms);
                loop {
                    match state.child.try_wait() {
                        Ok(Some(status)) => {
                            state.exited = true;
                            state.exit_value = normalize_exit_code(status);
                            rt.call_resume_process_wait_for_timeout_ok(store, ctx, suspension, true)?;
                            return Ok(());
                        }
                        Ok(None) => {}
                        Err(e) => {
                            rt.call_resume_process_wait_for_timeout_err(
                                store,
                                ctx,
                                suspension,
                                &process_io_error_from(e),
                            )?;
                            return Ok(());
                        }
                    }

                    if Instant::now() >= deadline {
                        rt.call_resume_process_wait_for_timeout_ok(store, ctx, suspension, false)?;
                        return Ok(());
                    }

                    std::thread::sleep(Duration::from_millis(10));
                }
            }
            OpRequest::ProcessStdinWrite(r) => {
                let Some(state) = self.processes.get_mut(&r.process_id) else {
                    rt.call_resume_process_stdin_write_err(store, ctx, suspension, &invalid_process_handle())?;
                    return Ok(());
                };

                let Some(stdin) = state.child.stdin.as_mut() else {
                    rt.call_resume_process_stdin_write_err(
                        store,
                        ctx,
                        suspension,
                        &io_error(KIND_OTHER, "stdin unavailable"),
                    )?;
                    return Ok(());
                };

                match stdin.write_all(&r.bytes).and_then(|_| stdin.flush()) {
                    Ok(()) => rt.call_resume_process_stdin_write_ok(store, ctx, suspension)?,
                    Err(e) => rt.call_resume_process_stdin_write_err(
                        store,
                        ctx,
                        suspension,
                        &process_io_error_from(e),
                    )?,
                }
                Ok(())
            }
            OpRequest::ProcessStdoutRead(r) => {
                let Some(state) = self.processes.get_mut(&r.process_id) else {
                    rt.call_resume_process_stdout_read_err(store, ctx, suspension, &invalid_process_handle())?;
                    return Ok(());
                };

                let Some(stdout) = state.child.stdout.as_mut() else {
                    rt.call_resume_process_stdout_read_ok(store, ctx, suspension, &[])?;
                    return Ok(());
                };

                if r.max_bytes == 0 {
                    rt.call_resume_process_stdout_read_ok(store, ctx, suspension, &[])?;
                    return Ok(());
                }

                let mut buf = vec![0u8; r.max_bytes as usize];
                match stdout.read(&mut buf) {
                    Ok(0) => rt.call_resume_process_stdout_read_ok(store, ctx, suspension, &[])?,
                    Ok(n) => rt.call_resume_process_stdout_read_ok(store, ctx, suspension, &buf[..n])?,
                    Err(e) => rt.call_resume_process_stdout_read_err(
                        store,
                        ctx,
                        suspension,
                        &process_io_error_from(e),
                    )?,
                }
                Ok(())
            }
            OpRequest::ProcessStderrRead(r) => {
                let Some(state) = self.processes.get_mut(&r.process_id) else {
                    rt.call_resume_process_stderr_read_err(store, ctx, suspension, &invalid_process_handle())?;
                    return Ok(());
                };

                let Some(stderr) = state.child.stderr.as_mut() else {
                    rt.call_resume_process_stderr_read_ok(store, ctx, suspension, &[])?;
                    return Ok(());
                };

                if r.max_bytes == 0 {
                    rt.call_resume_process_stderr_read_ok(store, ctx, suspension, &[])?;
                    return Ok(());
                }

                let mut buf = vec![0u8; r.max_bytes as usize];
                match stderr.read(&mut buf) {
                    Ok(0) => rt.call_resume_process_stderr_read_ok(store, ctx, suspension, &[])?,
                    Ok(n) => rt.call_resume_process_stderr_read_ok(store, ctx, suspension, &buf[..n])?,
                    Err(e) => rt.call_resume_process_stderr_read_err(
                        store,
                        ctx,
                        suspension,
                        &process_io_error_from(e),
                    )?,
                }
                Ok(())
            }
            OpRequest::ProcessRelease(r) => {
                if let Some(mut state) = self.processes.remove(&r.process_id) {
                    if let Err(e) = state.refresh_exit_status() {
                        rt.call_resume_process_release_err(store, ctx, suspension, &process_io_error_from(e))?;
                        return Ok(());
                    }

                    if !state.exited {
                        // Avoid zombies by reaping in the background without blocking the runner.
                        let mut child = state.child;
                        std::thread::spawn(move || {
                            let _ = child.wait();
                        });
                    }
                }

                rt.call_resume_process_release_ok(store, ctx, suspension)?;
                Ok(())
            }

            // --- TCP ---
            OpRequest::TcpSocketConnect(r) => {
                let ip = match parse_ip(&r.ip) {
                    Ok(ip) => ip,
                    Err(e) => {
                        rt.call_resume_tcp_socket_connect_err(store, ctx, suspension, &e)?;
                        return Ok(());
                    }
                };

                let addr = SocketAddr::new(ip, r.port);
                match TcpStream::connect_timeout(&addr, self.tcp_connect_timeout) {
                    Ok(s) => {
                        s.set_read_timeout(Some(self.tcp_io_timeout)).ok();
                        s.set_write_timeout(Some(self.tcp_io_timeout)).ok();
                        let id = self.tcp.alloc_socket_id();
                        self.tcp.sockets.insert(id, s);
                        rt.call_resume_tcp_socket_connect_ok(store, ctx, suspension, id)?;
                    }
                    Err(e) => {
                        rt.call_resume_tcp_socket_connect_err(store, ctx, suspension, &tcp_io_error_from(e))?
                    }
                }
                Ok(())
            }
            OpRequest::TcpSocketRead(r) => {
                let Some(stream) = self.tcp.sockets.get_mut(&r.socket_id) else {
                    rt.call_resume_tcp_socket_read_err(
                        store,
                        ctx,
                        suspension,
                        &io_error(KIND_OTHER, "invalid socket handle."),
                    )?;
                    return Ok(());
                };

                let mut buf = vec![0u8; r.max_bytes as usize];
                if let Err(e) = stream.set_nonblocking(true) {
                    rt.call_resume_tcp_socket_read_err(store, ctx, suspension, &tcp_io_error_from(e))?;
                    return Ok(());
                }
                match stream.read(&mut buf) {
                    Ok(0) => rt.call_resume_tcp_socket_read_ok(store, ctx, suspension, &[])?,
                    Ok(n) => rt.call_resume_tcp_socket_read_ok(store, ctx, suspension, &buf[..n])?,
                    Err(e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                        let _ = stream.set_nonblocking(false);
                        return Err(PendingIo.into());
                    }
                    Err(e) => rt.call_resume_tcp_socket_read_err(store, ctx, suspension, &tcp_io_error_from(e))?,
                }
                let _ = stream.set_nonblocking(false);
                Ok(())
            }
            OpRequest::TcpSocketWrite(r) => {
                let Some(stream) = self.tcp.sockets.get_mut(&r.socket_id) else {
                    rt.call_resume_tcp_socket_write_err(
                        store,
                        ctx,
                        suspension,
                        &io_error(KIND_OTHER, "invalid socket handle."),
                    )?;
                    return Ok(());
                };

                match stream.write_all(&r.bytes) {
                    Ok(()) => rt.call_resume_tcp_socket_write_ok(store, ctx, suspension)?,
                    Err(e) => rt.call_resume_tcp_socket_write_err(store, ctx, suspension, &tcp_io_error_from(e))?,
                }
                Ok(())
            }
            OpRequest::TcpSocketClose(r) => {
                if self.tcp.sockets.remove(&r.socket_id).is_none() {
                    rt.call_resume_tcp_socket_close_err(
                        store,
                        ctx,
                        suspension,
                        &io_error(KIND_OTHER, "invalid socket handle."),
                    )?;
                    return Ok(());
                }

                rt.call_resume_tcp_socket_close_ok(store, ctx, suspension)?;
                Ok(())
            }
            OpRequest::TcpServerBind(r) => {
                let ip = match parse_ip(&r.ip) {
                    Ok(ip) => ip,
                    Err(e) => {
                        rt.call_resume_tcp_server_bind_err(store, ctx, suspension, &e)?;
                        return Ok(());
                    }
                };

                let addr = SocketAddr::new(ip, r.port);
                match TcpListener::bind(addr) {
                    Ok(l) => {
                        if let Err(e) = l.set_nonblocking(true) {
                            rt.call_resume_tcp_server_bind_err(store, ctx, suspension, &tcp_io_error_from(e))?;
                            return Ok(());
                        }
                        let id = self.tcp.alloc_server_id();
                        self.tcp.servers.insert(id, l);
                        rt.call_resume_tcp_server_bind_ok(store, ctx, suspension, id)?;
                    }
                    Err(e) => {
                        rt.call_resume_tcp_server_bind_err(store, ctx, suspension, &tcp_io_error_from(e))?
                    }
                }
                Ok(())
            }
            OpRequest::TcpServerAccept(r) => {
                let Some(listener) = self.tcp.servers.get_mut(&r.server_id) else {
                    rt.call_resume_tcp_server_accept_err(
                        store,
                        ctx,
                        suspension,
                        &io_error(KIND_OTHER, "invalid server handle."),
                    )?;
                    return Ok(());
                };

                match listener.accept() {
                    Ok((s, _addr)) => {
                        s.set_read_timeout(Some(self.tcp_io_timeout)).ok();
                        s.set_write_timeout(Some(self.tcp_io_timeout)).ok();
                        let id = self.tcp.alloc_socket_id();
                        self.tcp.sockets.insert(id, s);
                        rt.call_resume_tcp_server_accept_ok(store, ctx, suspension, id)?;
                    }
                    Err(e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                        return Err(PendingIo.into());
                    }
                    Err(e) => rt.call_resume_tcp_server_accept_err(store, ctx, suspension, &tcp_io_error_from(e))?,
                }
                Ok(())
            }
            OpRequest::TcpServerLocalPort(r) => {
                let Some(listener) = self.tcp.servers.get_mut(&r.server_id) else {
                    rt.call_resume_tcp_server_local_port_err(
                        store,
                        ctx,
                        suspension,
                        &io_error(KIND_OTHER, "invalid server handle."),
                    )?;
                    return Ok(());
                };

                match listener.local_addr() {
                    Ok(addr) => rt.call_resume_tcp_server_local_port_ok(store, ctx, suspension, addr.port())?,
                    Err(e) => rt.call_resume_tcp_server_local_port_err(store, ctx, suspension, &tcp_io_error_from(e))?,
                }
                Ok(())
            }
            OpRequest::TcpServerClose(r) => {
                if self.tcp.servers.remove(&r.server_id).is_none() {
                    rt.call_resume_tcp_server_close_err(
                        store,
                        ctx,
                        suspension,
                        &io_error(KIND_OTHER, "invalid server handle."),
                    )?;
                    return Ok(());
                }

                rt.call_resume_tcp_server_close_ok(store, ctx, suspension)?;
                Ok(())
            }

            OpRequest::Unknown(r) => {
                let msg = format!("unsupported op: effId={} opId={}", r.eff_id, r.op_id);
                let v = rt.call_box_string(&mut *store, ctx, &msg)?;
                rt.call_resume_throw(&mut *store, ctx, suspension, v)?;
                v.resource_drop(&mut *store)?;
                Ok(())
            }

            // The runner handles timer-sleep.
            OpRequest::TimerSleep(_) => Ok(()),
        }
    }
}
