const DEFAULT_BUDGET = 100;
const DEFAULT_MAX_REDIRECTS = 20;

function sleep(ms) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

function clampMs(ms) {
  if (!Number.isFinite(ms) || ms <= 0) return 0;
  // Avoid exceeding the maximum delay supported by setTimeout (~2^31-1 ms).
  return Math.min(ms, 0x7fffffff);
}

function bigintToSafeNumber(x) {
  // Most of our time values are expected to fit in a JS number in practice.
  // If a host supplies a huge value, clamp it rather than throwing.
  if (typeof x !== "bigint") return Number(x);
  const max = BigInt(Number.MAX_SAFE_INTEGER);
  if (x > max) return Number.MAX_SAFE_INTEGER;
  if (x < -max) return -Number.MAX_SAFE_INTEGER;
  return Number(x);
}

function kebabToPascal(tag) {
  return tag
    .split("-")
    .filter((p) => p.length > 0)
    .map((p) => p[0].toUpperCase() + p.slice(1))
    .join("");
}

function isRedirectStatus(status) {
  return status === 301 || status === 302 || status === 303 || status === 307 || status === 308;
}

function splitHeaderValue(value) {
  // In Fetch, multiple header values are often combined into one comma-separated string.
  // For the portable Flix HTTP contract we need *separate* values (at least for custom headers).
  // This heuristic is RFC-imperfect (some headers contain commas), but it matches current
  // portable stdlib expectations and JVM tests for repeated headers.
  return value
    .split(",")
    .map((v) => v.trim())
    .filter((v) => v.length > 0);
}

export class FlixRunner {
  constructor(runtime, options = {}) {
    if (!runtime) throw new Error("FlixRunner: missing runtime");

    this.runtime = runtime;
    this.handlers = options.handlers ?? Object.create(null);

    this.budget = options.budget ?? DEFAULT_BUDGET;
    this.maxRedirects = options.maxRedirects ?? DEFAULT_MAX_REDIRECTS;
    this.httpTimeoutMs = options.httpTimeoutMs ?? null; // null => no timeout
  }

  /**
   * Drive the scheduler and host ops until `taskId` completes (pollTask returns a non-null outcome).
   *
   * Returns the raw `task-outcome` lifted by jco: `{ tag: "ok"|"thrown", val: valueHandle }`.
   *
   * Note: The caller owns `val` and should dispose it after unboxing.
   */
  async runTaskToCompletion(ctx, taskId, options = {}) {
    const budget = options.budget ?? this.budget;

    const pending = new Set();
    const runState = { closed: false };

    try {
      while (true) {
        const out = this.runtime.pollTask(ctx, taskId);
        if (out != null) {
          runState.closed = true;
          return out;
        }

        const suspensions = this.runtime.schedStep(ctx, budget);
        for (const s of suspensions) {
          const p = this._handleSuspension(ctx, s, runState)
            .catch((e) => {
              if (runState.closed) return;
              // Best-effort: if a handler throws, try to resume with an `Other` IO error (when possible).
              try {
                const msg = e instanceof Error ? e.message : String(e);
                this._resumeIoErr(ctx, s, this.runtime.suspensionRequest(ctx, s).tag, {
                  kindCode: 14,
                  msg,
                });
              } catch {
                // If we can't resume, surface the error to the host.
                throw e;
              }
            })
            .finally(() => pending.delete(p));
          pending.add(p);
        }

        if (pending.size > 0) {
          // Keep pumping the cooperative scheduler even while host I/O is pending.
          // Otherwise a long-lived external promise (e.g. a detached timeout task)
          // can stall purely internal wakeups that have already made tasks runnable.
          await sleep(1);
        } else {
          // Ensure we yield to the JS event loop even for pure computations.
          await Promise.resolve();
        }
      }
    } finally {
      runState.closed = true;
    }
  }

  async _handleSuspension(ctx, suspension, runState) {
    const req = this.runtime.suspensionRequest(ctx, suspension);
    const tag = req.tag;

    const handler = this.handlers[tag];
    if (handler) {
      await handler({
        runtime: this.runtime,
        ctx,
        suspension,
        request: req.val,
        isClosed: () => runState.closed,
      });
      return;
    }

    switch (tag) {
      case "timer-sleep": {
        const ms = clampMs(bigintToSafeNumber(req.val.ms));
        await sleep(ms);
        if (runState.closed) return;
        this.runtime.resumeTimerSleep(ctx, suspension);
        return;
      }
      case "console-readln": {
        const lines = globalThis.__flix_stdin_lines;
        let line = "";
        if (Array.isArray(lines) && lines.length > 0) {
          const next = lines.shift();
          line = typeof next === "string" ? next : String(next);
        }
        // Normalize accidental line terminators to match `readln` semantics.
        if (line.endsWith("\n")) line = line.slice(0, -1);
        if (line.endsWith("\r")) line = line.slice(0, -1);

        if (runState.closed) return;
        this.runtime.resumeConsoleReadlnOk(ctx, suspension, line);
        return;
      }
      case "http-request": {
        await this._handleHttpRequest(ctx, suspension, req.val, runState);
        return;
      }
      case "unknown": {
        const msg = `unsupported op: effId=${req.val.effId} opId=${req.val.opId}`;
        const v = this.runtime.boxString(ctx, msg);
        try {
          if (runState.closed) return;
          this.runtime.resumeThrow(ctx, suspension, v);
        } finally {
          // `resumeThrow` consumes the suspension, not the value.
          v?.[Symbol.dispose ?? Symbol.for("dispose")]?.();
        }
        return;
      }
      default: {
        this._resumeIoErr(ctx, suspension, tag, { kindCode: 12, msg: "unsupported" });
      }
    }
  }

  _resumeIoErr(ctx, suspension, tag, err) {
    if (tag === "http-request") {
      this.runtime.resumeHttpErr(ctx, suspension, err);
      return;
    }

    const pascal = kebabToPascal(tag);
    const method = `resume${pascal}Err`;

    const fn = this.runtime[method];
    if (typeof fn !== "function") {
      throw new Error(`FlixRunner: no err resumer for op '${tag}' (expected ${method})`);
    }

    fn.call(this.runtime, ctx, suspension, err);
  }

  async _handleHttpRequest(ctx, suspension, req, runState) {
    const fetchFn = globalThis.fetch;
    if (typeof fetchFn !== "function") {
      if (runState.closed) return;
      this.runtime.resumeHttpErr(ctx, suspension, {
        kindCode: 12,
        msg: "fetch unavailable",
      });
      return;
    }

    let initialUrl;
    try {
      initialUrl = new URL(req.url);
    } catch (e) {
      if (runState.closed) return;
      this.runtime.resumeHttpErr(ctx, suspension, {
        kindCode: 4,
        msg: "invalid URL",
      });
      return;
    }

    const initialScheme = initialUrl.protocol.replace(/:$/, "").toLowerCase();
    if (initialScheme !== "http" && initialScheme !== "https") {
      if (runState.closed) return;
      this.runtime.resumeHttpErr(ctx, suspension, {
        kindCode: 12,
        msg: "unsupported URL scheme",
      });
      return;
    }

    let url = initialUrl.toString();
    let method = req.method;
    let body = req.body;

    // Preserve request headers across redirects (portable semantics).
    // We'll rebuild a `Headers` object per hop so we can tweak for method rewrites.
    const originalHeaders = Array.isArray(req.headers) ? req.headers : [];

    for (let redirects = 0; redirects <= this.maxRedirects; redirects++) {
      const headers = new Headers();
      for (const h of originalHeaders) {
        // WIT headers are already name/value strings; allow duplicates.
        headers.append(h.name, h.value);
      }

      // If we rewrote to GET (no body), drop common body-related headers.
      if (!body && method === "GET") {
        headers.delete("content-length");
        headers.delete("content-type");
      }

      const ac = this.httpTimeoutMs != null ? new AbortController() : null;
      const timeout =
        this.httpTimeoutMs != null
          ? setTimeout(() => ac.abort(new Error("timeout")), clampMs(this.httpTimeoutMs))
          : null;

      let resp;
      try {
        resp = await fetchFn(url, {
          method,
          headers,
          body,
          redirect: "manual",
          signal: ac?.signal,
        });
      } catch (e) {
        if (timeout != null) clearTimeout(timeout);

        // Timeout / abort.
        if (e?.name === "AbortError") {
          if (runState.closed) return;
          this.runtime.resumeHttpErr(ctx, suspension, { kindCode: 10, msg: "timeout" });
          return;
        }

        // Best-effort network failure classification.
        const msg = e instanceof Error ? e.message : String(e);
        const kindCode =
          /ENOTFOUND|Unknown host|getaddrinfo/i.test(msg) ? 13 : 1; // UnknownHost vs ConnectionFailed
        if (runState.closed) return;
        this.runtime.resumeHttpErr(ctx, suspension, { kindCode, msg });
        return;
      } finally {
        if (timeout != null) clearTimeout(timeout);
      }

      if (isRedirectStatus(resp.status)) {
        const loc = resp.headers.get("location");
        if (loc == null) break;

        let next;
        try {
          next = new URL(loc, url);
        } catch {
          if (runState.closed) return;
          this.runtime.resumeHttpErr(ctx, suspension, { kindCode: 14, msg: "invalid redirect URL" });
          return;
        }

        const nextScheme = next.protocol.replace(/:$/, "").toLowerCase();
        if (initialScheme === "https" && nextScheme === "http") {
          if (runState.closed) return;
          this.runtime.resumeHttpErr(ctx, suspension, {
            kindCode: 9, // PolicyViolation
            msg: "redirect disallowed: https -> http",
          });
          return;
        }

        if (nextScheme !== "http" && nextScheme !== "https") {
          if (runState.closed) return;
          this.runtime.resumeHttpErr(ctx, suspension, {
            kindCode: 12,
            msg: "unsupported redirect scheme",
          });
          return;
        }

        // Redirect method rewriting (portable semantics / JVM parity):
        // - 303: always GET
        // - 301/302: non-GET/HEAD become GET
        // - 307/308: preserve method
        if (resp.status === 303) {
          method = "GET";
          body = undefined;
        } else if ((resp.status === 301 || resp.status === 302) && method !== "GET" && method !== "HEAD") {
          method = "GET";
          body = undefined;
        }

        url = next.toString();
        continue;
      }

      const text = await resp.text();

      const outHeaders = [];
      for (const [name, value] of resp.headers.entries()) {
        const lname = name.toLowerCase();
        for (const v of splitHeaderValue(value)) {
          outHeaders.push({ name: lname, value: v });
        }
      }

      if (runState.closed) return;
      this.runtime.resumeHttpOk(ctx, suspension, {
        status: resp.status,
        headers: outHeaders,
        body: text,
      });
      return;
    }

    if (runState.closed) return;
    this.runtime.resumeHttpErr(ctx, suspension, { kindCode: 14, msg: "redirect loop" });
  }
}
