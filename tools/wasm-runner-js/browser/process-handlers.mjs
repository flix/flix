const KIND_UNSUPPORTED = 12;

function unsupportedProcessError(msg) {
  return {
    kindCode: KIND_UNSUPPORTED,
    msg,
  };
}

export function makeUnsupportedProcessHandlers(msg = "process unavailable in browser") {
  const err = unsupportedProcessError(msg);

  return {
    "process-exec": async ({ runtime, ctx, suspension }) => {
      runtime.resumeProcessExecErr(ctx, suspension, err);
    },
    "process-exit-value": async ({ runtime, ctx, suspension }) => {
      runtime.resumeProcessExitValueErr(ctx, suspension, err);
    },
    "process-is-alive": async ({ runtime, ctx, suspension }) => {
      runtime.resumeProcessIsAliveErr(ctx, suspension, err);
    },
    "process-pid": async ({ runtime, ctx, suspension }) => {
      runtime.resumeProcessPidErr(ctx, suspension, err);
    },
    "process-stop": async ({ runtime, ctx, suspension }) => {
      runtime.resumeProcessStopErr(ctx, suspension, err);
    },
    "process-wait-for": async ({ runtime, ctx, suspension }) => {
      runtime.resumeProcessWaitForErr(ctx, suspension, err);
    },
    "process-wait-for-timeout": async ({ runtime, ctx, suspension }) => {
      runtime.resumeProcessWaitForTimeoutErr(ctx, suspension, err);
    },
    "process-stdin-write": async ({ runtime, ctx, suspension }) => {
      runtime.resumeProcessStdinWriteErr(ctx, suspension, err);
    },
    "process-stdout-read": async ({ runtime, ctx, suspension }) => {
      runtime.resumeProcessStdoutReadErr(ctx, suspension, err);
    },
    "process-stderr-read": async ({ runtime, ctx, suspension }) => {
      runtime.resumeProcessStderrReadErr(ctx, suspension, err);
    },
    "process-release": async ({ runtime, ctx, suspension }) => {
      runtime.resumeProcessReleaseErr(ctx, suspension, err);
    },
  };
}
