import { newCtx, Exports } from "../../build/wasm/sdk/js/export-smoke.bindings.mjs";

const disposeSym = Symbol.dispose ?? Symbol.for("dispose");

function maybeDispose(x) {
  try {
    const fn = x?.[disposeSym];
    if (typeof fn === "function") fn.call(x);
  } catch {
    // ignore
  }
}

function assert(cond, msg) {
  if (!cond) throw new Error(msg);
}

function eqBytes(a, b) {
  if (!(a instanceof Uint8Array) || !(b instanceof Uint8Array)) return false;
  if (a.length !== b.length) return false;
  for (let i = 0; i < a.length; i++) {
    if (a[i] !== b[i]) return false;
  }
  return true;
}

const ctx = newCtx();

try {
  const add = Exports.Api.add(ctx, 1, 2);
  assert(add.tag === "ok" && add.val === 3, `bad add: ${JSON.stringify(add)}`);

  const echo = Exports.Api.echo(ctx, "hello");
  assert(echo.tag === "ok" && echo.val === "hello", `bad echo: ${JSON.stringify(echo)}`);

  const data = new Uint8Array([0, 1, 2, 255]);
  const bytes = Exports.Api.bytesId(ctx, data);
  assert(bytes.tag === "ok" && eqBytes(bytes.val, data), "bad bytes");

  const some = Exports.Api.maybeSucc(ctx, 41);
  assert(some.tag === "ok" && some.val === 42, `bad maybeSucc(Some): ${JSON.stringify(some)}`);

  const none = Exports.Api.maybeSucc(ctx, null);
  assert(none.tag === "ok" && none.val === null, `bad maybeSucc(None): ${JSON.stringify(none)}`);

  const pair = Exports.Api.flipPair(ctx, [7, "hello"]);
  assert(pair.tag === "ok" && pair.val[0] === "hello" && pair.val[1] === 7, `bad flipPair: ${JSON.stringify(pair)}`);

  const even = Exports.Api.halfEven(ctx, 8);
  assert(even.tag === "ok" && even.val.tag === "ok" && even.val.val === 4, `bad halfEven(8): ${JSON.stringify(even)}`);

  const odd = Exports.Api.halfEven(ctx, 7);
  assert(odd.tag === "ok" && odd.val.tag === "err" && odd.val.val === "odd", `bad halfEven(7): ${JSON.stringify(odd)}`);

  const badge = Exports.Api.badge(ctx, { name: "hello", score: 41 });
  assert(badge.tag === "ok" && badge.val.label === "hello" && badge.val.score === 42, `bad badge: ${JSON.stringify(badge)}`);

  const list = Exports.Api.prependAnswer(ctx, [1, 2, 3]);
  assert(list.tag === "ok" && JSON.stringify(list.val) === JSON.stringify([42, 1, 2, 3]), `bad prependAnswer: ${JSON.stringify(list)}`);

  const ints = Exports.Api.echoInts(ctx, [1, 2, 3]);
  assert(ints.tag === "ok" && JSON.stringify(ints.val) === JSON.stringify([1, 2, 3]), `bad echoInts: ${JSON.stringify(ints)}`);

  const names = Exports.Api.echoNames(ctx, ["hello", "world"]);
  assert(names.tag === "ok" && JSON.stringify(names.val) === JSON.stringify(["hello", "world"]), `bad echoNames: ${JSON.stringify(names)}`);

  const promoted = Exports.Api.promoteUsers(ctx, [
    { name: "hello", score: 41 },
    { name: "world", score: 9 },
  ]);
  assert(
    promoted.tag === "ok" &&
      JSON.stringify(promoted.val) === JSON.stringify([
        { label: "hello", score: 42 },
        { label: "world", score: 10 },
      ]),
    `bad promoteUsers: ${JSON.stringify(promoted)}`
  );

  const userArray = Exports.Api.echoUserArray(ctx, [
    { name: "hello", score: 41 },
    { name: "world", score: 9 },
  ]);
  assert(
    userArray.tag === "ok" &&
      JSON.stringify(userArray.val) === JSON.stringify([
        { name: "hello", score: 41 },
        { name: "world", score: 9 },
      ]),
    `bad echoUserArray: ${JSON.stringify(userArray)}`
  );

  const flippedPairs = Exports.Api.flipPairs(ctx, [
    [7, "hello"],
    [9, "world"],
  ]);
  assert(
    flippedPairs.tag === "ok" &&
      JSON.stringify(flippedPairs.val) === JSON.stringify([
        ["hello", 7],
        ["world", 9],
      ]),
    `bad flipPairs: ${JSON.stringify(flippedPairs)}`
  );

  const maybeInts = Exports.Api.echoMaybeInts(ctx, [41, null, 9]);
  assert(
    maybeInts.tag === "ok" &&
      JSON.stringify(maybeInts.val) === JSON.stringify([41, null, 9]),
    `bad echoMaybeInts: ${JSON.stringify(maybeInts)}`
  );

  const susp = Exports.Api.suspendEcho(ctx, "hello");
  assert(susp.tag === "suspended", `bad suspend tag: ${String(susp.tag)}`);
  assert(Exports.Api.requestSuspendEcho(ctx, susp.val) === "hello", "bad suspension request");

  const resumed = Exports.Api.resumeSuspendEcho(ctx, susp.val, "ok");
  assert(resumed.tag === "ok" && resumed.val === "ok", `bad resumed result: ${JSON.stringify(resumed)}`);

  console.log("OK");
} finally {
  maybeDispose(ctx);
}
