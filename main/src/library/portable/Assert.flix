/*
 * Copyright 2026 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

///
/// Portable `Assert` overlay for non-JVM targets.
///
/// The surface matches the JVM library, but assertion failure is represented as a portable `Exn`
/// carrying the rendered failure message.
///
pub eff Assert {

    ///
    /// Asserts that the given condition `cond` is `true` with a message `msg`.
    ///
    def assert(cond: Bool, msg: RichString): Unit

}

pub mod Assert {

    use RichString.{blue, red, text, yellow};

    pub def assertTrue(cond: Bool): Unit \ Assert =
        let m = red("Assertion failed: ") + text("expected ") + blue("true");
        Assert.assert(cond, m)

    pub def assertTrueWithMsg(cond: Bool, msg: String): Unit \ Assert =
        Assert.assert(cond, text(msg))

    pub def assertFalse(cond: Bool): Unit \ Assert =
        let m = red("Assertion failed: ") + text("expected ") + blue("false");
        Assert.assert(not cond, m)

    pub def assertFalseWithMsg(cond: Bool, msg: String): Unit \ Assert =
        Assert.assert(not cond, text(msg))

    pub def assertEq(expected: {expected = a}, actual: a): Unit \ Assert with Eq[a], ToString[a] =
        let c = actual == expected#expected;
        let m = red("Assertion failed: ") + text("expected ") + blue("`${expected#expected}`") + text(", but got ") + yellow("`${actual}`");
        Assert.assert(c, m)

    pub def assertEqWithMsg(expected: {expected = a}, actual: a, msg: String): Unit \ Assert with Eq[a] =
        let c = actual == expected#expected;
        let m = text(msg);
        Assert.assert(c, m)

    pub def assertNeq(unexpected: {unexpected = a}, actual: a): Unit \ Assert with Eq[a], ToString[a] =
        let c = actual != unexpected#unexpected;
        let m = red("Assertion failed: ") + text("expected values to be different, but both were ") + yellow("`${actual}`");
        Assert.assert(c, m)

    pub def assertNeqWithMsg(unexpected: {unexpected = a}, actual: a, msg: String): Unit \ Assert with Eq[a] =
        let c = actual != unexpected#unexpected;
        let m = text(msg);
        Assert.assert(c, m)

    pub def assertSome(o: Option[a]): Unit \ Assert =
        let c = Option.nonEmpty(o);
        let m = red("Assertion failed: ") + text("expected ") + blue("Some(_)") + text(", but got ") + yellow("None");
        Assert.assert(c, m)

    pub def assertNone(o: Option[a]): Unit \ Assert =
        let c = Option.isEmpty(o);
        let m = red("Assertion failed: ") + text("expected ") + blue("None") + text(", but got ") + yellow("Some(_)");
        Assert.assert(c, m)

    pub def assertOk(r: Result[e, a]): Unit \ Assert =
        let c = Result.isOk(r);
        let m = red("Assertion failed: ") + text("expected ") + blue("Ok(_)") + text(", but got ") + yellow("Err(_)");
        Assert.assert(c, m)

    pub def assertErr(r: Result[e, a]): Unit \ Assert =
        let c = Result.isErr(r);
        let m = red("Assertion failed: ") + text("expected ") + blue("Err(_)") + text(", but got ") + yellow("Ok(_)");
        Assert.assert(c, m)

    pub def assertEmpty(ma: m[a]): Unit \ (Assert + Foldable.Aef[m]) with Foldable[m], ToString[m[a]] =
        let c = Foldable.isEmpty(ma);
        let m = red("Assertion failed: ") + text("expected ") + blue("empty collection") + text(", but got ") + yellow("${ma}");
        Assert.assert(c, m)

    pub def assertNonEmpty(ma: m[a]): Unit \ (Assert + Foldable.Aef[m]) with Foldable[m], ToString[m[a]] =
        let c = Foldable.nonEmpty(ma);
        let m = red("Assertion failed: ") + text("expected ") + blue("non-empty collection") + text(", but got ") + yellow("${ma}");
        Assert.assert(c, m)

    pub def assertMemberOf(x: a, ma: m[a]): Unit \ (Assert + Foldable.Aef[m]) with Foldable[m], Eq[a], ToString[a], ToString[m[a]] =
        let c = Foldable.memberOf(x, ma);
        let m = red("Assertion failed: ") + text("expected ") + blue("`${x}`") + text(" to be a member of ") + yellow("${ma}");
        Assert.assert(c, m)

    pub def success(msg: String): Unit \ Assert =
        Assert.assert(true, text(msg))

    pub def fail(msg: String): Unit \ Assert =
        Assert.assert(false, text(msg))

    pub def handle(f: a -> b \ ef): (a -> b \ (ef - Assert) + IO) = x ->
        run {
            f(x)
        } with handler Assert {
            def assert(cond, msg, resume) = {
                if (cond)
                    resume()
                else {
                    let rendered = RichString.toString(msg);
                    println(rendered);
                    throw Exn.mk(rendered)
                }
            }
        }

    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - Assert) + IO = handle(f)()

    pub def handleWithNoOp(f: a -> b \ ef): (a -> b \ (ef - Assert)) = x ->
        run {
            f(x)
        } with handler Assert {
            def assert(_cond, _msg, resume) = resume()
        }

    pub def handleWithLogger(f: a -> b \ ef): (a -> b \ (ef - Assert) + Logger) = x ->
        run {
            f(x)
        } with handler Assert {
            def assert(cond, msg, resume) = {
                if (cond)
                    resume()
                else {
                    Logger.warn(msg);
                    resume()
                }
            }
        }

    pub def handleWithStdOut(f: a -> b \ ef): (a -> b \ (ef - Assert) + IO) = x ->
        run {
            f(x)
        } with handler Assert {
            def assert(cond, msg, resume) = {
                if (cond)
                    resume()
                else {
                    println(RichString.toString(msg));
                    resume()
                }
            }
        }

    pub def handleWithStdErr(f: a -> b \ ef): (a -> b \ (ef - Assert) + IO) = x ->
        run {
            f(x)
        } with handler Assert {
            def assert(cond, msg, resume) = {
                if (cond)
                    resume()
                else {
                    use Console.eprintln;
                    run {
                        eprintln(RichString.toString(msg))
                    } with Console.runWithIO;
                    resume()
                }
            }
        }

    pub def handleWithPrompt(f: a -> b \ ef): (a -> b \ (ef - Assert) + IO) = x ->
        run {
            f(x)
        } with handler Assert {
            def assert(cond, msg, resume) = {
                if (cond)
                    resume()
                else {
                    use Console.{print, readln};
                    run {
                        println("");
                        println(RichString.toString(msg));
                        println("");
                        print(RichString.toString(yellow("Continue execution? (Y/n): ")));
                        let input = readln();
                        let trimmed = String.trim(input);
                        if (String.toLowerCase(trimmed) == "n" or String.toLowerCase(trimmed) == "no")
                            throw Exn.mk("Execution aborted by user after the following assertion failure: ${RichString.toString(msg)}")
                        else
                            resume()
                    } with Console.runWithIO
                }
            }
        }

}
