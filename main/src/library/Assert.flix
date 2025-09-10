/*
 * Copyright 2025 Magnus Madsen
 * Copyright 2025 Daniel Neo López Martínez
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
/// An effect used to perform assertions during program execution.
///
pub eff Assert {

    ///
    /// Asserts that the given condition `cond` is `true` with a message `msg`.
    ///
    def assert(cond: Bool, msg: RichString): Unit

}

mod Assert {

    use RichString.{blue, red, text, yellow};
    import java.lang.AssertionError;

    ///
    /// Asserts that the given condition `cond` is `true`.
    ///
    pub def assertTrue(cond: Bool): Unit \ Assert =
        Assert.assert(cond, text("Assertion failed: expected true"))

    ///
    /// Asserts that the given condition `cond` is `true` with the given message `msg`.
    ///
    pub def assertTrueWithMsg(cond: Bool, msg: String): Unit \ Assert =
        Assert.assert(cond, text(msg))

    ///
    /// Asserts that the given condition `cond` is `false`.
    ///
    pub def assertFalse(cond: Bool): Unit \ Assert =
        Assert.assert(not cond, text("Assertion failed: expected false"))

    ///
    /// Asserts that the given condition `cond` is `false` with the given message `msg`.
    ///
    pub def assertFalseWithMsg(cond: Bool, msg: String): Unit \ Assert =
        Assert.assert(not cond, text(msg))

    ///
    /// Asserts that the given values `expected` and `actual` are equal.
    ///
    pub def assertEq(expected: {expected = a}, actual: a): Unit \ Assert with Eq[a], ToString[a] =
        Assert.assert(actual == expected#expected, red("Assertion failed: ") + text("expected ") + blue("`${expected#expected}`") + text(", but got ") + yellow("`${actual}`"))

    ///
    /// Asserts that the given values `expected` and `actual` are equal with the given message `msg`.
    ///
    pub def assertEqWithMsg(expected: {expected = a}, actual: a, msg: String): Unit \ Assert with Eq[a] =
        Assert.assert(actual == expected#expected, text(msg))

    ///
    /// Asserts that the given values `unexpected` and `actual` are not equal.
    ///
    pub def assertNeq(unexpected: {unexpected = a}, actual: a): Unit \ Assert with Eq[a], ToString[a] =
        Assert.assert(actual != unexpected#unexpected, text("Assertion failed: expected values to be different, but both were ") + text("`${actual}`"))

    ///
    /// Asserts that the given values `unexpected` and `actual` are not equal with the given message `msg`.
    ///
    pub def assertNeqWithMsg(unexpected: {unexpected = a}, actual: a, msg: String): Unit \ Assert with Eq[a] =
        Assert.assert(actual != unexpected#unexpected, text(msg))

    ///
    /// Asserts that the given Option `o` is `Some(v)`.
    ///
    pub def assertSome(o: Option[a]): Unit \ Assert =
        Assert.assert(Option.nonEmpty(o), text("Assertion failed: expected Some(_), but got None"))

    ///
    /// Asserts that the given Option `o` is `None`.
    ///
    pub def assertNone(o: Option[a]): Unit \ Assert =
        Assert.assert(Option.isEmpty(o), text("Assertion failed: expected None, but got Some(_)"))

    ///
    /// Asserts that the given Result `r` is `Ok(v)`.
    ///
    pub def assertOk(r: Result[e, a]): Unit \ Assert =
        Assert.assert(Result.isOk(r), text("Assertion failed: expected Ok(_), but got Err(_)"))

    ///
    /// Asserts that the given Result `r` is `Err(e)`.
    ///
    pub def assertErr(r: Result[e, a]): Unit \ Assert =
        Assert.assert(Result.isErr(r), text("Assertion failed: expected Err(_), but got Ok(_)"))

    ///
    /// Asserts that the given foldable `ma` is empty.
    ///
    pub def assertEmpty(ma: m[a]): Unit \ (Assert + Foldable.Aef[m]) with Foldable[m], ToString[m[a]] =
        Assert.assert(Foldable.isEmpty(ma), text("Assertion failed: expected empty collection, but got ") + text("`${ma}`"))

    ///
    /// Asserts that the given foldable `ma` is non-empty.
    ///
    pub def assertNonEmpty(ma: m[a]): Unit \ (Assert + Foldable.Aef[m]) with Foldable[m], ToString[m[a]] =
        Assert.assert(Foldable.nonEmpty(ma), text("Assertion failed: expected non-empty collection, but got ") + text("`${ma}`"))

    ///
    /// Asserts that the given element `x` is a member of the given foldable `ma`.
    ///
    pub def assertMemberOf(x: a, ma: m[a]): Unit \ (Assert + Foldable.Aef[m]) with Foldable[m], Eq[a], ToString[a], ToString[m[a]] =
        Assert.assert(Foldable.memberOf(x, ma), text("Assertion failed: expected ") + text("`${x}`") + text(" to be a member of ") + text("`${ma}`"))

    ///
    /// Unconditionally succeeds with the given message `msg`.
    ///
    pub def success(msg: String): Unit \ Assert =
        Assert.assert(true, text(msg))

    ///
    /// Unconditionally fails with the given message `msg`.
    ///
    pub def fail(msg: String): Unit \ Assert =
        Assert.assert(false, text(msg))

    ///
    /// Handles the `Assert` effect of the given function `f` by throwing exceptions on assertion failures.
    ///
    /// In other words, re-interprets the `Assert` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): (a -> b \ (ef - Assert) + IO + Sys) = x ->
        run {
            f(x)
        } with handler Assert {
            def assert(cond, msg, resume) = {
                if (cond)
                    resume(())
                else {
                    run {
                        println("");
                        println(RichString.toAnsiString(msg));
                        println("");
                        throw new AssertionError(RichString.toString(msg))
                    } with Environment.runWithIO
                }
            }
        }

    ///
    /// Handles the `Assert` effect by ignoring all assertions (no-op).
    ///
    pub def handleWithNoOp(f: a -> b \ ef): (a -> b \ (ef - Assert)) = x ->
        run {
            f(x)
        } with handler Assert {
            def assert(_cond, _msg, resume) = resume(())
        }

    ///
    /// Handles the `Assert` effect by logging assertion failures using the `Logger` effect.
    ///
    /// Assertions continue execution after logging the assertion failure.
    ///
    pub def handleWithLogger(f: a -> b \ ef): (a -> b \ (ef - Assert) + Logger) = x ->
        run {
            f(x)
        } with handler Assert {
            def assert(cond, msg, resume) = {
                if (cond)
                    resume(())
                else {
                    Logger.warn(msg);
                    resume(())
                }
            }
        }

    ///
    /// Handles the `Assert` effect by printing assertion failures to stdout.
    ///
    /// Assertions continue execution after printing the assertion failure.
    ///
    pub def handleWithStdOut(f: a -> b \ ef): (a -> b \ (ef - Assert) + IO + Sys) = x ->
        run {
            f(x)
        } with handler Assert {
            def assert(cond, msg, resume) = {
                if (cond)
                    resume(())
                else {
                    println(RichString.toAnsiString(msg));
                    resume(())
                }
            }
        }

    ///
    /// Handles the `Assert` effect by printing assertion failures to stderr.
    ///
    /// Assertions continue execution after printing the assertion failure.
    ///
    pub def handleWithStdErr(f: a -> b \ ef): (a -> b \ (ef - Assert) + IO + Sys) = x ->
        run {
            f(x)
        } with handler Assert {
            def assert(cond, msg, resume) = {
                if (cond)
                    resume(())
                else {
                    use Console.eprintln;
                    run {
                        eprintln(RichString.toAnsiString(msg))
                    } with Console.runWithIO;
                    resume(())
                }
            }
        }

    ///
    /// Handles the `Assert` effect by prompting the user whether to continue execution.
    ///
    /// When an assertion fails, prints the assertion failure and asks the user if they want to continue.
    /// If the user chooses not to continue, the program throws an exception.
    ///
    pub def handleWithPrompt(f: a -> b \ ef): (a -> b \ (ef - Assert) + IO + Sys) = x ->
        run {
            f(x)
        } with handler Assert {
            def assert(cond, msg, resume) = {
                if (cond)
                    resume(())
                else {
                    use Console.{print, readln};
                    run {
                        println("");
                        println(RichString.toAnsiString(msg));
                        println("");
                        print(RichString.toAnsiString(yellow("Continue execution? (Y/n): ")));
                        let input = readln();
                        let trimmed = String.trim(input);
                        if (String.toLowerCase(trimmed) == "n" or String.toLowerCase(trimmed) == "no")
                            throw new AssertionError("Execution aborted by user after the following assertion failure: ${RichString.toString(msg)}")
                        else
                            resume(())
                    } with Console.runWithIO
                }
            }
        }

    ///
    /// Runs the `Assert` effect of the given function `f` by throwing exceptions on assertion failures.
    ///
    /// In other words, re-interprets the `Assert` effect using the `IO` effect.
    ///
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - Assert) + IO + Sys = handle(f)()

}
