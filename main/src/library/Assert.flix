/*
 * Copyright 2022 Magnus Madsen
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
    /// The provided functions in the associated module treat this message as
    /// the reason of failure, but it does not necessarily have to be that.
    ///
    /// If the assertion fails, the behavior depends on the effect handler.
    ///
    def assert(cond: Bool, msg: String): Unit

}

mod Assert {
    import java.lang.AssertionError;

    ///
    /// Asserts that the given condition `cond` is `true`.
    ///
    pub def assertTrue(cond: Bool): Unit \ Assert =
        Assert.assert(cond, "Assertion failed: expected true")

    ///
    /// Asserts that the given condition `cond` is `true` with the given message `msg`.
    ///
    pub def assertTrueWithMsg(cond: Bool, msg: String): Unit \ Assert =
        Assert.assert(cond, msg)

    ///
    /// Asserts that the given condition `cond` is `false`.
    ///
    pub def assertFalse(cond: Bool): Unit \ Assert =
        Assert.assert(not cond, "Assertion failed: expected false")

    ///
    /// Asserts that the given condition `cond` is `false` with the given message `msg`.
    ///
    pub def assertFalseWithMsg(cond: Bool, msg: String): Unit \ Assert =
        Assert.assert(not cond, msg)

    ///
    /// Asserts that the given values `expected` and `actual` are equal.
    ///
    pub def assertEq(exp: {expected = a}, actual: a): Unit \ Assert with Eq[a], ToString[a] =
        Assert.assert(actual == exp#expected, "Assertion failed: expected `${exp#expected}`, but got `${actual}`")

    ///
    /// Asserts that the given values `expected` and `actual` are equal with the given message `msg`.
    ///
    pub def assertEqWithMsg(exp: {expected = a}, actual: a, msg: String): Unit \ Assert with Eq[a] =
        Assert.assert(actual == exp#expected, msg)

    ///
    /// Asserts that the given values `unexpected` and `actual` are not equal.
    ///
    pub def assertNeq(unexp: {unexpected = a}, actual: a): Unit \ Assert with Eq[a], ToString[a] =
        Assert.assert(actual != unexp#unexpected, "Assertion failed: expected values to be different, but both were `${actual}`")

    ///
    /// Asserts that the given values `unexpected` and `actual` are not equal with the given message `msg`.
    ///
    pub def assertNeqWithMsg(unexp: {unexpected = a}, actual: a, msg: String): Unit \ Assert with Eq[a] =
        Assert.assert(actual != unexp#unexpected, msg)

    ///
    /// Asserts that the given Option `o` is `Some(v)`.
    ///
    pub def assertSome(o: Option[a]): Unit \ Assert =
        Assert.assert(Option.nonEmpty(o), "Assertion failed: expected Some(_), but got None")

    ///
    /// Asserts that the given Option `o` is `None`.
    ///
    pub def assertNone(o: Option[a]): Unit \ Assert =
        Assert.assert(Option.isEmpty(o), "Assertion failed: expected None, but got Some(_)")

    ///
    /// Asserts that the given Result `r` is `Ok(v)`.
    ///
    pub def assertOk(r: Result[e, a]): Unit \ Assert =
        Assert.assert(Result.isOk(r), "Assertion failed: expected Ok(_), but got Err(_)")

    ///
    /// Asserts that the given Result `r` is `Err(e)`.
    ///
    pub def assertErr(r: Result[e, a]): Unit \ Assert =
        Assert.assert(Result.isErr(r), "Assertion failed: expected Err(_), but got Ok(_)")

    ///
    /// Unconditionally fails with the given message `msg`.
    ///
    pub def fail(msg: String): Unit \ Assert =
        Assert.assert(false, msg)

    ///
    /// Handles the `Assert` effect of the given function `f` by throwing exceptions on assertion failures.
    ///
    /// In other words, re-interprets the `Assert` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): (a -> b \ (ef - Assert) + IO + Sys) =
        x ->
        run {
            f(x)
        } with handler Assert {
            def assert(cond, msg, resume) =
            {
                if (cond)
                    resume(())
                else {
                    use Chalk.{red, bold};
                    run {
                        println("");
                        println("${red("Assertion Failed:")} ${bold(msg)}");
                        println("");
                        throw new AssertionError(msg)
                    } with Environment.runWithIO
                }
            }
        }

    ///
    /// Runs the `Assert` effect of the given function `f` by throwing exceptions on assertion failures.
    ///
    /// In other words, re-interprets the `Assert` effect using the `IO` effect.
    ///
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - Assert) + IO + Sys = handle(f)()

    ///
    /// Asserts that `expected` must equal `actual`.
    ///
    /// Aborts execution if the two values are un-equal.
    ///
    pub def eq(expected: a, actual: a): Bool with Eq[a], ToString[a] = unchecked_cast({
        if (expected == actual)
            true
        else {
            use Chalk.{green, red};
            Environment.runWithIO(() -> {
                println("Assertion Error");
                println("  Expected: ${green(expected)}");
                println("  Actual:   ${red(actual)}");
                println("");
                ?assertEq
            })
        }
    } as _ \ {})

}
