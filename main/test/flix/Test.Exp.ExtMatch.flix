mod Test.Exp.ExtMatch {

    use Assert.{assertEq, assertTrue, success, fail};

    ///////////////////////////////////////////////////////////////////////////
    /// `ematch` should support matching variants with different types.
    ///////////////////////////////////////////////////////////////////////////

    @Test
    def testExtMatch01BigInt(): Unit \ Assert = {
        ematch xvar A(1ii) {
            case A(x) => assertEq(expected = 1ii, x)
        }
    }

    @Test
    def testExtMatch02BigInt(): Unit \ Assert = {
        ematch xvar A(1ii) {
            case A(x) => assertEq(expected = 1ii, x)
            case B(_) => fail("unexpected B")
        }
    }

    @Test
    def testExtMatch03BigInt(): Unit \ Assert = {
        ematch xvar B(1ii) {
            case B(x) => assertEq(expected = 1ii, x)
        }
    }

    @Test
    def testExtMatch04BigInt(): Unit \ Assert = {
        ematch xvar B(1ii) {
            case A(_) => fail("unexpected A")
            case B(_) => success("matched B")
        }
    }

    @Test
    def testExtMatch01Bool(): Unit \ Assert = {
        ematch xvar A(true) {
            case A(x) => assertTrue(x)
        }
    }

    @Test
    def testExtMatch02Bool(): Unit \ Assert = {
        ematch xvar A(true) {
            case A(x) => assertTrue(x)
            case B(_) => fail("unexpected B")
        }
    }

    @Test
    def testExtMatch03Bool(): Unit \ Assert = {
        ematch xvar B(true) {
            case B(x) => assertTrue(x)
        }
    }

    @Test
    def testExtMatch04Bool(): Unit \ Assert = {
        ematch xvar B(false) {
            case A(_) => fail("unexpected A")
            case B(_) => success("matched B")
        }
    }

    @Test
    def testExtMatch01Char(): Unit \ Assert = {
        ematch xvar A('a') {
            case A(x) => assertEq(expected = 'a', x)
        }
    }

    @Test
    def testExtMatch02Char(): Unit \ Assert = {
        ematch xvar A('a') {
            case A(x) => assertEq(expected = 'a', x)
            case B(_) => fail("unexpected B")
        }
    }

    @Test
    def testExtMatch03Char(): Unit \ Assert = {
        ematch xvar B('a') {
            case B(x) => assertEq(expected = 'a', x)
        }
    }

    @Test
    def testExtMatch04Char(): Unit \ Assert = {
        ematch xvar B('a') {
            case A(_) => fail("unexpected A")
            case B(_) => success("matched B")
        }
    }

    @Test
    def testExtMatch01Float32(): Unit \ Assert = {
        ematch xvar A(1.0f32) {
            case A(x) => assertEq(expected = 1.0f32, x)
        }
    }

    @Test
    def testExtMatch02Float32(): Unit \ Assert = {
        ematch xvar A(1.0f32) {
            case A(x) => assertEq(expected = 1.0f32, x)
            case B(_) => fail("unexpected B")
        }
    }

    @Test
    def testExtMatch03Float32(): Unit \ Assert = {
        ematch xvar B(1.0f32) {
            case B(x) => assertEq(expected = 1.0f32, x)
        }
    }

    @Test
    def testExtMatch04Float32(): Unit \ Assert = {
        ematch xvar B(1.0f32) {
            case A(_) => fail("unexpected A")
            case B(_) => success("matched B")
        }
    }

    @Test
    def testExtMatch01Float64(): Unit \ Assert = {
        ematch xvar A(1.0f64) {
            case A(x) => assertEq(expected = 1.0f64, x)
        }
    }

    @Test
    def testExtMatch02Float64(): Unit \ Assert = {
        ematch xvar A(1.0f64) {
            case A(x) => assertEq(expected = 1.0f64, x)
            case B(_) => fail("unexpected B")
        }
    }

    @Test
    def testExtMatch03Float64(): Unit \ Assert = {
        ematch xvar B(1.0f64) {
            case B(x) => assertEq(expected = 1.0f64, x)
        }
    }

    @Test
    def testExtMatch04Float64(): Unit \ Assert = {
        ematch xvar B(1.0f64) {
            case A(_) => fail("unexpected A")
            case B(_) => success("matched B")
        }
    }

    @Test
    def testExtMatch01Int8(): Unit \ Assert = {
        ematch xvar A(1i8) {
            case A(x) => assertEq(expected = 1i8, x)
        }
    }

    @Test
    def testExtMatch02Int8(): Unit \ Assert = {
        ematch xvar A(1i8) {
            case A(x) => assertEq(expected = 1i8, x)
            case B(_) => fail("unexpected B")
        }
    }

    @Test
    def testExtMatch03Int8(): Unit \ Assert = {
        ematch xvar B(1i8) {
            case B(x) => assertEq(expected = 1i8, x)
        }
    }

    @Test
    def testExtMatch04Int8(): Unit \ Assert = {
        ematch xvar B(1i8) {
            case A(_) => fail("unexpected A")
            case B(_) => success("matched B")
        }
    }

    @Test
    def testExtMatch01Int16(): Unit \ Assert = {
        ematch xvar A(1i16) {
            case A(x) => assertEq(expected = 1i16, x)
        }
    }

    @Test
    def testExtMatch02Int16(): Unit \ Assert = {
        ematch xvar A(1i16) {
            case A(x) => assertEq(expected = 1i16, x)
            case B(_) => fail("unexpected B")
        }
    }

    @Test
    def testExtMatch03Int16(): Unit \ Assert = {
        ematch xvar B(1i16) {
            case B(x) => assertEq(expected = 1i16, x)
        }
    }

    @Test
    def testExtMatch04Int16(): Unit \ Assert = {
        ematch xvar B(1i16) {
            case A(_) => fail("unexpected A")
            case B(_) => success("matched B")
        }
    }

    @Test
    def testExtMatch01Int32(): Unit \ Assert = {
        ematch xvar A(1) {
            case A(x) => assertEq(expected = 1, x)
        }
    }

    @Test
    def testExtMatch02Int32(): Unit \ Assert = {
        ematch xvar A(1) {
            case A(x) => assertEq(expected = 1, x)
            case B(_) => fail("unexpected B")
        }
    }

    @Test
    def testExtMatch03Int32(): Unit \ Assert = {
        ematch xvar B(1) {
            case B(x) => assertEq(expected = 1, x)
        }
    }

    @Test
    def testExtMatch04Int32(): Unit \ Assert = {
        ematch xvar B(1) {
            case A(_) => fail("unexpected A")
            case B(_) => success("matched B")
        }
    }

    @Test
    def testExtMatch01Int64(): Unit \ Assert = {
        ematch xvar A(1i64) {
            case A(x) => assertEq(expected = 1i64, x)
        }
    }

    @Test
    def testExtMatch02Int64(): Unit \ Assert = {
        ematch xvar A(1i64) {
            case A(x) => assertEq(expected = 1i64, x)
            case B(_) => fail("unexpected B")
        }
    }

    @Test
    def testExtMatch03Int64(): Unit \ Assert = {
        ematch xvar B(1i64) {
            case B(x) => assertEq(expected = 1i64, x)
        }
    }

    @Test
    def testExtMatch04Int64(): Unit \ Assert = {
        ematch xvar B(1i64) {
            case A(_) => fail("unexpected A")
            case B(_) => success("matched B")
        }
    }

    @Test
    def testExtMatch01String(): Unit \ Assert = {
        ematch xvar A("a") {
            case A(x) => assertEq(expected = "a", x)
        }
    }

    @Test
    def testExtMatch02String(): Unit \ Assert = {
        ematch xvar A("a") {
            case A(x) => assertEq(expected = "a", x)
            case B(_) => fail("unexpected B")
        }
    }

    @Test
    def testExtMatch03String(): Unit \ Assert = {
        ematch xvar B("a") {
            case B(x) => assertEq(expected = "a", x)
        }
    }

    @Test
    def testExtMatch04String(): Unit \ Assert = {
        ematch xvar B("a") {
            case A(_) => fail("unexpected A")
            case B(_) => success("matched B")
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    /// `ematch` should support matching on variants with different arities.
    ///////////////////////////////////////////////////////////////////////////

    @Test
    def testExtMatchArities01(): Unit \ Assert = {
        ematch xvar A(42) {
            case A(x) => assertEq(expected = 42, x)
            case B    => fail("unexpected B")
        }
    }

    @Test
    def testExtMatchArities02(): Unit \ Assert = {
        ematch xvar B("test", 1) {
            case A(_)    => fail("unexpected A")
            case B(x, y) => { assertEq(expected = "test", x); assertEq(expected = 1, y) }
            case C       => fail("unexpected C")
        }
    }

    @Test
    def testExtMatchArities03(): Unit \ Assert = {
        ematch xvar A(1.0f32, 'x', true) {
            case A(x, y, z) => { assertEq(expected = 1.0f32, x); assertEq(expected = 'x', y); assertTrue(z) }
            case B(_, _)    => fail("unexpected B")
            case C(_)       => fail("unexpected C")
        }
    }

    @Test
    def testExtMatchArities04(): Unit \ Assert = {
        ematch xvar B(42, "test", 3.14f64, 'a') {
            case A(_)          => fail("unexpected A")
            case B(w, x, y, z) => { assertEq(expected = 42, w); assertEq(expected = "test", x); assertEq(expected = 3.14f64, y); assertEq(expected = 'a', z) }
            case C(_, _, _)    => fail("unexpected C")
            case D             => fail("unexpected D")
        }
    }

    @Test
    def testExtMatchArities05(): Unit \ Assert = {
        ematch xvar C(1i8, 2i16, 3i32, 4i64, 5ii) {
            case A(_)             => fail("unexpected A")
            case B(_, _, _, _)    => fail("unexpected B")
            case C(a, b, c, d, e) => { assertEq(expected = 1i8, a); assertEq(expected = 2i16, b); assertEq(expected = 3i32, c); assertEq(expected = 4i64, d); assertEq(expected = 5ii, e) }
            case D(_)             => fail("unexpected D")
            case E                => fail("unexpected E")
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    /// `ematch` should support matching on nullary variants.
    ///////////////////////////////////////////////////////////////////////////

    @Test
    def testExtMatchNullary01(): Unit \ Assert = {
        ematch xvar A {
            case A => success("matched A")
        }
    }

    @Test
    def testExtMatchNullary02(): Unit \ Assert = {
        ematch xvar B {
            case A => fail("unexpected A")
            case B => success("matched B")
        }
    }

    @Test
    def testExtMatchNullary03(): Unit \ Assert = {
        ematch xvar C {
            case A => fail("unexpected A")
            case B => fail("unexpected B")
            case C => success("matched C")
        }
    }

    @Test
    def testExtMatchNullary04(): Unit \ Assert = {
        ematch xvar A {
            case B => fail("unexpected B")
            case C => fail("unexpected C")
            case A => success("matched A")
        }
    }

    @Test
    def testExtMatchNullary05(): Unit \ Assert = {
        ematch xvar B {
            case A => fail("unexpected A")
            case B => success("matched B")
            case C => fail("unexpected C")
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    /// `ematch` should support matching on `Unit` variants.
    ///////////////////////////////////////////////////////////////////////////

    @Test
    def testExtMatchUnit01(): Unit \ Assert = {
        ematch xvar A(()) {
            case A(x) => assertEq(expected = (), x)
        }
    }

    @Test
    def testExtMatchUnit02(): Unit \ Assert = {
        ematch xvar A(()) {
            case A(x) => assertEq(expected = (), x)
            case B(_) => fail("unexpected B")
        }
    }

    @Test
    def testExtMatchUnit03(): Unit \ Assert = {
        ematch xvar B((), 42) {
            case A(_)    => fail("unexpected A")
            case B(x, y) => { assertEq(expected = (), x); assertEq(expected = 42, y) }
        }
    }

    @Test
    def testExtMatchUnit04(): Unit \ Assert = {
        ematch xvar A(42, (), "test") {
            case A(x, y, z) => { assertEq(expected = 42, x); assertEq(expected = (), y); assertEq(expected = "test", z) }
            case B(_, _)    => fail("unexpected B")
        }
    }

    @Test
    def testExtMatchUnit05(): Unit \ Assert = {
        ematch xvar Q() {
            case Q() => success("matched Q")
        }
    }

    @Test
    def testExtMatchUnit06(): Unit \ Assert = {
        ematch xvar Q() {
            case Q(u) => assertEq(expected = (), u)
        }
    }

    @Test
    def testExtMatchUnit07(): Unit \ Assert = {
        ematch xvar Q(()) {
            case Q() => success("matched Q")
        }
    }

    @Test
    def testExtMatchUnit08(): Unit \ Assert = {
        ematch xvar A((), ()) {
            case A(x, y) => { assertEq(expected = (), x); assertEq(expected = (), y) }
        }
    }


    ///////////////////////////////////////////////////////////////////////////
    /// `ematch` should support matching on default patterns.
    ///////////////////////////////////////////////////////////////////////////

    @Test
    def testExtMatchDefault01(): Unit \ Assert = {
        ematch xvar A(1) {
            case _ => success("matched default")
        }
    }

    @Test
    def testExtMatchDefault02(): Unit \ Assert = {
        ematch xvar A(1) {
            case A(x) => assertEq(expected = 1, x)
            case _ => fail("unexpected default")
        }
    }

    @Test
    def testExtMatchDefault03(): Unit \ Assert = {
        ematch xvar A(1) {
            case B(_) => fail("unexpected B")
            case _ => success("matched default")
        }
    }


    ///////////////////////////////////////////////////////////////////////////
    /// Unsorted.
    ///////////////////////////////////////////////////////////////////////////

    @Test
    def testExtMatchDatalog01(): Unit \ Assert = {
        let p = #{
            A(1).
        };
        let v = pquery p select A(1) with {A};
        ematch Vector.get(0, v) {
            case A(x) => assertEq(expected = 1, x)
        }
    }

    @Test
    def testExtMatchMultipleScrutinees01(): Unit \ Assert = {
        let scrutinee = if (true) xvar A(1) else xvar B(1);
        ematch scrutinee {
            case A(x) => assertEq(expected = 1, x)
            case B(_) => fail("unexpected B")
        }
    }

    @Test
    def testExtMatchMultipleScrutinees02(): Unit \ Assert = {
        let scrutinee = if (true) xvar A(1) else if (true) xvar B(2) else xvar C(false);
        ematch scrutinee {
            case A(x) => assertEq(expected = 1, x)
            case B(_) => fail("unexpected B")
            case C(x) => assertTrue(x)
        }
    }

    @Test
    def testExtMatchMultipleScrutinees03(): Unit \ Assert = {
        let scrutinee = if (true) xvar A(1) else if (true) xvar B(2) else xvar C(false);
        ematch scrutinee {
            case B(_) => fail("unexpected B")
            case C(x) => assertTrue(x)
            case A(x) => assertEq(expected = 1, x)
        }
    }

    @Test
    def testExtMatchMultipleScrutinees04(): Unit \ Assert = {
        let scrutinee = if (true) xvar A(1) else if (true) xvar B(2) else xvar C(false);
        ematch scrutinee {
            case B(_) => fail("unexpected B")
            case _ => success("matched default")
        }
    }

    @Test
    def testExtMatchCall01(): Unit \ Assert = {
        ematch generatesA() {
            case A(i) => assertEq(expected = 1, i)
        }
    }

    @Test
    def testExtMatchCall02(): Unit \ Assert = {
        ematch generatesAOpen() {
            case B(i) => assertEq(expected = 1, i)
            case A(i) => assertEq(expected = 1, i)
        }
    }

    @Test
    def testExtMatchCall03(): Unit \ Assert = {
        assertTrue(matchesA(xvar A(1)))
    }

    @Test
    def testExtMatchCall04(): Unit \ Assert = {
        assertTrue(not matchesA(xvar B(1)))
    }

    @Test
    def testExtMatchCall05(): Unit \ Assert = {
        assertTrue(matchesAOpen(xvar A(1)))
    }

    @Test
    def testExtMatchCall06(): Unit \ Assert = {
        assertTrue(not matchesAOpen(xvar C(1)))
    }

    def generatesA(): #| A(Int32) |# = xvar A(1)

    def generatesAOpen(): #| A(Int32) | r |# = xvar A(1)

    def matchesA(var: #| A(Int32), B(Int32) |#): Bool = ematch var {
        case A(_) => true
        case B(_) => false
    }

    def matchesAOpen(var: #| A(Int32), B(Int32) | r |#): Bool = ematch var {
        case A(_) => true
        case B(_) => false
        case _ => false
    }

}
