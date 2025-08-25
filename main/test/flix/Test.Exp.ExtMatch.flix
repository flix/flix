mod Test.Exp.ExtMatch {

    ///////////////////////////////////////////////////////////////////////////
    /// `ematch` should support matching variants with different types.
    ///////////////////////////////////////////////////////////////////////////

    @Test
    def testExtMatch01BigInt(): Bool = {
        ematch xvar A(1ii) {
            case A(x) => x == 1ii
        }
    }

    @Test
    def testExtMatch02BigInt(): Bool = {
        ematch xvar A(1ii) {
            case A(x) => x == 1ii
            case B(_) => false
        }
    }

    @Test
    def testExtMatch03BigInt(): Bool = {
        ematch xvar B(1ii) {
            case B(x) => x == 1ii
        }
    }

    @Test
    def testExtMatch04BigInt(): Bool = {
        ematch xvar B(1ii) {
            case A(_) => false
            case B(_) => true
        }
    }

    @Test
    def testExtMatch01Bool(): Bool = {
        ematch xvar A(true) {
            case A(x) => x
        }
    }

    @Test
    def testExtMatch02Bool(): Bool = {
        ematch xvar A(true) {
            case A(x) => x
            case B(_) => false
        }
    }

    @Test
    def testExtMatch03Bool(): Bool = {
        ematch xvar B(true) {
            case B(x) => x
        }
    }

    @Test
    def testExtMatch04Bool(): Bool = {
        ematch xvar B(false) {
            case A(_) => false
            case B(_) => true
        }
    }

    @Test
    def testExtMatch01Char(): Bool = {
        ematch xvar A('a') {
            case A(x) => x == 'a'
        }
    }

    @Test
    def testExtMatch02Char(): Bool = {
        ematch xvar A('a') {
            case A(x) => x == 'a'
            case B(_) => false
        }
    }

    @Test
    def testExtMatch03Char(): Bool = {
        ematch xvar B('a') {
            case B(x) => x == 'a'
        }
    }

    @Test
    def testExtMatch04Char(): Bool = {
        ematch xvar B('a') {
            case A(_) => false
            case B(_) => true
        }
    }

    @Test
    def testExtMatch01Float32(): Bool = {
        ematch xvar A(1.0f32) {
            case A(x) => x == 1.0f32
        }
    }

    @Test
    def testExtMatch02Float32(): Bool = {
        ematch xvar A(1.0f32) {
            case A(x) => x == 1.0f32
            case B(_) => false
        }
    }

    @Test
    def testExtMatch03Float32(): Bool = {
        ematch xvar B(1.0f32) {
            case B(x) => x == 1.0f32
        }
    }

    @Test
    def testExtMatch04Float32(): Bool = {
        ematch xvar B(1.0f32) {
            case A(_) => false
            case B(_) => true
        }
    }

    @Test
    def testExtMatch01Float64(): Bool = {
        ematch xvar A(1.0f64) {
            case A(x) => x == 1.0f64
        }
    }

    @Test
    def testExtMatch02Float64(): Bool = {
        ematch xvar A(1.0f64) {
            case A(x) => x == 1.0f64
            case B(_) => false
        }
    }

    @Test
    def testExtMatch03Float64(): Bool = {
        ematch xvar B(1.0f64) {
            case B(x) => x == 1.0f64
        }
    }

    @Test
    def testExtMatch04Float64(): Bool = {
        ematch xvar B(1.0f64) {
            case A(_) => false
            case B(_) => true
        }
    }

    @Test
    def testExtMatch01Int8(): Bool = {
        ematch xvar A(1i8) {
            case A(x) => x == 1i8
        }
    }

    @Test
    def testExtMatch02Int8(): Bool = {
        ematch xvar A(1i8) {
            case A(x) => x == 1i8
            case B(_) => false
        }
    }

    @Test
    def testExtMatch03Int8(): Bool = {
        ematch xvar B(1i8) {
            case B(x) => x == 1i8
        }
    }

    @Test
    def testExtMatch04Int8(): Bool = {
        ematch xvar B(1i8) {
            case A(_) => false
            case B(_) => true
        }
    }

    @Test
    def testExtMatch01Int16(): Bool = {
        ematch xvar A(1i16) {
            case A(x) => x == 1i16
        }
    }

    @Test
    def testExtMatch02Int16(): Bool = {
        ematch xvar A(1i16) {
            case A(x) => x == 1i16
            case B(_) => false
        }
    }

    @Test
    def testExtMatch03Int16(): Bool = {
        ematch xvar B(1i16) {
            case B(x) => x == 1i16
        }
    }

    @Test
    def testExtMatch04Int16(): Bool = {
        ematch xvar B(1i16) {
            case A(_) => false
            case B(_) => true
        }
    }

    @Test
    def testExtMatch01Int32(): Bool = {
        ematch xvar A(1) {
            case A(x) => x == 1
        }
    }

    @Test
    def testExtMatch02Int32(): Bool = {
        ematch xvar A(1) {
            case A(x) => x == 1
            case B(_) => false
        }
    }

    @Test
    def testExtMatch03Int32(): Bool = {
        ematch xvar B(1) {
            case B(x) => x == 1
        }
    }

    @Test
    def testExtMatch04Int32(): Bool = {
        ematch xvar B(1) {
            case A(_) => false
            case B(_) => true
        }
    }

    @Test
    def testExtMatch01Int64(): Bool = {
        ematch xvar A(1i64) {
            case A(x) => x == 1i64
        }
    }

    @Test
    def testExtMatch02Int64(): Bool = {
        ematch xvar A(1i64) {
            case A(x) => x == 1i64
            case B(_) => false
        }
    }

    @Test
    def testExtMatch03Int64(): Bool = {
        ematch xvar B(1i64) {
            case B(x) => x == 1i64
        }
    }

    @Test
    def testExtMatch04Int64(): Bool = {
        ematch xvar B(1i64) {
            case A(_) => false
            case B(_) => true
        }
    }

    @Test
    def testExtMatch01String(): Bool = {
        ematch xvar A("a") {
            case A(x) => x == "a"
        }
    }

    @Test
    def testExtMatch02String(): Bool = {
        ematch xvar A("a") {
            case A(x) => x == "a"
            case B(_) => false
        }
    }

    @Test
    def testExtMatch03String(): Bool = {
        ematch xvar B("a") {
            case B(x) => x == "a"
        }
    }

    @Test
    def testExtMatch04String(): Bool = {
        ematch xvar B("a") {
            case A(_) => false
            case B(_) => true
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    /// `ematch` should support matching on variants with different arities.
    ///////////////////////////////////////////////////////////////////////////

    @Test
    def testExtMatchArities01(): Bool = {
        ematch xvar A(42) {
            case A(x) => x == 42
            case B    => false
        }
    }

    @Test
    def testExtMatchArities02(): Bool = {
        ematch xvar B("test", 1) {
            case A(_)    => false
            case B(x, y) => x == "test" and y == 1
            case C       => false
        }
    }

    @Test
    def testExtMatchArities03(): Bool = {
        ematch xvar A(1.0f32, 'x', true) {
            case A(x, y, z) => x == 1.0f32 and y == 'x' and z == true
            case B(_, _)    => false
            case C(_)       => false
        }
    }

    @Test
    def testExtMatchArities04(): Bool = {
        ematch xvar B(42, "test", 3.14f64, 'a') {
            case A(_)          => false
            case B(w, x, y, z) => w == 42 and x == "test" and y == 3.14f64 and z == 'a'
            case C(_, _, _)    => false
            case D             => false
        }
    }

    @Test
    def testExtMatchArities05(): Bool = {
        ematch xvar C(1i8, 2i16, 3i32, 4i64, 5ii) {
            case A(_)             => false
            case B(_, _, _, _)    => false
            case C(a, b, c, d, e) => a == 1i8 and b == 2i16 and c == 3i32 and d == 4i64 and e == 5ii
            case D(_)             => false
            case E                => false
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    /// `ematch` should support matching on nullary variants.
    ///////////////////////////////////////////////////////////////////////////

    @Test
    def testExtMatchNullary01(): Bool = {
        ematch xvar A {
            case A => true
        }
    }

    @Test
    def testExtMatchNullary02(): Bool = {
        ematch xvar B {
            case A => false
            case B => true
        }
    }

    @Test
    def testExtMatchNullary03(): Bool = {
        ematch xvar C {
            case A => false
            case B => false
            case C => true
        }
    }

    @Test
    def testExtMatchNullary04(): Bool = {
        ematch xvar A {
            case B => false
            case C => false
            case A => true
        }
    }

    @Test
    def testExtMatchNullary05(): Bool = {
        ematch xvar B {
            case A => false
            case B => true
            case C => false
        }
    }

    ///////////////////////////////////////////////////////////////////////////
    /// `ematch` should support matching on `Unit` variants.
    ///////////////////////////////////////////////////////////////////////////

    @Test
    def testExtMatchUnit01(): Bool = {
        ematch xvar A(()) {
            case A(x) => x == ()
        }
    }

    @Test
    def testExtMatchUnit02(): Bool = {
        ematch xvar A(()) {
            case A(x) => x == ()
            case B(_) => false
        }
    }

    @Test
    def testExtMatchUnit03(): Bool = {
        ematch xvar B((), 42) {
            case A(_)    => false
            case B(x, y) => x == () and y == 42
        }
    }

    @Test
    def testExtMatchUnit04(): Bool = {
        ematch xvar A(42, (), "test") {
            case A(x, y, z) => x == 42 and y == () and z == "test"
            case B(_, _)    => false
        }
    }

    @Test
    def testExtMatchUnit05(): Bool = {
        ematch xvar Q() {
            case Q() => true
        }
    }

    @Test
    def testExtMatchUnit06(): Bool = {
        ematch xvar Q() {
            case Q(u) => u == ()
        }
    }

    @Test
    def testExtMatchUnit07(): Bool = {
        ematch xvar Q(()) {
            case Q() => true
        }
    }

    @Test
    def testExtMatchUnit08(): Bool = {
        ematch xvar A((), ()) {
            case A(x, y) => x == () and y == ()
        }
    }


    ///////////////////////////////////////////////////////////////////////////
    /// `ematch` should support matching on default patterns.
    ///////////////////////////////////////////////////////////////////////////

    @Test
    def testExtMatchDefault01(): Bool = {
        ematch xvar A(1) {
            case _ => true
        }
    }

    @Test
    def testExtMatchDefault02(): Bool = {
        ematch xvar A(1) {
            case A(x) => x == 1
            case _ => false
        }
    }

    @Test
    def testExtMatchDefault03(): Bool = {
        ematch xvar A(1) {
            case B(_) => false
            case _ => true
        }
    }


    ///////////////////////////////////////////////////////////////////////////
    /// Unsorted.
    ///////////////////////////////////////////////////////////////////////////

    @Test
    def testExtMatchDatalog01(): Bool = {
        let p = #{
            A(1).
        };
        let v = pquery p select A(1) with {A};
        ematch Vector.get(0, v) {
            case A(x) => x == 1
        }
    }

    @Test
    def testExtMatchMultipleScrutinees01(): Bool = {
        let scrutinee = if (true) xvar A(1) else xvar B(1);
        ematch scrutinee {
            case A(x) => x == 1
            case B(_) => false
        }
    }

    @Test
    def testExtMatchMultipleScrutinees02(): Bool = {
        let scrutinee = if (true) xvar A(1) else if (true) xvar B(2) else xvar C(false);
        ematch scrutinee {
            case A(x) => x == 1
            case B(_) => false
            case C(x) => x
        }
    }

    @Test
    def testExtMatchMultipleScrutinees03(): Bool = {
        let scrutinee = if (true) xvar A(1) else if (true) xvar B(2) else xvar C(false);
        ematch scrutinee {
            case B(_) => false
            case C(x) => x
            case A(x) => x == 1
        }
    }

    @Test
    def testExtMatchMultipleScrutinees04(): Bool = {
        let scrutinee = if (true) xvar A(1) else if (true) xvar B(2) else xvar C(false);
        ematch scrutinee {
            case B(_) => false
            case _ => true
        }
    }

    @Test
    def testExtMatchCall01(): Bool = {
        ematch generatesA() {
            case A(i) => i == 1
        }
    }

    @Test
    def testExtMatchCall02(): Bool = {
        ematch generatesAOpen() {
            case B(i) => i == 1
            case A(i) => i == 1
        }
    }

    @Test
    def testExtMatchCall03(): Bool = {
        matchesA(xvar A(1))
    }

    @Test
    def testExtMatchCall04(): Bool = {
        not matchesA(xvar B(1))
    }

    @Test
    def testExtMatchCall05(): Bool = {
        not matchesAOpen(xvar A(1))
    }

    @Test
    def testExtMatchCall06(): Bool = {
        not matchesAOpen(xvar C(1))
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
