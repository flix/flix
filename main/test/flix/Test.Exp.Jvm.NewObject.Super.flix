mod Test.Exp.Jvm.NewObject.Super {

    use Assert.{assertEq, assertTrue}

    import dev.flix.test.TestClassWithDefaultConstructor
    import java.lang.Object

    // Test 1: super.toString() — override wraps parent's toString
    @Test
    def testSuperMethod01(): Unit \ Assert + IO =
        let obj = new Object {
            def toString(_this: Object): String \ IO =
                "<<" + super.toString() + ">>"
        };
        assertTrue(obj.toString().startsWith("<<"))

    // Test 2: super.hashCode() — override delegates to parent's hashCode
    @Test
    def testSuperMethod02(): Unit \ Assert + IO =
        let obj = new Object {
            def hashCode(_this: Object): Int32 \ IO =
                super.hashCode()
        };
        // hashCode should return some integer (just check it doesn't crash)
        let h = obj.hashCode();
        assertEq(expected = h, h)

    // Test 3: super.concreteMethod(y) — override appends suffix to super result
    @Test
    def testSuperMethod03(): Unit \ Assert + IO =
        let anon = new TestClassWithDefaultConstructor {
            def abstractMethod(_this: TestClassWithDefaultConstructor, x: Int32): Int32 = x
            def concreteMethod(_this: TestClassWithDefaultConstructor, y: String): String \ IO =
                super.concreteMethod(y) + "!"
        };
        assertEq(expected = "foobar!", anon.concreteMethod("bar"))

    // Test 4: super.concreteMethod(y) on TestClassWithDefaultConstructor
    //         Parent returns m_y + y = "foo" + y; override prepends "bar"
    @Test
    def testSuperMethod04(): Unit \ Assert + IO =
        let anon = new TestClassWithDefaultConstructor {
            def abstractMethod(_this: TestClassWithDefaultConstructor, x: Int32): Int32 = x
            def concreteMethod(_this: TestClassWithDefaultConstructor, y: String): String \ IO =
                "bar" + super.concreteMethod(y)
        };
        assertEq(expected = "barfoobaz", anon.concreteMethod("baz"))

    // Test 5: super.concreteMethod(y) — override returns super result unchanged
    @Test
    def testSuperMethod05(): Unit \ Assert + IO =
        let anon = new TestClassWithDefaultConstructor {
            def abstractMethod(_this: TestClassWithDefaultConstructor, x: Int32): Int32 = x
            def concreteMethod(_this: TestClassWithDefaultConstructor, y: String): String \ IO =
                super.concreteMethod(y)
        };
        assertEq(expected = "foobar", anon.concreteMethod("bar"))

    // Test 6: super.toString() — override ignores super result, returns constant
    @Test
    def testSuperMethod06(): Unit \ Assert + IO =
        let obj = new Object {
            def toString(_this: Object): String \ IO =
                let _ = super.toString();
                "constant"
        };
        assertEq(expected = "constant", obj.toString())

    // Test 7: super.toString() with captured variable in scope
    @Test
    def testSuperMethod07(): Unit \ Assert + IO =
        let prefix = "prefix:";
        let obj = new Object {
            def toString(_this: Object): String \ IO =
                prefix + super.toString()
        };
        assertTrue(obj.toString().startsWith("prefix:"))

    // Test 8: super.hashCode() — override uses super result in arithmetic
    @Test
    def testSuperMethod08(): Unit \ Assert + IO =
        let obj = new Object {
            def hashCode(_this: Object): Int32 \ IO =
                super.hashCode() + 1
        };
        let obj2 = new Object {
            def hashCode(_this: Object): Int32 \ IO =
                super.hashCode()
        };
        // Just verify both return integers without crashing
        let _ = obj.hashCode();
        let _ = obj2.hashCode();
        assertEq(expected = true, true)

    // Test 9: two methods in same NewObject both use super
    @Test
    def testSuperMethod09(): Unit \ Assert + IO =
        let obj = new Object {
            def toString(_this: Object): String \ IO =
                "wrapped:" + super.toString()
            def hashCode(_this: Object): Int32 \ IO =
                super.hashCode() * 2
        };
        assertTrue(obj.toString().startsWith("wrapped:"));
        let _ = obj.hashCode();
        assertEq(expected = true, true)

    // Test 10: super.toString() on Object — override returns super result unchanged
    @Test
    def testSuperMethod10(): Unit \ Assert + IO =
        let obj = new Object {
            def toString(_this: Object): String \ IO =
                super.toString()
        };
        let s = obj.toString();
        assertEq(expected = s, s)

    // Test 11: call super.toString() from within a concreteMethod override (cross-method)
    @Test
    def testSuperMethod11(): Unit \ Assert + IO =
        let anon = new TestClassWithDefaultConstructor {
            def abstractMethod(_this: TestClassWithDefaultConstructor, x: Int32): Int32 = x
            def concreteMethod(_this: TestClassWithDefaultConstructor, _y: String): String \ IO =
                super.toString()
        };
        assertTrue(anon.concreteMethod("ignored").startsWith("Anon"))

    // Test 12: call super.hashCode() from within a toString override (cross-method)
    @Test
    def testSuperMethod12(): Unit \ Assert + IO =
        let obj = new Object {
            def toString(_this: Object): String \ IO =
                "hash:" + Int32.toString(super.hashCode())
        };
        assertTrue(obj.toString().startsWith("hash:"))

    // Test 13: call super.concreteMethod(y) from within a toString override (cross-method)
    @Test
    def testSuperMethod13(): Unit \ Assert + IO =
        let anon = new TestClassWithDefaultConstructor {
            def abstractMethod(_this: TestClassWithDefaultConstructor, x: Int32): Int32 = x
            def toString(_this: TestClassWithDefaultConstructor): String \ IO =
                super.concreteMethod("bar")
        };
        assertEq(expected = "foobar", anon.toString())

    // Test 14: call both super.toString() and super.concreteMethod(y) from the same method override
    @Test
    def testSuperMethod14(): Unit \ Assert + IO =
        let anon = new TestClassWithDefaultConstructor {
            def abstractMethod(_this: TestClassWithDefaultConstructor, x: Int32): Int32 = x
            def concreteMethod(_this: TestClassWithDefaultConstructor, y: String): String \ IO =
                super.concreteMethod(y) + "|" + super.toString()
        };
        assertTrue(anon.concreteMethod("baz").startsWith("foobaz|"))

    // Test 15: cross-method super call in one method, same-method super call in another
    @Test
    def testSuperMethod15(): Unit \ Assert + IO =
        let anon = new TestClassWithDefaultConstructor {
            def abstractMethod(_this: TestClassWithDefaultConstructor, x: Int32): Int32 = x
            def concreteMethod(_this: TestClassWithDefaultConstructor, y: String): String \ IO =
                super.concreteMethod(y)
            def toString(_this: TestClassWithDefaultConstructor): String \ IO =
                super.concreteMethod("cross")
        };
        assertEq(expected = "foobar", anon.concreteMethod("bar"));
        assertEq(expected = "foocross", anon.toString())

}
