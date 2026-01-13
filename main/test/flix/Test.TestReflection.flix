mod Test.TestReflection {
    use TestReflection.{UnitTest, getUnitTestName, isUnitTestPure};
    use Assert.{assertEq, assertTrue, assertFalse, assertMemberOf, assertEmpty};

    ///
    /// Test module with simple tests
    ///
    mod TestModule01 {
        @Test
        def test01(): Unit \ Assert = Assert.assertTrue(true)

        @Test
        def test02(): Unit \ Assert = Assert.assertEq(expected = 42, 42)
    }

    ///
    /// Test module with submodule
    ///
    mod TestModule02 {
        @Test
        def parentTest01(): Unit \ Assert = Assert.assertTrue(true)

        @Test
        def parentTest02(): Unit \ Assert = Assert.assertEq(expected = 1, 1)

        mod Sub {
            @Test
            def subTest01(): Unit \ Assert = Assert.assertTrue(true)

            @Test
            def subTest02(): Unit \ Assert = Assert.assertEq(expected = 2, 2)

            @Test
            def subTest03(): Unit \ Assert = Assert.assertFalse(false)
        }
    }

    ///
    /// Test modules for pure/IO testing
    ///
    mod TestModule03 {
        @Test
        def pureTest(): Unit \ Assert = Assert.assertTrue(true)
    }
    mod TestModule04 {
        @Test
        def ioTest(): Unit \ Assert + IO = {
            println("This test uses IO");
            Assert.assertTrue(true)
        }
    }

    ///
    /// Test module with orphan submodules
    ///
    mod TestModule05 {
        @Test
        def rootTest(): Unit \ Assert = Assert.assertTrue(true)

        mod Sub.Nested {
            @Test
            def nestedTest(): Unit \ Assert = Assert.assertTrue(true)
        }
    }

    ///
    /// Empty test module
    ///
    mod TestModule06 {
        // No tests
        def _helper(): Int32 = 42
    }

    ///
    /// Test: Verify that test names are correctly retrieved
    ///
    @Test
    def testGetTestNames01(): Unit \ Assert = {
        let tests = TestModule01.getTests();
        let names = Vector.map(getUnitTestName, tests);

        assertEq(expected = 2, Vector.length(names));
        assertMemberOf("Test.TestReflection.TestModule01.test01", names);
        assertMemberOf("Test.TestReflection.TestModule01.test02", names)
    }

    ///
    /// Test: Verify that submodule tests are included
    ///
    @Test
    def testGetTestsIncludesSubmodules01(): Unit \ Assert = {
        let tests = TestModule02.getTests();
        let names = Vector.map(getUnitTestName, tests);

        // Should have 2 tests from TestModule02 + 3 tests from TestModule02.Sub
        assertEq(expected = 5, Vector.length(names));
        assertMemberOf("Test.TestReflection.TestModule02.parentTest01", names);
        assertMemberOf("Test.TestReflection.TestModule02.parentTest02", names);
        assertMemberOf("Test.TestReflection.TestModule02.Sub.subTest01", names);
        assertMemberOf("Test.TestReflection.TestModule02.Sub.subTest02", names);
        assertMemberOf("Test.TestReflection.TestModule02.Sub.subTest03", names)
    }

    ///
    /// Test: Verify that submodule getTests returns only its own tests
    ///
    @Test
    def testGetTestsSubmoduleOnly01(): Unit \ Assert = {
        let tests = TestModule02.Sub.getTests();
        let names = Vector.map(getUnitTestName, tests);

        assertEq(expected = 3, Vector.length(names));
        assertMemberOf("Test.TestReflection.TestModule02.Sub.subTest01", names);
        assertMemberOf("Test.TestReflection.TestModule02.Sub.subTest02", names);
        assertMemberOf("Test.TestReflection.TestModule02.Sub.subTest03", names)
    }

    ///
    /// Test: Verify pure tests are correctly identified
    ///
    @Test
    def testGetTestsPureTests01(): Unit \ Assert = {
        let pureTests = TestModule03.getTests();
        assertEq(expected = 1, Vector.length(pureTests));
        let pureTest = Vector.get(0, pureTests);
        assertTrue(isUnitTestPure(pureTest))
    }

    ///
    /// Test: Verify IO tests are correctly identified
    ///
    @Test
    def testGetTestsIOTests01(): Unit \ Assert = {
        let iOTests = TestModule04.getTests();
        assertEq(expected = 1, Vector.length(iOTests));
        let iOTest = Vector.get(0, iOTests);
        assertFalse(isUnitTestPure(iOTest))
    }

    ///
    /// Test: Verify nested submodules are included
    ///
    @Test
    def testGetTestsNestedSubmodules01(): Unit \ Assert = {
        let tests = TestModule05.getTests();
        let names = Vector.map(getUnitTestName, tests);

        // Should include tests from TestModule05 and TestModule05.Sub.Nested
        assertEq(expected = 2, Vector.length(names));
        assertMemberOf("Test.TestReflection.TestModule05.rootTest", names);
        assertMemberOf("Test.TestReflection.TestModule05.Sub.Nested.nestedTest", names)
    }

    ///
    /// Test: Verify empty module returns empty vector
    ///
    @Test
    def testGetTestsEmptyModule01(): Unit \ Assert = {
        let tests = TestModule06.getTests();

        assertEmpty(tests)
    }
}
