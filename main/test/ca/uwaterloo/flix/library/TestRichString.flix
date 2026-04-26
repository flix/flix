mod TestRichString {
    use Assert.{assertEq, assertNeq}
    
    @Test
    def testEquals01(): Unit \ Assert = 
        let testRc = RichString.fromString("hello") |> RichString.bgWhite |> RichString.bold;
        let testRc1 = RichString.fromString("hel") |> RichString.bgWhite |> RichString.bold;
        let testRc2 = RichString.fromString("lo") |> RichString.bgWhite |> RichString.bold;
        let testRcFinal = testRc1 + testRc2;
        assertEq(expected = testRc, testRcFinal)

    @Test
    def testEquals02(): Unit \ Assert = 
        let testRc = RichString.fromString("hello") |> RichString.bgWhite |> RichString.bold;
        let testRc1 = RichString.fromString("hel") |> RichString.bgWhite |> RichString.underline;
        let testRc2 = RichString.fromString("lo") |> RichString.bgWhite |> RichString.bold;
        let testRcFinal = testRc1 + testRc2;
        assertNeq(unexpected = testRc, testRcFinal)

    @Test
    def testEquals03(): Unit \ Assert = 
        let testRc = RichString.fromString("hello") |> RichString.bgWhite |> RichString.bold |> RichString.black;
        let testSpan1 = RichString.Span.Span({ bgColor = RichString.Color.Rgb(255, 255, 255), fgColor = RichString.Color.Rgb(0, 0, 0), styles = Set#{RichString.Style.Bold}, text = "hel" });
        let testSpan2 = RichString.Span.Span({ bgColor = RichString.Color.Rgb(255, 255, 255), fgColor = RichString.Color.Rgb(0, 0, 0), styles = Set#{RichString.Style.Bold}, text = "lo" });
        let testRc2 = RichString.RichString(Chain.append(Chain.singleton(testSpan1), Chain.singleton(testSpan2)));
        assertEq(expected = testRc, testRc2)

    @Test
    def testCompare01(): Unit \ Assert =
        let rs1 = RichString.fromString("a") |> RichString.bgWhite |> RichString.bold |> RichString.black;
        let rs2 = RichString.fromString("a") |> RichString.bgWhite |> RichString.bold |> RichString.black;
        assertEq(expected = Comparison.EqualTo, rs1 <=> rs2)

    @Test
    def testCompare02(): Unit \ Assert =
        let rs1 = RichString.fromString("a") |> RichString.bgWhite |> RichString.bold |> RichString.black;
        let rs2 = RichString.fromString("b") |> RichString.bgWhite |> RichString.bold |> RichString.black;
        assertEq(expected = Comparison.LessThan, rs1 <=> rs2)

    @Test
    def testCompare03(): Unit \ Assert =
        let rs1 = RichString.fromString("b") |> RichString.bgWhite |> RichString.bold |> RichString.black;
        let rs2 = RichString.fromString("a") |> RichString.bgWhite |> RichString.bold |> RichString.black;
        assertEq(expected = Comparison.GreaterThan, rs1 <=> rs2)

    @Test
    def testCompare04(): Unit \ Assert =
        let rs1 = RichString.fromString("a") |> RichString.bgWhite |> RichString.bold |> RichString.black;
        let rs2 = RichString.fromString("a") |> RichString.bgBlack |> RichString.bold |> RichString.black;
        assertEq(expected = Comparison.GreaterThan, rs1 <=> rs2)

    @Test
    def testCompare05(): Unit \ Assert =
        let rs1 = RichString.fromString("a") |> RichString.bgWhite |> RichString.bold |> RichString.black;
        let rs2 = RichString.fromString("a") |> RichString.bgWhite |> RichString.underline |> RichString.black;
        assertEq(expected = Comparison.LessThan, rs1 <=> rs2)

    @Test
    def testCompare06(): Unit \ Assert =
        let rs1 = RichString.fromString("a") |> RichString.bgWhite |> RichString.bold |> RichString.white;
        let rs2 = RichString.fromString("a") |> RichString.bgWhite |> RichString.bold |> RichString.black;
        assertEq(expected = Comparison.GreaterThan, rs1 <=> rs2)

    @Test
    def testCompare07(): Unit \ Assert =
        let rs1 = RichString.fromString("aaaa") |> RichString.bgWhite |> RichString.bold |> RichString.white;
        let rs2 = RichString.fromString("a") |> RichString.bgWhite |> RichString.bold |> RichString.black;
        assertEq(expected = Comparison.GreaterThan, rs1 <=> rs2)

}