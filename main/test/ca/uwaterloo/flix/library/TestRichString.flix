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
}