mod TestGetOpt {

    use GetOpt.{OptionDescr}
    use GetOpt.ArgDescr.{NoArg, ReqArg, OptArg}
    use GetOpt.ArgOrder.{RequireOrder, Permute, ReturnInOrder}
    use GetOpt.{getOpt}
    use Assert.{assertEq, assertTrue, fail}

    // Preliminary code to define options...

    enum Flag {
        case HiRes
        case Colour(Option[String])
        case Width(Int32)
        case Height(Int32)
    }

    instance Eq[Flag] {
        pub def eq(x: Flag, y: Flag): Bool = match (x, y) {
            case (Flag.HiRes, Flag.HiRes)             => true
            case (Flag.Colour(x1), Flag.Colour(y1))   => x1 == y1
            case (Flag.Width(x1), Flag.Width(y1))     => x1 == y1
            case (Flag.Height(x1), Flag.Height(y1))   => x1 == y1
            case (_, _)                               => false
        }
    }

    instance ToString[Flag] {
        pub def toString(x: Flag): String = match x {
            case Flag.HiRes      => "A"
            case Flag.Colour(y)  => "Colour(${y})"
            case Flag.Width(i)   => "Width('${i}')"
            case Flag.Height(i)  => "Height('${i}')"
        }
    }

    def colour(x: Option[String]): Option[Flag] = Some(Flag.Colour(x))

    def width(x: String): Option[Flag] = Option.map(x1 -> Flag.Width(x1), (FromString.fromString(x) : Option[Int32]))

    def height(x: String): Option[Flag] = Option.map(x1 -> Flag.Height(x1), (FromString.fromString(x) : Option[Int32]))


    def options(): List[OptionDescr[Flag]] =
            {optionIds = 'X' :: Nil,        optionNames = "hires" :: Nil,               argDescriptor = NoArg(Flag.HiRes),          explanation = "show in high resolution"}
        ::  {optionIds = 'c' :: 'C' :: Nil, optionNames = "colour" :: "color" :: Nil,   argDescriptor = OptArg(colour, "COLOUR"),   explanation = "add filter of COLOUR"}
        ::  {optionIds = 'w':: Nil,         optionNames = "width" :: Nil,               argDescriptor = ReqArg(width, "WIDTH"),     explanation = "image WIDTH" }
        ::  {optionIds = 'h':: Nil,         optionNames = "height" :: Nil,              argDescriptor = ReqArg(height, "HEIGHT"),   explanation = "image HEIGHT" }
        :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // No command line arguments                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testNoArgs01(): Unit \ Assert =
        let args = Nil;
        match getOpt(Permute, options(), args) {
            case Validation.Success(x) => {
                assertEq(expected = Nil, x#options);
                assertEq(expected = Nil, x#nonOptions)
            }
            case Validation.Failure(_) => fail("Expected success but got failure")
        }

    /////////////////////////////////////////////////////////////////////////////
    // Non-options                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testNonOptions01(): Unit \ Assert =
        let args = "non_option1" :: Nil;
        match getOpt(Permute, options(), args) {
            case Validation.Success(x) => {
                assertEq(expected = Nil, x#options);
                assertEq(expected = "non_option1" :: Nil, x#nonOptions)
            }
            case Validation.Failure(_) => fail("Expected success but got failure")
        }

    @Test
    def testNonOptions02(): Unit \ Assert =
        let args = "non_option1" :: "non_option2" :: Nil;
        match getOpt(Permute, options(), args) {
            case Validation.Success(x) => {
                assertEq(expected = Nil, x#options);
                assertEq(expected = "non_option1" :: "non_option2" :: Nil, x#nonOptions)
            }
            case Validation.Failure(_) => fail("Expected success but got failure")
        }

    /////////////////////////////////////////////////////////////////////////////
    // Single char options                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testCharArgs01(): Unit \ Assert =
        let args = "-X" :: Nil;
        match getOpt(Permute, options(), args) {
            case Validation.Success(x) => {
                assertEq(expected = Flag.HiRes :: Nil, x#options);
                assertEq(expected = Nil, x#nonOptions)
            }
            case Validation.Failure(_) => fail("Expected success but got failure")
        }

    @Test
    def testCharArgs02(): Unit \ Assert =
        let args = "-Xc" :: Nil;
        match getOpt(Permute, options(), args) {
            case Validation.Success(x) => {
                assertEq(expected = Flag.HiRes :: Flag.Colour(None) :: Nil, x#options);
                assertEq(expected = Nil, x#nonOptions)
            }
            case Validation.Failure(_) => fail("Expected success but got failure")
        }

    @Test
    def testCharArgs03(): Unit \ Assert =
        let args = "-XcBlue" :: Nil;
        match getOpt(Permute, options(), args) {
            case Validation.Success(x) => {
                assertEq(expected = Flag.HiRes :: Flag.Colour(Some("Blue")) :: Nil, x#options);
                assertEq(expected = Nil, x#nonOptions)
            }
            case Validation.Failure(_) => fail("Expected success but got failure")
        }

    @Test
    def testCharArgs04(): Unit \ Assert =
        let args = "-X" :: "-c" :: Nil;
        match getOpt(Permute, options(), args) {
            case Validation.Success(x) => {
                assertEq(expected = Flag.HiRes :: Flag.Colour(None) :: Nil, x#options);
                assertEq(expected = Nil, x#nonOptions)
            }
            case Validation.Failure(_) => fail("Expected success but got failure")
        }

    @Test
    def testCharArgs05(): Unit \ Assert =
        let args = "-X" :: "-cBlue" :: Nil;
        match getOpt(Permute, options(), args) {
            case Validation.Success(x) => {
                assertEq(expected = Flag.HiRes :: Flag.Colour(Some("Blue")) :: Nil, x#options);
                assertEq(expected = Nil, x#nonOptions)
            }
            case Validation.Failure(_) => fail("Expected success but got failure")
        }

    @Test
    def testCharArgs06(): Unit \ Assert =
        let args = "-X" :: "-cBlue" :: "non_option1" :: Nil;
        match getOpt(Permute, options(), args) {
            case Validation.Success(x) => {
                assertEq(expected = Flag.HiRes :: Flag.Colour(Some("Blue")) :: Nil, x#options);
                assertEq(expected = "non_option1" :: Nil, x#nonOptions)
            }
            case Validation.Failure(_) => fail("Expected success but got failure")
        }

    @Test
    def testCharArgs07(): Unit \ Assert =
        let args = "-X" :: "-cBlue" :: "-w1024" :: "-h768" :: Nil;
        match getOpt(Permute, options(), args) {
            case Validation.Success(x) => {
                assertEq(expected = Flag.HiRes :: Flag.Colour(Some("Blue")) :: Flag.Width(1024) :: Flag.Height(768) :: Nil, x#options);
                assertEq(expected = Nil, x#nonOptions)
            }
            case Validation.Failure(_) => fail("Expected success but got failure")
        }

    @Test
    def testCharArgs08(): Unit \ Assert =
        let args = "-X" :: "-cBlue" :: "-w1024" :: "-h768" :: "non_option1" :: Nil;
        match getOpt(Permute, options(), args) {
            case Validation.Success(x) => {
                assertEq(expected = Flag.HiRes :: Flag.Colour(Some("Blue")) :: Flag.Width(1024) :: Flag.Height(768) :: Nil, x#options);
                assertEq(expected = "non_option1" :: Nil, x#nonOptions)
            }
            case Validation.Failure(_) => fail("Expected success but got failure")
        }

    @Test
    def testCharArgs09(): Unit \ Assert =
        // "-m" not defined
        let args = "-m" :: Nil;
        match getOpt(Permute, options(), args) {
            case Validation.Success(_) => fail("Expected failure but got success")
            case Validation.Failure(es) => assertEq(expected = 1, Nec.length(es))
        }

    @Test
    def testCharArgs10(): Unit \ Assert =
        // "-m" not defined
        let args = "-Xm" :: Nil;
        match getOpt(Permute, options(), args) {
            case Validation.Success(_) => fail("Expected failure but got success")
            case Validation.Failure(es) => assertEq(expected = 1, Nec.length(es))
        }

    @Test
    def testCharArgs11(): Unit \ Assert =
        // "-m" not defined
        let args = "-X" :: "-m" :: Nil;
        match getOpt(Permute, options(), args) {
            case Validation.Success(_) => fail("Expected failure but got success")
            case Validation.Failure(es) => assertEq(expected = 1, Nec.length(es))
        }

    /////////////////////////////////////////////////////////////////////////////
    // String / name options                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testStringArgs01(): Unit \ Assert =
        let args = "--hires" :: Nil;
        match getOpt(Permute, options(), args) {
            case Validation.Success(x) => {
                assertEq(expected = Flag.HiRes :: Nil, x#options);
                assertEq(expected = Nil, x#nonOptions)
            }
            case Validation.Failure(_) => fail("Expected success but got failure")
        }

    @Test
    def testStringArgs02(): Unit \ Assert =
        let args = "--hires" :: "--colour" :: Nil;
        match getOpt(Permute, options(), args) {
            case Validation.Success(x) => {
                assertEq(expected = Flag.HiRes :: Flag.Colour(None) :: Nil, x#options);
                assertEq(expected = Nil, x#nonOptions)
            }
            case Validation.Failure(_) => fail("Expected success but got failure")
        }

    @Test
    def testStringArgs03(): Unit \ Assert =
        let args = "--hires" :: "--colour=Blue" :: Nil;
        match getOpt(Permute, options(), args) {
            case Validation.Success(x) => {
                assertEq(expected = Flag.HiRes :: Flag.Colour(Some("Blue")) :: Nil, x#options);
                assertEq(expected = Nil, x#nonOptions)
            }
            case Validation.Failure(_) => fail("Expected success but got failure")
        }

    @Test
    def testStringArgs04(): Unit \ Assert =
        // Alternative spelling
        let args = "--hires" :: "--color" :: Nil;
        match getOpt(Permute, options(), args) {
            case Validation.Success(x) => {
                assertEq(expected = Flag.HiRes :: Flag.Colour(None) :: Nil, x#options);
                assertEq(expected = Nil, x#nonOptions)
            }
            case Validation.Failure(_) => fail("Expected success but got failure")
        }

    @Test
    def testStringArgs05(): Unit \ Assert =
        // Alternative spelling
        let args = "--hires" :: "--color=Blue" :: Nil;
        match getOpt(Permute, options(), args) {
            case Validation.Success(x) => {
                assertEq(expected = Flag.HiRes :: Flag.Colour(Some("Blue")) :: Nil, x#options);
                assertEq(expected = Nil, x#nonOptions)
            }
            case Validation.Failure(_) => fail("Expected success but got failure")
        }

    @Test
    def testStringArgs06(): Unit \ Assert =
        let args = "--hires" :: "--colour=Blue" :: "non_option1" :: Nil;
        match getOpt(Permute, options(), args) {
            case Validation.Success(x) => {
                assertEq(expected = Flag.HiRes :: Flag.Colour(Some("Blue")) :: Nil, x#options);
                assertEq(expected = "non_option1" :: Nil, x#nonOptions)
            }
            case Validation.Failure(_) => fail("Expected success but got failure")
        }

    @Test
    def testStringArgs07(): Unit \ Assert =
        let args = "--hires" :: "--colour=Blue" :: "--width=1024" :: "--height=768" :: Nil;
        match getOpt(Permute, options(), args) {
            case Validation.Success(x) => {
                assertEq(expected = Flag.HiRes :: Flag.Colour(Some("Blue")) :: Flag.Width(1024) :: Flag.Height(768) :: Nil, x#options);
                assertEq(expected = Nil, x#nonOptions)
            }
            case Validation.Failure(_) => fail("Expected success but got failure")
        }

    @Test
    def testStringArgs08(): Unit \ Assert =
        let args = "--hires" :: "--colour=Blue" :: "--width=1024" :: "--height=768" :: "non_option1" :: Nil;
        match getOpt(Permute, options(), args) {
            case Validation.Success(x) => {
                assertEq(expected = Flag.HiRes :: Flag.Colour(Some("Blue")) :: Flag.Width(1024) :: Flag.Height(768) :: Nil, x#options);
                assertEq(expected = "non_option1" :: Nil, x#nonOptions)
            }
            case Validation.Failure(_) => fail("Expected success but got failure")
        }

    @Test
    def testStringArgs09(): Unit \ Assert =
        // "--multiple" not defined
        let args = "--multiple" :: Nil;
        match getOpt(Permute, options(), args) {
            case Validation.Success(_) => fail("Expected failure but got success")
            case Validation.Failure(es) => assertEq(expected = 1, Nec.length(es))
        }

    @Test
    def testStringArgs10(): Unit \ Assert =
        // "--multiple" not defined
        let args = "--hires" :: "--multiple" :: Nil;
        match getOpt(Permute, options(), args) {
            case Validation.Success(_) => fail("Expected failure but got success")
            case Validation.Failure(es) => assertEq(expected = 1, Nec.length(es))
        }

    @Test
    def testStringArgs11(): Unit \ Assert =
        // "--multiple" not defined
        let args = "--hires" :: "--multiple" :: "test_option1" :: Nil;
        match getOpt(Permute, options(), args) {
            case Validation.Success(_) => fail("Expected failure but got success")
            case Validation.Failure(es) => assertEq(expected = 1, Nec.length(es))
        }

    /////////////////////////////////////////////////////////////////////////////
    // Mixed short and name options                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testMixedArgs01(): Unit \ Assert =
        let args = "--hires" :: "-c" :: Nil;
        match getOpt(Permute, options(), args) {
            case Validation.Success(x) => {
                assertEq(expected = Flag.HiRes :: Flag.Colour(None) :: Nil, x#options);
                assertEq(expected = Nil, x#nonOptions)
            }
            case Validation.Failure(_) => fail("Expected success but got failure")
        }

    @Test
    def testMixedArgs02(): Unit \ Assert =
        let args = "-X" :: "--colour" :: Nil;
        match getOpt(Permute, options(), args) {
            case Validation.Success(x) => {
                assertEq(expected = Flag.HiRes :: Flag.Colour(None) :: Nil, x#options);
                assertEq(expected = Nil, x#nonOptions)
            }
            case Validation.Failure(_) => fail("Expected success but got failure")
        }

    @Test
    def testMixedArgs03(): Unit \ Assert =
        let args = "--hires" :: "-cBlue" :: Nil;
        match getOpt(Permute, options(), args) {
            case Validation.Success(x) => {
                assertEq(expected = Flag.HiRes :: Flag.Colour(Some("Blue")) :: Nil, x#options);
                assertEq(expected = Nil, x#nonOptions)
            }
            case Validation.Failure(_) => fail("Expected success but got failure")
        }

    @Test
    def testMixedArgs04(): Unit \ Assert =
        // Alternative spelling
        let args = "-X" :: "--color" :: Nil;
        match getOpt(Permute, options(), args) {
            case Validation.Success(x) => {
                assertEq(expected = Flag.HiRes :: Flag.Colour(None) :: Nil, x#options);
                assertEq(expected = Nil, x#nonOptions)
            }
            case Validation.Failure(_) => fail("Expected success but got failure")
        }

    @Test
    def testMixedArgs05(): Unit \ Assert =
        // Alternative spelling
        let args = "-X" :: "--color=Blue" :: Nil;
        match getOpt(Permute, options(), args) {
            case Validation.Success(x) => {
                assertEq(expected = Flag.HiRes :: Flag.Colour(Some("Blue")) :: Nil, x#options);
                assertEq(expected = Nil, x#nonOptions)
            }
            case Validation.Failure(_) => fail("Expected success but got failure")
        }

    @Test
    def testMixedArgs06(): Unit \ Assert =
        let args = "-X" :: "--colour=Blue" :: "non_option1" :: Nil;
        match getOpt(Permute, options(), args) {
            case Validation.Success(x) => {
                assertEq(expected = Flag.HiRes :: Flag.Colour(Some("Blue")) :: Nil, x#options);
                assertEq(expected = "non_option1" :: Nil, x#nonOptions)
            }
            case Validation.Failure(_) => fail("Expected success but got failure")
        }

    @Test
    def testMixedArgs07(): Unit \ Assert =
        let args = "-X" :: "--colour=Blue" :: "-w1024" :: "-h768" :: Nil;
        match getOpt(Permute, options(), args) {
            case Validation.Success(x) => {
                assertEq(expected = Flag.HiRes :: Flag.Colour(Some("Blue")) :: Flag.Width(1024) :: Flag.Height(768) :: Nil, x#options);
                assertEq(expected = Nil, x#nonOptions)
            }
            case Validation.Failure(_) => fail("Expected success but got failure")
        }

    @Test
    def testMixedArgs08(): Unit \ Assert =
        let args = "-X" :: "-cBlue" :: "--width=1024" :: "--height=768" :: "non_option1" :: Nil;
        match getOpt(Permute, options(), args) {
            case Validation.Success(x) => {
                assertEq(expected = Flag.HiRes :: Flag.Colour(Some("Blue")) :: Flag.Width(1024) :: Flag.Height(768) :: Nil, x#options);
                assertEq(expected = "non_option1" :: Nil, x#nonOptions)
            }
            case Validation.Failure(_) => fail("Expected success but got failure")
        }

    @Test
    def testMixedArgs09(): Unit \ Assert =
        // "-m" not defined
        let args = "--hires" :: "-m" :: Nil;
        match getOpt(Permute, options(), args) {
            case Validation.Success(_) => fail("Expected failure but got success")
            case Validation.Failure(es) => assertEq(expected = 1, Nec.length(es))
        }

    @Test
    def testMixedArgs10(): Unit \ Assert =
        // "--multiple" not defined
        let args = "-X" :: "--multiple" :: Nil;
        match getOpt(Permute, options(), args) {
            case Validation.Success(_) => fail("Expected failure but got success")
            case Validation.Failure(es) => assertEq(expected = 1, Nec.length(es))
        }

    @Test
    def testMixedArgs11(): Unit \ Assert =
        // "--multiple" not defined
        let args = "-X" :: "--multiple" :: "test_option1" :: Nil;
        match getOpt(Permute, options(), args) {
            case Validation.Success(_) => fail("Expected failure but got success")
            case Validation.Failure(es) => assertEq(expected = 1, Nec.length(es))
        }

    /////////////////////////////////////////////////////////////////////////////
    // Prefix of long option                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testPrefixArgs01(): Unit \ Assert =
        // Fail: Could be "hires" or "height"
        let args = "--h=1000" :: Nil;
        match getOpt(Permute, options(), args) {
            case Validation.Success(_) => fail("Expected failure but got success")
            case Validation.Failure(es) => assertEq(expected = 1, Nec.length(es))
        }

    @Test
    def testPrefixArgs02(): Unit \ Assert =
        // Success: Could be only be "hires"
        let args = "--hi" :: Nil;
        match getOpt(Permute, options(), args) {
            case Validation.Success(x) => {
                assertEq(expected = Flag.HiRes :: Nil, x#options);
                assertEq(expected = Nil, x#nonOptions)
            }
            case Validation.Failure(_) => fail("Expected success but got failure")
        }

    @Test
    def testPrefixArgs03(): Unit \ Assert =
        // Success: Could be only be "height"
        let args = "--he=1200" :: Nil;
        match getOpt(Permute, options(), args) {
            case Validation.Success(x) => {
                assertEq(expected = Flag.Height(1200) :: Nil, x#options);
                assertEq(expected = Nil, x#nonOptions)
            }
            case Validation.Failure(_) => fail("Expected success but got failure")
        }

    @Test
    def testPrefixArgs04(): Unit \ Assert =
        // Success: Could be only be "height"
        let args = "--hi" :: "--he=1200" :: "test_option1" :: Nil;
        match getOpt(Permute, options(), args) {
            case Validation.Success(x) => {
                assertEq(expected = Flag.HiRes :: Flag.Height(1200) :: Nil, x#options);
                assertEq(expected = "test_option1" :: Nil, x#nonOptions)
            }
            case Validation.Failure(_) => fail("Expected success but got failure")
        }

    /////////////////////////////////////////////////////////////////////////////
    // Decoding option args                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testDecodeArgs01(): Unit \ Assert =
        let args = "--width=1400" :: Nil;
        match getOpt(Permute, options(), args) {
            case Validation.Success(x) => {
                assertEq(expected = Flag.Width(1400) :: Nil, x#options);
                assertEq(expected = Nil, x#nonOptions)
            }
            case Validation.Failure(_) => fail("Expected success but got failure")
        }

    @Test
    def testDecodeArgs02(): Unit \ Assert =
        let args = "--width=fourteen_thousand" :: Nil;
        match getOpt(Permute, options(), args) {
            case Validation.Success(_) => fail("Expected failure but got success")
            case Validation.Failure(es) => assertEq(expected = 1, Nec.length(es))
        }

    @Test
    def testDecodeArgs03(): Unit \ Assert =
        let args = "--width=1400" :: "--height=1000" :: Nil;
        match getOpt(Permute, options(), args) {
            case Validation.Success(x) => {
                assertEq(expected = Flag.Width(1400) :: Flag.Height(1000) :: Nil, x#options);
                assertEq(expected = Nil, x#nonOptions)
            }
            case Validation.Failure(_) => fail("Expected success but got failure")
        }

    @Test
    def testDecodeArgs04(): Unit \ Assert =
        let args = "--width=fourteen_hundred" :: "--height=1000" :: Nil;
        match getOpt(Permute, options(), args) {
            case Validation.Success(_) => fail("Expected failure but got success")
            case Validation.Failure(es) => assertEq(expected = 1, Nec.length(es))
        }

    @Test
    def testDecodeArgs05(): Unit \ Assert =
        let args = "--width=1400" :: "--height=one_thousand" :: Nil;
        match getOpt(Permute, options(), args) {
            case Validation.Success(_) => fail("Expected failure but got success")
            case Validation.Failure(es) => assertEq(expected = 1, Nec.length(es))
        }

    @Test
    def testDecodeArgs06(): Unit \ Assert =
        let args = "--width=fourteen_hundred" :: "--height=one_thousand" :: Nil;
        match getOpt(Permute, options(), args) {
            case Validation.Success(_) => fail("Expected failure but got success")
            case Validation.Failure(es) => assertEq(expected = 2, Nec.length(es))
        }

    /////////////////////////////////////////////////////////////////////////////
    // Preprocess                                                              //
    /////////////////////////////////////////////////////////////////////////////

    def squote(): String = "'"

    @Test
    def preprocess01(): Unit \ Assert =
        let args = Nil;
        let result = GetOpt.preprocess({quoteOpen = squote(), quoteClose = squote(), stripQuoteMarks = false}, args);
        assertEq(expected = Nil, result)

    @Test
    def preprocess02(): Unit \ Assert =
        let args = String.split({regex = "\\s+"}, "--version");
        let result = GetOpt.preprocess({quoteOpen = squote(), quoteClose = squote(), stripQuoteMarks = false}, args);
        assertEq(expected = "--version" :: Nil, result)


    @Test
    def preprocess03(): Unit \ Assert =
        let args = String.split({regex = "\\s+"}, "--editor=vi");
        let result = GetOpt.preprocess({quoteOpen = squote(), quoteClose = squote(), stripQuoteMarks = false}, args);
        assertEq(expected = "--editor=vi" :: Nil, result)

    @Test
    def preprocess04(): Unit \ Assert =
        // Note - not quoted...
        let args = String.split({regex = "\\s+"}, "--editor=vs code");
        let result = GetOpt.preprocess({quoteOpen = squote(), quoteClose = squote(), stripQuoteMarks = false}, args);
        assertEq(expected = "--editor=vs" :: "code" :: Nil, result)

    @Test
    def preprocess05(): Unit \ Assert =
        // Note - not quoted...
        let args = String.split({regex = "\\s+"}, "--editor='vs code'");
        let result = GetOpt.preprocess({quoteOpen = squote(), quoteClose = squote(), stripQuoteMarks = false}, args);
        assertEq(expected = "--editor='vs code'" :: Nil, result)

    @Test
    def preprocess06(): Unit \ Assert =
        let args = String.split({regex = "\\s+"}, "--editor='vs code'");
        let result = GetOpt.preprocess({quoteOpen = squote(), quoteClose = squote(), stripQuoteMarks = true}, args);
        assertEq(expected = "--editor=vs code" :: Nil, result)

    @Test
    def preprocess07(): Unit \ Assert =
        let args = String.split({regex = "\\s+"}, "--version --editor='vs code'");
        let result = GetOpt.preprocess({quoteOpen = squote(), quoteClose = squote(), stripQuoteMarks = false}, args);
        assertEq(expected = "--version" :: "--editor='vs code'" :: Nil, result)

    @Test
    def preprocess08(): Unit \ Assert =
        let args = String.split({regex = "\\s+"}, "--version --editor='vs code' 'D:/my file1.txt'");
        let result = GetOpt.preprocess({quoteOpen = squote(), quoteClose = squote(), stripQuoteMarks = false}, args);
        assertEq(expected = "--version" :: "--editor='vs code'" :: "'D:/my file1.txt'" :: Nil, result)

    @Test
    def preprocess09(): Unit \ Assert =
        let args = String.split({regex = "\\s+"}, "--version --editor='vs code' 'D:/my file1.txt'");
        let result = GetOpt.preprocess({quoteOpen = squote(), quoteClose = squote(), stripQuoteMarks = true}, args);
        assertEq(expected = "--version" :: "--editor=vs code" :: "D:/my file1.txt" :: Nil, result)

}
