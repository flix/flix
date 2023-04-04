/*
 * Copyright 2021 Stephen Tetley
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

mod TestGetOpt {

    use GetOpt.{OptionDescr}
    use GetOpt.ArgDescr.{NoArg, ReqArg, OptArg}
    use GetOpt.ArgOrder.{RequireOrder, Permute, ReturnInOrder}
    use GetOpt.{getOpt}

    /// Preliminary code to define options...

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

    def width(x: String): Option[Flag] = Option.map(x1 -> Flag.Width(x1), FromString.fromString(x) : Option[Int32])

    def height(x: String): Option[Flag] = Option.map(x1 -> Flag.Height(x1), FromString.fromString(x) : Option[Int32])


    def options(): List[OptionDescr[Flag]] =
            {optionIds = 'X' :: Nil,        optionNames = "hires" :: Nil,               argDescriptor = NoArg(Flag.HiRes),          explanation = "show in high resolution"}
        ::  {optionIds = 'c' :: 'C' :: Nil, optionNames = "colour" :: "color" :: Nil,   argDescriptor = OptArg(colour, "COLOUR"),   explanation = "add filter of COLOUR"}
        ::  {optionIds = 'w':: Nil,         optionNames = "width" :: Nil,               argDescriptor = ReqArg(width, "WIDTH"),     explanation = "image WIDTH" }
        ::  {optionIds = 'h':: Nil,         optionNames = "height" :: Nil,              argDescriptor = ReqArg(height, "HEIGHT"),   explanation = "image HEIGHT" }
        :: Nil

    def assertSuccessWith(x: Validation[e, a], test: a -> Bool): Bool  = match x {
        case Validation.Success(x1) => test(x1)
        case Validation.Failure(_) => false
    }

    def assertFailureWith(x: Validation[e, a], test: Nec[e] -> Bool): Bool  = match x {
        case Validation.Success(_) => false
        case Validation.Failure(es) => test(es)
    }

    /////////////////////////////////////////////////////////////////////////////
    // No command line arguments                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testNoArgs01(): Bool  =
        let args = Nil;
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            x.options == Nil and x.nonOptions == Nil

    /////////////////////////////////////////////////////////////////////////////
    // Non-options                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testNonOptions01(): Bool  =
        let args = "non_option1" :: Nil;
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            x.options == Nil and x.nonOptions == "non_option1" :: Nil

    @test
    def testNonOptions02(): Bool  =
        let args = "non_option1" :: "non_option2" :: Nil;
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            x.options == Nil and x.nonOptions == "non_option1" :: "non_option2" :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // Single char options                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testCharArgs01(): Bool  =
        let args = "-X" :: Nil;
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == Flag.HiRes :: Nil) and (x.nonOptions == Nil)

    @test
    def testCharArgs02(): Bool  =
        let args = "-Xc" :: Nil;
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == Flag.HiRes :: Flag.Colour(None) :: Nil) and (x.nonOptions == Nil)

    @test
    def testCharArgs03(): Bool  =
        let args = "-XcBlue" :: Nil;
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == Flag.HiRes :: Flag.Colour(Some("Blue")) :: Nil) and (x.nonOptions == Nil)

    @test
    def testCharArgs04(): Bool  =
        let args = "-X" :: "-c" :: Nil;
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == Flag.HiRes :: Flag.Colour(None) :: Nil) and (x.nonOptions == Nil)

    @test
    def testCharArgs05(): Bool  =
        let args = "-X" :: "-cBlue" :: Nil;
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == Flag.HiRes :: Flag.Colour(Some("Blue")) :: Nil) and (x.nonOptions == Nil)

    @test
    def testCharArgs06(): Bool  =
        let args = "-X" :: "-cBlue" :: "non_option1" :: Nil;
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == Flag.HiRes :: Flag.Colour(Some("Blue")) :: Nil) and (x.nonOptions == "non_option1" :: Nil)

    @test
    def testCharArgs07(): Bool  =
        let args = "-X" :: "-cBlue" :: "-w1024" :: "-h768" :: Nil;
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == Flag.HiRes :: Flag.Colour(Some("Blue")) :: Flag.Width(1024) :: Flag.Height(768) :: Nil)
                and (x.nonOptions == Nil)

    @test
    def testCharArgs08(): Bool  =
        let args = "-X" :: "-cBlue" :: "-w1024" :: "-h768" :: "non_option1" :: Nil;
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == Flag.HiRes :: Flag.Colour(Some("Blue")) :: Flag.Width(1024) :: Flag.Height(768) :: Nil)
                and (x.nonOptions == "non_option1" :: Nil)

    @test
    def testCharArgs09(): Bool  =
        /// "-m" not defined
        let args = "-m" :: Nil;
        getOpt(Permute, options(), args) `assertFailureWith` x ->
            Nec.length(x) == 1

    @test
    def testCharArgs10(): Bool  =
        /// "-m" not defined
        let args = "-Xm" :: Nil;
        getOpt(Permute, options(), args) `assertFailureWith` x ->
            Nec.length(x) == 1

    @test
    def testCharArgs11(): Bool  =
        /// "-m" not defined
        let args = "-X" :: "-m" :: Nil;
        getOpt(Permute, options(), args) `assertFailureWith` x ->
            Nec.length(x) == 1

    /////////////////////////////////////////////////////////////////////////////
    // String / name options                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testStringArgs01(): Bool  =
        let args = "--hires" :: Nil;
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == Flag.HiRes :: Nil) and (x.nonOptions == Nil)

    @test
    def testStringArgs02(): Bool  =
        let args = "--hires" :: "--colour" :: Nil;
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == Flag.HiRes :: Flag.Colour(None) :: Nil) and (x.nonOptions == Nil)

    @test
    def testStringArgs03(): Bool  =
        let args = "--hires" :: "--colour=Blue" :: Nil;
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == Flag.HiRes :: Flag.Colour(Some("Blue")) :: Nil) and (x.nonOptions == Nil)

    @test
    def testStringArgs04(): Bool  =
        /// Alternative spelling
        let args = "--hires" :: "--color" :: Nil;
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == Flag.HiRes :: Flag.Colour(None) :: Nil) and (x.nonOptions == Nil)

    @test
    def testStringArgs05(): Bool  =
    /// Alternative spelling
        let args = "--hires" :: "--color=Blue" :: Nil;
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == Flag.HiRes :: Flag.Colour(Some("Blue")) :: Nil) and (x.nonOptions == Nil)

    @test
    def testStringArgs06(): Bool  =
        let args = "--hires" :: "--colour=Blue" :: "non_option1" :: Nil;
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == Flag.HiRes :: Flag.Colour(Some("Blue")) :: Nil) and (x.nonOptions == "non_option1" :: Nil)

    @test
    def testStringArgs07(): Bool  =
        let args = "--hires" :: "--colour=Blue" :: "--width=1024" :: "--height=768" :: Nil;
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == Flag.HiRes :: Flag.Colour(Some("Blue")) :: Flag.Width(1024) :: Flag.Height(768) :: Nil)
                and (x.nonOptions == Nil)

    @test
    def testStringArgs08(): Bool  =
        let args = "--hires" :: "--colour=Blue" :: "--width=1024" :: "--height=768" :: "non_option1" :: Nil;
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == Flag.HiRes :: Flag.Colour(Some("Blue")) :: Flag.Width(1024) :: Flag.Height(768) :: Nil)
                and (x.nonOptions == "non_option1" :: Nil)

    @test
    def testStringArgs09(): Bool  =
        /// "--multiple" not defined
        let args = "--multiple" :: Nil;
        getOpt(Permute, options(), args) `assertFailureWith` x ->
            Nec.length(x) == 1

    @test
    def testStringArgs10(): Bool  =
        /// "--multiple" not defined
        let args = "--hires" :: "--multiple" :: Nil;
        getOpt(Permute, options(), args) `assertFailureWith` x ->
            Nec.length(x) == 1

    @test
    def testStringArgs11(): Bool  =
        /// "--multiple" not defined
        let args = "--hires" :: "--multiple" :: "test_option1" :: Nil;
        getOpt(Permute, options(), args) `assertFailureWith` x ->
            Nec.length(x) == 1

    /////////////////////////////////////////////////////////////////////////////
    // Mixed short and name options                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testMixedArgs01(): Bool  =
        let args = "--hires" :: "-c" :: Nil;
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == Flag.HiRes :: Flag.Colour(None) :: Nil) and (x.nonOptions == Nil)

    @test
    def testMixedArgs02(): Bool  =
        let args = "-X" :: "--colour" :: Nil;
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == Flag.HiRes :: Flag.Colour(None) :: Nil) and (x.nonOptions == Nil)

    @test
    def testMixedArgs03(): Bool  =
        let args = "--hires" :: "-cBlue" :: Nil;
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == Flag.HiRes :: Flag.Colour(Some("Blue")) :: Nil) and (x.nonOptions == Nil)

    @test
    def testMixedArgs04(): Bool  =
        /// Alternative spelling
        let args = "-X" :: "--color" :: Nil;
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == Flag.HiRes :: Flag.Colour(None) :: Nil) and (x.nonOptions == Nil)

    @test
    def testMixedArgs05(): Bool  =
    /// Alternative spelling
        let args = "-X" :: "--color=Blue" :: Nil;
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == Flag.HiRes :: Flag.Colour(Some("Blue")) :: Nil) and (x.nonOptions == Nil)

    @test
    def testMixedArgs06(): Bool  =
        let args = "-X" :: "--colour=Blue" :: "non_option1" :: Nil;
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == Flag.HiRes :: Flag.Colour(Some("Blue")) :: Nil) and (x.nonOptions == "non_option1" :: Nil)

    @test
    def testMixedArgs07(): Bool  =
        let args = "-X" :: "--colour=Blue" :: "-w1024" :: "-h768" :: Nil;
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == Flag.HiRes :: Flag.Colour(Some("Blue")) :: Flag.Width(1024) :: Flag.Height(768) :: Nil)
                and (x.nonOptions == Nil)

    @test
    def testMixedArgs08(): Bool  =
        let args = "-X" :: "-cBlue" :: "--width=1024" :: "--height=768" :: "non_option1" :: Nil;
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == Flag.HiRes :: Flag.Colour(Some("Blue")) :: Flag.Width(1024) :: Flag.Height(768) :: Nil)
                and (x.nonOptions == "non_option1" :: Nil)

    @test
    def testMixedArgs09(): Bool  =
        /// "-m" not defined
        let args = "--hires" :: "-m" :: Nil;
        getOpt(Permute, options(), args) `assertFailureWith` x ->
            Nec.length(x) == 1

    @test
    def testMixedArgs10(): Bool  =
        /// "--multiple" not defined
        let args = "-X" :: "--multiple" :: Nil;
        getOpt(Permute, options(), args) `assertFailureWith` x ->
            Nec.length(x) == 1

    @test
    def testMixedArgs11(): Bool  =
        /// "--multiple" not defined
        let args = "-X" :: "--multiple" :: "test_option1" :: Nil;
        getOpt(Permute, options(), args) `assertFailureWith` x ->
            Nec.length(x) == 1

    /////////////////////////////////////////////////////////////////////////////
    // Prefix of long option                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testPrefixArgs01(): Bool  =
        /// Fail: Could be "hires" or "height"
        let args = "--h=1000" :: Nil;
        getOpt(Permute, options(), args) `assertFailureWith` x ->
            Nec.length(x) == 1

    @test
    def testPrefixArgs02(): Bool  =
        /// Success: Could be only be "hires"
        let args = "--hi" :: Nil;
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == Flag.HiRes :: Nil) and (x.nonOptions == Nil)

    @test
    def testPrefixArgs03(): Bool  =
        /// Success: Could be only be "height"
        let args = "--he=1200" :: Nil;
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == Flag.Height(1200) :: Nil) and (x.nonOptions == Nil)

    @test
    def testPrefixArgs04(): Bool  =
        /// Success: Could be only be "height"
        let args = "--hi" :: "--he=1200" :: "test_option1" :: Nil;
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == Flag.HiRes :: Flag.Height(1200) :: Nil) and (x.nonOptions == "test_option1" :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // Decoding option args                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testDecodeArgs01(): Bool  =
        let args = "--width=1400" :: Nil;
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == Flag.Width(1400) :: Nil) and (x.nonOptions == Nil)

    @test
    def testDecodeArgs02(): Bool  =
        let args = "--width=fourteen_thousand" :: Nil;
        getOpt(Permute, options(), args) `assertFailureWith` x ->
            Nec.length(x) == 1

    @test
    def testDecodeArgs03(): Bool  =
        let args = "--width=1400" :: "--height=1000" :: Nil;
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == Flag.Width(1400) :: Flag.Height(1000) :: Nil) and (x.nonOptions == Nil)

    @test
    def testDecodeArgs04(): Bool  =
        let args = "--width=fourteen_hundred" :: "--height=1000" :: Nil;
        getOpt(Permute, options(), args) `assertFailureWith` x ->
            Nec.length(x) == 1

    @test
    def testDecodeArgs05(): Bool  =
        let args = "--width=1400" :: "--height=one_thousand" :: Nil;
        getOpt(Permute, options(), args) `assertFailureWith` x ->
            Nec.length(x) == 1

    @test
    def testDecodeArgs06(): Bool  =
        let args = "--width=fourteen_hundred" :: "--height=one_thousand" :: Nil;
        getOpt(Permute, options(), args) `assertFailureWith` x ->
            Nec.length(x) == 2

    /////////////////////////////////////////////////////////////////////////////
    // Preprocess                                                              //
    /////////////////////////////////////////////////////////////////////////////

    def squote(): String = "'"

    @test
    def preprocess01(): Bool  =
        let args = Nil;
        GetOpt.preprocess({quoteOpen = squote(), quoteClose = squote(), stripQuoteMarks = false}, args)
            == Nil

    @test
    def preprocess02(): Bool  =
        let args = String.split({regex = "\\s+"}, "--version");
        GetOpt.preprocess({quoteOpen = squote(), quoteClose = squote(), stripQuoteMarks = false}, args)
            == "--version" :: Nil


    @test
    def preprocess03(): Bool  =
        let args = String.split({regex = "\\s+"}, "--editor=vi");
        GetOpt.preprocess({quoteOpen = squote(), quoteClose = squote(), stripQuoteMarks = false}, args)
            == "--editor=vi" :: Nil

    @test
    def preprocess04(): Bool  =
        /// Note - not quoted...
        let args = String.split({regex = "\\s+"}, "--editor=vs code");
        GetOpt.preprocess({quoteOpen = squote(), quoteClose = squote(), stripQuoteMarks = false}, args)
            == "--editor=vs" :: "code" :: Nil

    @test
    def preprocess05(): Bool  =
        /// Note - not quoted...
        let args = String.split({regex = "\\s+"}, "--editor='vs code'");
        GetOpt.preprocess({quoteOpen = squote(), quoteClose = squote(), stripQuoteMarks = false}, args)
            == "--editor='vs code'" :: Nil

    @test
    def preprocess06(): Bool  =
        let args = String.split({regex = "\\s+"}, "--editor='vs code'");
        GetOpt.preprocess({quoteOpen = squote(), quoteClose = squote(), stripQuoteMarks = true}, args)
            == "--editor=vs code" :: Nil

    @test
    def preprocess07(): Bool  =
        let args = String.split({regex = "\\s+"}, "--version --editor='vs code'");
        GetOpt.preprocess({quoteOpen = squote(), quoteClose = squote(), stripQuoteMarks = false}, args)
            == "--version" :: "--editor='vs code'" :: Nil

    @test
    def preprocess08(): Bool  =
        let args = String.split({regex = "\\s+"}, "--version --editor='vs code' 'D:/my file1.txt'");
        GetOpt.preprocess({quoteOpen = squote(), quoteClose = squote(), stripQuoteMarks = false}, args)
            == "--version" :: "--editor='vs code'" :: "'D:/my file1.txt'" :: Nil

    @test
    def preprocess09(): Bool  =
        let args = String.split({regex = "\\s+"}, "--version --editor='vs code' 'D:/my file1.txt'");
        GetOpt.preprocess({quoteOpen = squote(), quoteClose = squote(), stripQuoteMarks = true}, args)
            == "--version" :: "--editor=vs code" :: "D:/my file1.txt" :: Nil

}
