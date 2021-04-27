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

namespace TestGetOpt {

    use GetOpt.{OptionDescr};
    use GetOpt.ArgDescr.{NoArg, ReqArg, OptArg};
    use GetOpt.ArgOrder.{RequireOrder, Permute, ReturnInOrder};
    use GetOpt.{getOpt};

    /// Preliminary code to define options...

    enum Flag {
        case HiRes
        case Colour(Option[String])
        case Width(Int32)
        case Height(Int32)
    }

    instance Eq[Flag] {
        pub def eq(x: Flag, y: Flag): Bool = match (x, y) {
            case (HiRes, HiRes)             => true
            case (Colour(x1), Colour(y1))   => x1 == y1
            case (Width(x1), Width(y1))     => x1 == y1
            case (Height(x1), Height(y1))   => x1 == y1
            case (_, _)                     => false
        }
    }

    instance ToString[Flag] {
        pub def toString(x: Flag): String = match x {
            case HiRes      => "A"
            case Colour(y)  => "Colour(${y})"
            case Width(i)   => "Width('${i}')"
            case Height(i)  => "Height('${i}')"
        }
    }

    def colour(x: Option[String]): Option[Flag] = Some(Colour(x))

    def width(x: String): Option[Flag] = Option.map(x1 -> Width(x1), FromString.fromString(x) : Option[Int32])

    def height(x: String): Option[Flag] = Option.map(x1 -> Height(x1), FromString.fromString(x) : Option[Int32])


    def options(): List[OptionDescr[Flag]] =
            {optionIds = 'X' :: Nil,        optionNames = "hires" :: Nil,               argDescriptor = NoArg(HiRes),               explanation = "show in high resolution"}
        ::  {optionIds = 'c' :: 'C' :: Nil, optionNames = "colour" :: "color" :: Nil,   argDescriptor = OptArg(colour, "COLOUR"),   explanation = "add filter of COLOUR"}
        ::  {optionIds = 'w':: Nil,         optionNames = "width" :: Nil,               argDescriptor = ReqArg(width, "WIDTH"),     explanation = "image WIDTH" }
        ::  {optionIds = 'h':: Nil,         optionNames = "height" :: Nil,              argDescriptor = ReqArg(height, "HEIGHT"),   explanation = "image HEIGHT" }
        :: Nil

    def assertSuccessWith(x: Validation[a, e], test: a -> Bool): Bool  = match x {
        case Success(x1) => test(x1)
        case Failure(_) => false
    }

    def assertFailureWith(x: Validation[a, e], test: Nel[e] -> Bool): Bool  = match x {
        case Success(_) => false
        case Failure(es) => test(es)
    }

    /////////////////////////////////////////////////////////////////////////////
    // No command line arguments                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testNoArgs01(): Bool & Impure =
        let args = [];
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            x.options == Nil and x.nonOptions == Nil

    /////////////////////////////////////////////////////////////////////////////
    // Non-options                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testNonOptions01(): Bool & Impure =
        let args = ["non_option1"];
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            x.options == Nil and x.nonOptions == "non_option1" :: Nil

    @test
    def testNonOptions02(): Bool & Impure =
        let args = ["non_option1", "non_option2"];
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            x.options == Nil and x.nonOptions == "non_option1" :: "non_option2" :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // Single char options                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testCharArgs01(): Bool & Impure =
        let args = ["-X"];
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == HiRes :: Nil) and (x.nonOptions == Nil)

    @test
    def testCharArgs02(): Bool & Impure =
        let args = ["-Xc"];
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == HiRes :: Colour(None) :: Nil) and (x.nonOptions == Nil)

    @test
    def testCharArgs03(): Bool & Impure =
        let args = ["-XcBlue"];
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == HiRes :: Colour(Some("Blue")) :: Nil) and (x.nonOptions == Nil)

    @test
    def testCharArgs04(): Bool & Impure =
        let args = ["-X", "-c"];
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == HiRes :: Colour(None) :: Nil) and (x.nonOptions == Nil)

    @test
    def testCharArgs05(): Bool & Impure =
        let args = ["-X", "-cBlue"];
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == HiRes :: Colour(Some("Blue")) :: Nil) and (x.nonOptions == Nil)

    @test
    def testCharArgs06(): Bool & Impure =
        let args = ["-X", "-cBlue", "non_option1"];
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == HiRes :: Colour(Some("Blue")) :: Nil) and (x.nonOptions == "non_option1" :: Nil)

    @test
    def testCharArgs07(): Bool & Impure =
        let args = ["-X", "-cBlue", "-w1024", "-h768"];
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == HiRes :: Colour(Some("Blue")) :: Width(1024) :: Height(768) :: Nil)
                and (x.nonOptions == Nil)

    @test
    def testCharArgs08(): Bool & Impure =
        let args = ["-X", "-cBlue", "-w1024", "-h768", "non_option1"];
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == HiRes :: Colour(Some("Blue")) :: Width(1024) :: Height(768) :: Nil)
                and (x.nonOptions == "non_option1" :: Nil)

    @test
    def testCharArgs09(): Bool & Impure =
        /// "-m" not defined
        let args = ["-m"];
        getOpt(Permute, options(), args) `assertFailureWith` x ->
            Nel.length(x) == 1

    @test
    def testCharArgs10(): Bool & Impure =
        /// "-m" not defined
        let args = ["-Xm"];
        getOpt(Permute, options(), args) `assertFailureWith` x ->
            Nel.length(x) == 1

    @test
    def testCharArgs11(): Bool & Impure =
        /// "-m" not defined
        let args = ["-X", "-m"];
        getOpt(Permute, options(), args) `assertFailureWith` x ->
            Nel.length(x) == 1

    /////////////////////////////////////////////////////////////////////////////
    // String / name options                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testStringArgs01(): Bool & Impure =
        let args = ["--hires"];
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == HiRes :: Nil) and (x.nonOptions == Nil)

    @test
    def testStringArgs02(): Bool & Impure =
        let args = ["--hires", "--colour"];
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == HiRes :: Colour(None) :: Nil) and (x.nonOptions == Nil)

    @test
    def testStringArgs03(): Bool & Impure =
        let args = ["--hires", "--colour=Blue"];
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == HiRes :: Colour(Some("Blue")) :: Nil) and (x.nonOptions == Nil)

    @test
    def testStringArgs04(): Bool & Impure =
        /// Alternative spelling
        let args = ["--hires", "--color"];
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == HiRes :: Colour(None) :: Nil) and (x.nonOptions == Nil)

    @test
    def testStringArgs05(): Bool & Impure =
    /// Alternative spelling
        let args = ["--hires", "--color=Blue"];
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == HiRes :: Colour(Some("Blue")) :: Nil) and (x.nonOptions == Nil)

    @test
    def testStringArgs06(): Bool & Impure =
        let args = ["--hires", "--colour=Blue", "non_option1"];
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == HiRes :: Colour(Some("Blue")) :: Nil) and (x.nonOptions == "non_option1" :: Nil)

    @test
    def testStringArgs07(): Bool & Impure =
        let args = ["--hires", "--colour=Blue", "--width=1024", "--height=768"];
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == HiRes :: Colour(Some("Blue")) :: Width(1024) :: Height(768) :: Nil)
                and (x.nonOptions == Nil)

    @test
    def testStringArgs08(): Bool & Impure =
        let args = ["--hires", "--colour=Blue", "--width=1024", "--height=768", "non_option1"];
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == HiRes :: Colour(Some("Blue")) :: Width(1024) :: Height(768) :: Nil)
                and (x.nonOptions == "non_option1" :: Nil)

    @test
    def testStringArgs09(): Bool & Impure =
        /// "--multiple" not defined
        let args = ["--multiple"];
        getOpt(Permute, options(), args) `assertFailureWith` x ->
            Nel.length(x) == 1

    @test
    def testStringArgs10(): Bool & Impure =
        /// "--multiple" not defined
        let args = ["--hires", "--multiple"];
        getOpt(Permute, options(), args) `assertFailureWith` x ->
            Nel.length(x) == 1

    @test
    def testStringArgs11(): Bool & Impure =
        /// "--multiple" not defined
        let args = ["--hires", "--multiple", "test_option1"];
        getOpt(Permute, options(), args) `assertFailureWith` x ->
            Nel.length(x) == 1

    /////////////////////////////////////////////////////////////////////////////
    // Mixed short and name options                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testMixedArgs01(): Bool & Impure =
        let args = ["--hires", "-c"];
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == HiRes :: Colour(None) :: Nil) and (x.nonOptions == Nil)

    @test
    def testMixedArgs02(): Bool & Impure =
        let args = ["-X", "--colour"];
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == HiRes :: Colour(None) :: Nil) and (x.nonOptions == Nil)

    @test
    def testMixedArgs03(): Bool & Impure =
        let args = ["--hires", "-cBlue"];
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == HiRes :: Colour(Some("Blue")) :: Nil) and (x.nonOptions == Nil)

    @test
    def testMixedArgs04(): Bool & Impure =
        /// Alternative spelling
        let args = ["-X", "--color"];
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == HiRes :: Colour(None) :: Nil) and (x.nonOptions == Nil)

    @test
    def testMixedArgs05(): Bool & Impure =
    /// Alternative spelling
        let args = ["-X", "--color=Blue"];
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == HiRes :: Colour(Some("Blue")) :: Nil) and (x.nonOptions == Nil)

    @test
    def testMixedArgs06(): Bool & Impure =
        let args = ["-X", "--colour=Blue", "non_option1"];
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == HiRes :: Colour(Some("Blue")) :: Nil) and (x.nonOptions == "non_option1" :: Nil)

    @test
    def testMixedArgs07(): Bool & Impure =
        let args = ["-X", "--colour=Blue", "-w1024", "-h768"];
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == HiRes :: Colour(Some("Blue")) :: Width(1024) :: Height(768) :: Nil)
                and (x.nonOptions == Nil)

    @test
    def testMixedArgs08(): Bool & Impure =
        let args = ["-X", "-cBlue", "--width=1024", "--height=768", "non_option1"];
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == HiRes :: Colour(Some("Blue")) :: Width(1024) :: Height(768) :: Nil)
                and (x.nonOptions == "non_option1" :: Nil)

    @test
    def testMixedArgs09(): Bool & Impure =
        /// "-m" not defined
        let args = ["--hires", "-m"];
        getOpt(Permute, options(), args) `assertFailureWith` x ->
            Nel.length(x) == 1

    @test
    def testMixedArgs10(): Bool & Impure =
        /// "--multiple" not defined
        let args = ["-X", "--multiple"];
        getOpt(Permute, options(), args) `assertFailureWith` x ->
            Nel.length(x) == 1

    @test
    def testMixedArgs11(): Bool & Impure =
        /// "--multiple" not defined
        let args = ["-X", "--multiple", "test_option1"];
        getOpt(Permute, options(), args) `assertFailureWith` x ->
            Nel.length(x) == 1

    /////////////////////////////////////////////////////////////////////////////
    // Prefix of long option                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testPrefixArgs01(): Bool & Impure =
        /// Fail: Could be "hires" or "height"
        let args = ["--h=1000"];
        getOpt(Permute, options(), args) `assertFailureWith` x ->
            Nel.length(x) == 1

    @test
    def testPrefixArgs02(): Bool & Impure =
        /// Success: Could be only be "hires"
        let args = ["--hi"];
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == HiRes :: Nil) and (x.nonOptions == Nil)

    @test
    def testPrefixArgs03(): Bool & Impure =
        /// Success: Could be only be "height"
        let args = ["--he=1200"];
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == Height(1200) :: Nil) and (x.nonOptions == Nil)

    @test
    def testPrefixArgs04(): Bool & Impure =
        /// Success: Could be only be "height"
        let args = ["--hi", "--he=1200", "test_option1"];
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == HiRes :: Height(1200) :: Nil) and (x.nonOptions == "test_option1" :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // Decoding option args                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testDecodeArgs01(): Bool & Impure =
        let args = ["--width=1400"];
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == Width(1400) :: Nil) and (x.nonOptions == Nil)

    @test
    def testDecodeArgs02(): Bool & Impure =
        let args = ["--width=fourteen_thousand"];
        getOpt(Permute, options(), args) `assertFailureWith` x ->
            Nel.length(x) == 1

    @test
    def testDecodeArgs03(): Bool & Impure =
        let args = ["--width=1400", "--height=1000"];
        getOpt(Permute, options(), args) `assertSuccessWith` x ->
            (x.options == Width(1400) :: Height(1000) :: Nil) and (x.nonOptions == Nil)

    @test
    def testDecodeArgs04(): Bool & Impure =
        let args = ["--width=fourteen_hundred", "--height=1000"];
        getOpt(Permute, options(), args) `assertFailureWith` x ->
            Nel.length(x) == 1

    @test
    def testDecodeArgs05(): Bool & Impure =
        let args = ["--width=1400", "--height=one_thousand"];
        getOpt(Permute, options(), args) `assertFailureWith` x ->
            Nel.length(x) == 1

    @test
    def testDecodeArgs06(): Bool & Impure =
        let args = ["--width=fourteen_hundred", "--height=one_thousand"];
        getOpt(Permute, options(), args) `assertFailureWith` x ->
            Nel.length(x) == 2

    /////////////////////////////////////////////////////////////////////////////
    // Preprocess                                                              //
    /////////////////////////////////////////////////////////////////////////////

    def squote(): String = "'"

    @test
    def preprocess01(): Bool & Impure =
        let args = [];
        GetOpt.preprocess({quoteOpen = squote(), quoteClose = squote(), stripQuoteMarks = false}, args)
            |> Array.toList == Nil

    @test
    def preprocess02(): Bool & Impure =
        let args = String.split("--version", "\s+");
        GetOpt.preprocess({quoteOpen = squote(), quoteClose = squote(), stripQuoteMarks = false}, args)
            |> Array.toList == "--version" :: Nil


    @test
    def preprocess03(): Bool & Impure =
        let args = String.split("--editor=vi", "\s+");
        GetOpt.preprocess({quoteOpen = squote(), quoteClose = squote(), stripQuoteMarks = false}, args)
            |> Array.toList == "--editor=vi" :: Nil

    @test
    def preprocess04(): Bool & Impure =
        /// Note - not quoted...
        let args = String.split("--editor=vs code", "\s+");
        GetOpt.preprocess({quoteOpen = squote(), quoteClose = squote(), stripQuoteMarks = false}, args)
            |> Array.toList == "--editor=vs" :: "code" :: Nil

    @test
    def preprocess05(): Bool & Impure =
        /// Note - not quoted...
        let args = String.split("--editor='vs code'", "\s+");
        GetOpt.preprocess({quoteOpen = squote(), quoteClose = squote(), stripQuoteMarks = false}, args)
            |> Array.toList == "--editor='vs code'" :: Nil

    @test
    def preprocess06(): Bool & Impure =
        let args = String.split("--editor='vs code'", "\s+");
        GetOpt.preprocess({quoteOpen = squote(), quoteClose = squote(), stripQuoteMarks = true}, args)
            |> Array.toList == "--editor=vs code" :: Nil

    @test
    def preprocess07(): Bool & Impure =
        let args = String.split("--version --editor='vs code'", "\s+");
        GetOpt.preprocess({quoteOpen = squote(), quoteClose = squote(), stripQuoteMarks = false}, args)
            |> Array.toList == "--version" :: "--editor='vs code'" :: Nil

    @test
    def preprocess08(): Bool & Impure =
        let args = String.split("--version --editor='vs code' 'D:/my file1.txt'", "\s+");
        GetOpt.preprocess({quoteOpen = squote(), quoteClose = squote(), stripQuoteMarks = false}, args)
            |> Array.toList == "--version" :: "--editor='vs code'" :: "'D:/my file1.txt'" :: Nil

    @test
    def preprocess09(): Bool & Impure =
        let args = String.split("--version --editor='vs code' 'D:/my file1.txt'", "\s+");
        GetOpt.preprocess({quoteOpen = squote(), quoteClose = squote(), stripQuoteMarks = true}, args)
            |> Array.toList == "--version" :: "--editor=vs code" :: "D:/my file1.txt" :: Nil

}