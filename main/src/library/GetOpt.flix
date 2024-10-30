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

///
/// Parse the list of command line options supplied to the program.
///
/// Acknowledgement: this is a port of Sven Panne's `System.Console.GetOpt` in the Haskell Base libraries.
///


mod GetOpt {

    ///
    /// `ArgOrder` defines how to parse further arguments that follow non-options.
    ///
    /// * `RequireOrder` after the first non-option all remain arguments are treated as non-options
    /// * `Permute` options and non-options are freely interspersed in the input stream
    /// * `ReturnInOrder` turn non-options into options by applying the supplied function
    ///
    pub enum ArgOrder[a] {
        case RequireOrder
        case Permute
        case ReturnInOrder(String -> a)
    }

    ///
    /// `ArgDescr` defines whether or not the option takes an argument.
    ///
    /// * `NoArg` expects no argument. The constructor takes a value of type `a` indicating the option that has been recognized.
    /// * `ReqArg` mandates an argument. The constructor takes a function to decode the argument and description string.
    /// * `OptArg` optionally requires an argument. The constructor takes a function to decode the argument and description string.
    ///
    /// The "decode" functions are expected to produce `Some(_)` when decoding is successful, and `None` to indicate
    /// failure.
    ///
    pub enum ArgDescr[a] {
        case NoArg(a)
        case ReqArg(String -> Option[a], String)
        case OptArg(Option[String] -> Option[a], String)
    }

    ///
    /// The kind of a command line option (internal).
    ///
    /// * `Opt(a)` is an option producing a value of type `a`
    /// * `UnreqOp` is an unrecognized option
    /// * `NonOpt` is a non-option, returned to the user as a String
    /// * `EndOfOpts` signals the end of options in the decoded command line String
    /// * `OptErr` is the error case with a String error message
    ///
    enum OptionKind[a] {
        case Opt(a)
        case UnreqOpt(String)
        case NonOpt(String)
        case EndOfOpts
        case OptErr(String)
    }

    ///
    /// An `OptionDescr` describes a single command line option.
    ///
    /// * `optionIds` is a list of single character abbreviations identifying the option
    /// * `optionNames` is a list of long names identifying the option
    /// * `argDescriptor` defines the format of the option
    /// * `explanation` is the description of the option printed to the user by the function `usageInfo`
    ///
    pub type alias OptionDescr[a] =
        { optionIds = List[Char]
        , optionNames = List[String]
        , argDescriptor = ArgDescr[a]
        , explanation = String
        }


    ///
    /// Return a formatted string describing the usage of the command.
    ///
    /// Typically this output is used by "--help"
    ///
    pub def usageInfo(header: String, optionDescriptors: List[OptionDescr[a]]): String =
        let paste = (x, y, z) -> "  ${x}  ${y}  ${z}";
        let (shorts, longs, ds) = List.flatMap(formatOpt, optionDescriptors) |> List.unzip3;
        let table = List.zipWith3(paste, sameLen(shorts), sameLen(longs), ds);
        String.unlines(header :: table)

    ///
    /// Returns `xs` with each string right-padded with spaces to the length of the longest string.
    ///
    def sameLen(xs: List[String]): List[String] =
        let width = List.foldLeft((ac, x) -> Int32.max(ac, String.length(x)), 0, xs);
        List.map(String.padRight(width, ' '), xs)

    ///
    /// Format each option descriptor.
    ///
    def formatOpt(opts: OptionDescr[a]): List[(String, String, String)] =
        let shorts  = List.map(formatShort(opts#argDescriptor),  opts#optionIds)   |> sepBy(',');
        let longs   = List.map(formatLong(opts#argDescriptor),   opts#optionNames) |> sepBy(',');
        match String.lines(opts#explanation) {
            case Nil => (shorts, longs, "") :: Nil
            case d :: ds => (shorts, longs, d) :: List.map(x -> ("", "", x), ds)
        }

    ///
    /// Returns the string formed by separating each element in `ss` with the Char `ch` and a space.
    ///
    def sepBy(ch: Char, ss: List[String]): String = String.intercalate("${ch} ", ss)

    ///
    /// Format a single character option.
    ///
    def formatShort(argDescr: ArgDescr[a], short: Char): String = match argDescr {
        case ArgDescr.NoArg(_)           => "-${short}"
        case ArgDescr.ReqArg(_, descr)   => "-${short} ${descr}"
        case ArgDescr.OptArg(_, descr)   => "-${short}[${descr}]"
    }

    ///
    /// Format a named option.
    ///
    def formatLong(argDescr: ArgDescr[a], long: String): String = match argDescr {
        case ArgDescr.NoArg(_)           => "--${long}"
        case ArgDescr.ReqArg(_, descr)   => "--${long}=${descr}"
        case ArgDescr.OptArg(_, descr)   => "--${long}[=${descr}]"
    }

    ///
    /// Decode the list of command line options supplied to the program.
    ///
    /// `ordering` mandates how processing of options and non-options is handled.
    ///
    /// `optDescriptors` is a list of processors for decoding individual options.
    ///
    /// `sourceArgs` should be the list of command line arguments supplied to the program.
    ///
    /// If successful, `getOpt` returns lists of decoded options and non-options. If unsuccessful it
    /// returns a non-empty list of error messages, with unknown options considered to be errors.
    ///
    pub def getOpt(ordering: ArgOrder[a],
                   optDescriptors: List[OptionDescr[a]],
                   sourceArgs: List[String]): Validation[String, {options = List[a], nonOptions = List[String]}] =
        let answers = getOpt1(ordering, optDescriptors, sourceArgs);
        match answers#errors ::: List.map(errUnrec, answers#unknowns) {
            case Nil => Validation.Success({options = answers#options, nonOptions = answers#nonOptions})
            case e :: es => match List.toNec(es) {
                case None    => Validation.Failure(Nec.singleton(e))
                case Some(c) => Validation.Failure(Nec.cons(e, c))
            }
        }

    ///
    /// This is a more general version of `getOpt` that returns all the results of decoding the command line
    /// options without distinguishing whether the decoding is successful or a failure.
    ///
    /// Client code calling this function, rather than `getOpt`, is free to process or ignore the results collected
    /// in `unknowns` and `errors` which would indicate problems with the decoding if either were not empty.
    ///
    pub def getOpt1(ordering: ArgOrder[a],
                    optDescriptors: List[OptionDescr[a]],
                    sourceArgs: List[String]): {options = List[a], nonOptions = List[String], unknowns = List[String], errors = List[String]} =
        let (os, xs, us, es) = getOpt1Helper(ordering, optDescriptors, sourceArgs);
        {options = os, nonOptions = xs, unknowns = us, errors = es}


    ///
    /// Helper for `getOpt1`
    ///
    def getOpt1Helper(ordering: ArgOrder[a],
                    optDescrs: List[OptionDescr[a]],
                    sourceArgs: List[String]): (List[a], List[String], List[String], List[String]) = match sourceArgs {
        case Nil => (Nil, Nil, Nil, Nil)
        case arg :: args => {
            let (opt, rest)         = getNext(arg, optDescrs, args);
            let (os, xs, us, es)    = getOpt1Helper(ordering, optDescrs, rest);
            match (opt, ordering) {
                case (OptionKind.Opt(o),       _)                           => (o :: os, xs, us, es)
                case (OptionKind.UnreqOpt(u),  _)                           => (os, xs, u :: us, es)        // u is added to
                case (OptionKind.NonOpt(x),    ArgOrder.RequireOrder)       => (Nil, x :: rest, Nil, Nil)
                case (OptionKind.NonOpt(x),    ArgOrder.Permute)            => (os, x :: xs, us, es)
                case (OptionKind.NonOpt(x),    ArgOrder.ReturnInOrder(f))   => (f(x) :: os, xs, us, es)
                case (OptionKind.EndOfOpts,    ArgOrder.RequireOrder)       => (Nil, rest, Nil, Nil)
                case (OptionKind.EndOfOpts,    ArgOrder.Permute)            => (Nil, rest, Nil, Nil)
                case (OptionKind.EndOfOpts,    ArgOrder.ReturnInOrder(f))   => (List.map(f, rest), Nil, Nil, Nil)
                case (OptionKind.OptErr(e),    _)                           => (os, xs, us, e :: es)
            }
        }
    }

    ///
    /// Returns the Option cacluated by decoding `arg` and a list of remaining tokens.
    ///
    /// Depending on whether the option decoded for `arg` takes an argument, we might look
    /// into the token stream for it.
    ///
    def getNext(arg: String,
                optDescrs: List[OptionDescr[a]],
                tokens: List[String]): (OptionKind[a], List[String]) = match arg {
        case "--"                                                                   => (OptionKind.EndOfOpts, tokens)
        case s if String.startsWith({prefix = "--"}, s)                             => longOpt(String.dropLeft(2, s), optDescrs, tokens)
        case s if String.startsWith({prefix = "-"}, s) and String.length(s) >= 2    => shortOpt(String.charAt(1, s), String.dropLeft(2, s), optDescrs, tokens)
        case s                                                                      => (OptionKind.NonOpt(s), tokens)
    }

    /// "--" has been parsed
    /// `xstr` is the rest of the current token
    def longOpt(xstr: String, optDescrs: List[OptionDescr[a]], tokens: List[String]): (OptionKind[a], List[String]) =
        let (optname, arg)  = String.breakOnLeft({substr = "="}, xstr);
        let optStr      = "--${optname}";
        match findOptionByName(optname, optDescrs) {
            case Ok(opt) => match (opt#argDescriptor, deconsLeft(arg), tokens) {
                case (ArgDescr.NoArg(a),     None,               rest)          => (OptionKind.Opt(a), rest)
                case (ArgDescr.NoArg(_),     Some(('=', _)),     rest)          => (errNoArgReq(optStr), rest)
                case (ArgDescr.ReqArg(_, d), None,               Nil)           => (errReq(d, optStr), Nil)
                case (ArgDescr.ReqArg(f, _), None,               r :: rest)     => (decodeArg(f(r), optStr), rest)
                case (ArgDescr.ReqArg(f, _), Some(('=', ss)),    rest)          => (decodeArg(f(ss), optStr), rest)
                case (ArgDescr.OptArg(f, _), None,               rest)          => (decodeArg(f(None), optStr), rest)
                case (ArgDescr.OptArg(f, _), Some(('=', ss)),    rest)          => (decodeArg(f(Some(ss)), optStr), rest)
                case (_,                        _,                  rest)       => (OptionKind.UnreqOpt("--${xstr}"), rest)
            }
            case Err(GetOptionFailure.Ambiguous(options))                            => (errAmbig(options, optStr), tokens)
            case Err(GetOptionFailure.NotFound)                                      => (OptionKind.UnreqOpt("--${xstr}"), tokens)
        }

    /// Returns the "left view" of String `s`.
    ///
    /// If `s` is the empty String, `None` is returned.
    ///
    /// Otherwise return `Some(c, rs)` where `c` is the first character of `s` and `rs` is the rest of `s`.
    ///
    def deconsLeft(s: String): Option[(Char, String)] =
        if (String.length(s) > 0)
            Some((String.charAt(0, s), String.dropLeft(1, s)))
        else
            None

    /// "-" has been parsed (i.e. x0).
    /// `xc` is the first char of the current token after the dash, which should identify the option.
    /// `xstr` is the rest of the current token
    /// Never see "=" for a short arg
    def shortOpt(xc: Char, xstr: String, optDescrs: List[OptionDescr[a]], tokens: List[String]): (OptionKind[a], List[String]) =
        let optStr  = "-${xc}";
        match findOptionById(xc, optDescrs) {
            case Ok(opt) => match (opt#argDescriptor, xstr, tokens)  {
                case (ArgDescr.NoArg(a), "", rest)           => (OptionKind.Opt(a), rest)
                case (ArgDescr.NoArg(a), str, rest)          => (OptionKind.Opt(a), "-${str}" :: rest)
                case (ArgDescr.ReqArg(_, d), "", Nil)        => (errReq(d, optStr), Nil)
                case (ArgDescr.ReqArg(f, _), "", r :: rest)  => (decodeArg(f(r), optStr), rest)
                case (ArgDescr.ReqArg(f, _), str, rest)      => (decodeArg(f(str), optStr), rest)
                case (ArgDescr.OptArg(f, _), "", rest)       => (decodeArg(f(None), optStr), rest)
                case (ArgDescr.OptArg(f, _), str, rest)      => (decodeArg(f(Some(str)), optStr), rest)
            }
            case Err(GetOptionFailure.Ambiguous(options))            => (errAmbig(options, optStr), tokens)
            case Err(GetOptionFailure.NotFound) if xstr == ""        => (OptionKind.UnreqOpt(optStr), tokens)
            case Err(GetOptionFailure.NotFound)                      => (OptionKind.UnreqOpt(optStr), "-${xstr}" :: tokens)
        }

    ///
    /// Decode the argument which should be `Some(_)` otherwise decoding has failed.
    ///
    def decodeArg(x: Option[a], optStr: String): OptionKind[a] = match x {
        case Some(x1) => OptionKind.Opt(x1)
        case None => errDecode(optStr)
    }

    /// Looking up options

    ///
    /// A single char or an option name should identify a single option descriptor,
    /// but if the list of option descriptors is ill-formed (different option descriptors
    /// share identifiers) or the command line string is ill-formed (i.e. uses an option that
    /// is not defined) looking up the option will cause a error.
    ///
    enum GetOptionFailure[a] {
        case Ambiguous(Nel[OptionDescr[a]])
        case NotFound
    }

    ///
    /// Return the option descriptor identified by the String `optname`.
    ///
    /// If `optname` exactly determines a single option descriptor it will be returned, otherwise
    /// `optname` will be used as a prefix to try to identify a single option descriptor.
    ///
    /// This function returns `Err(NotFound)` if `xc` is not recognized.
    ///
    /// It returns `Err(Ambiguous(xs))` if `xc` identifies more than one option descriptor.
    ///
    def findOptionByName(optname: String, optDescrs: List[OptionDescr[a]]): Result[GetOptionFailure[a], OptionDescr[a]] =
        let getWith     = p -> List.filterMap(o -> { let xs = o#optionNames; if (List.exists(p(optname), xs)) Some(o) else None}, optDescrs);
        // Exact match of `optname` ...
        let exact       = getWith((x,y) -> x == y);
        // Otherwise use `optname` as a prefix...
        let options     = if (List.isEmpty(exact)) getWith((sub, s1) -> String.startsWith({prefix = sub}, s1)) else exact;
        match options {
            case optd :: Nil    => Ok(optd)
            case x :: xs        => Err(GetOptionFailure.Ambiguous(Nel.Nel(x, xs)))
            case Nil            => Err(GetOptionFailure.NotFound)
        }

    ///
    /// Return the option descriptor identified by the Char `xc`.
    ///
    /// This function returns `Err(NotFound)` if `xc` is not recognized.
    ///
    /// It returns `Err(Ambiguous(xs))` if `xc` identifies more than one option descriptor.
    ///
    def findOptionById(xc: Char, optDescrs: List[OptionDescr[a]]): Result[GetOptionFailure[a], OptionDescr[a]] =
        // This finds 0, 1, or more results. "more" is a failure (ambiguous), 0 is ignored, 1 is success.
        let options = List.filterMap(o -> { let ss = o#optionIds; if (List.exists(s -> s == xc, ss)) Some(o) else None}, optDescrs);
        match options {
            case optd :: Nil    => Ok(optd)
            case x :: xs        => Err(GetOptionFailure.Ambiguous(Nel.Nel(x, xs)))
            case Nil            => Err(GetOptionFailure.NotFound)
        }


    /// Error formatting functions

    ///
    /// Return an error indicating the option is ambiguous and could identify more than one option descriptor.
    ///
    def errAmbig(ods: Nel[OptionDescr[a]], optStr: String): OptionKind[a] =
        let header = "option '${optStr}' is ambiguous; could be one of:";
        OptionKind.OptErr(usageInfo(header, Nel.toList(ods)))

    ///
    /// Return an error indicating the option requires an argument that was not supplied.
    ///
    def errReq(d: String, optStr: String): OptionKind[a] =
        OptionKind.OptErr("option '${optStr}' requires an argument ${d}\n")

    ///
    /// Return an error indicating the option name was not recognized.
    ///
    def errUnrec(optStr: String): String = "unrecognized option '${optStr}'\n"

    ///
    /// Return an error indicating the option was supplied an argument when it does not want one.
    ///
    def errNoArgReq(optStr: String): OptionKind[a] =
        OptionKind.OptErr("option '${optStr}' doesn't allow an argument\n")

    ///
    /// Return an error indicating the option was supplied an argument it could not decode.
    ///
    def errDecode(optStr: String): OptionKind[a] =
        OptionKind.OptErr("option '${optStr}' could not decode its argument\n")


    /// Preprocessing the commandline args.

    ///
    /// Preprocess the command line arguments before parsing them.
    ///
    /// Arguments supplied as an `List[String]` to the program are simply derived from the input split on space.
    /// This does not account for, for example, Windows file names which may include space.
    ///
    /// `preprocess` is a simple function that can be used to "rejoin" command line arguments if they were
    /// joined by "quotation marks" in the user supplied string (quotations marks are configurable and do not
    /// have to be double quotes).
    ///
    pub def preprocess(options: {quoteOpen = String, quoteClose = String, stripQuoteMarks = Bool}, args: List[String]): List[String] =
        let args1 = groupWhenQuoted(options#quoteOpen, options#quoteClose, args);
        if (options#stripQuoteMarks)
            removeQuoteMarks(options#quoteOpen, options#quoteClose, args1)
        else
            args1

    ///
    /// Returns `args` with tokens inside quotes grouped together as a single token.
    ///
    /// Starts in the initial state, outside a quotation.
    ///
    def groupWhenQuoted(quoteOpen: String, quoteClose: String, args: List[String]): List[String] =
        groupWhenQuotedHelper(quoteOpen, quoteClose, args, ks -> ks)

    ///
    /// Helper for `groupWhenQuoted`.
    ///
    /// Here we are in the initial state, outside a quotation.
    ///
    def groupWhenQuotedHelper(quoteOpen: String, quoteClose: String, tokens: List[String], k: List[String] -> List[String] \ ef): List[String] \ ef =
        match tokens {
            case Nil => k(Nil)
            case x :: rest if String.contains({substr = quoteOpen}, x) and String.endsWith({suffix = quoteClose}, x) => groupWhenQuotedHelper(quoteOpen, quoteClose, rest, ks -> k(x :: ks))
            case x :: rest if String.contains({substr = quoteOpen}, x) => groupWhenQuotedHelperInner(quoteOpen, quoteClose, rest, x, ks -> k(ks))
            case x :: rest => groupWhenQuotedHelper(quoteOpen, quoteClose, rest, ks -> k(x :: ks))
        }

    ///
    /// Helper for `groupWhenQuoted`.
    ///
    /// Here we are "inside" a quotation, stored so far as `acc`.
    ///
    def groupWhenQuotedHelperInner(quoteOpen: String, quoteClose: String, tokens: List[String], acc: String, k: List[String] -> List[String] \ ef): List[String] \ ef =
        match tokens {
            case Nil => k(acc :: Nil)
            case x :: rest if String.contains({substr = quoteClose}, x) => groupWhenQuotedHelper(quoteOpen, quoteClose, rest, ks -> k((acc + " " +  x) :: ks))
            case x :: rest => groupWhenQuotedHelperInner(quoteOpen, quoteClose, rest, (acc + " " +  x), k)
        }

    ///
    /// Remove all quote marks from the list of tokens.
    ///
    def removeQuoteMarks(quoteOpen: String, quoteClose: String, args: List[String]): List[String] =
        args |> List.map(String.replace({src = quoteOpen}, {dst = ""}) >> String.replace({src = quoteClose}, {dst = ""}))

}
