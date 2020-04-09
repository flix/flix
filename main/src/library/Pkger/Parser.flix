namespace Parser {
    
    /// The basic signature of a parser. It's a function from a list of characters to the
    /// characters not yet parsed, and an user defined `a`.
    type alias Parser[a] = List[Char] -> Option[(List[Char], a)]
    
    /// Creates a parser which uses none of it's input, and have parsed `ret`
    ///
    /// # Examples
    ///
    /// ```
    /// String.toList("a") |> pure(0) == Some(('a' :: Nil, 0))
    /// ```
    pub def pure[a](
        ret: a
    ): Parser[a] = input -> Some((input, ret))

    /// Creates a parser which takes the first character from it's input, and outputs it
    ///
    /// Fails if the input was empty
    ///
    /// # Examples
    ///
    /// ```
    /// Nil |> char() == None
    /// ```
    ///
    /// ```
    /// String.toList("a") |> char() == Some((Nil, 'a'))
    /// ```
    ///
    /// ```
    /// String.toList("ab") |> char() == Some(('b' :: Nil, 'a'))
    /// ```
    pub def char(): Parser[Char] = input -> match input {
        case Nil => None
        case x :: xs => Some((xs, x: Char))
    }

    /// Given a parser `p`, creates a parser like `p`, except it fails if the predicate `f` is not
    /// true for a parsed value.
    ///
    /// # Examples
    ///
    /// ```
    /// String.toList("0") |> char().filter(Char.isDigit) == Some((Nil, '0'))
    /// ```
    ///
    /// ```
    /// String.toList("a") |> char().filter(Char.isDigit) == None
    /// ```
    pub def filter[a](
        p: Parser[a],
        f: a -> Bool
    ): Parser[a] = input -> match input.p() {
        case Some((xs, v)) => if (v.f()) {
            Some((xs, v))
        } else {
            None
        }
        case None => None
    }

    /// Creates a parser which takes the first character from it's input if that character is equal
    /// to `c`.
    ///
    /// # Examples
    ///
    /// ```
    /// String.toList("a") |> tag('a') == Some((Nil, 'a'))
    /// ```
    ///
    /// ```
    /// String.toList("b") |> tag('a') == None
    /// ```
    ///
    /// ```
    /// String.toList("ab") |> tag('a') == Some(('b' :: Nil, 'a'))
    /// ```
    pub def tag(c: Char): Parser[Char] = char().filter(d -> d == c)

    /// Given a parser `p`, returns a parser like `p`, except any value prased is mapped with the
    /// function `f`.
    ///
    /// # Examples
    ///
    /// ```
    /// String.toList("a") |> char().map(c -> c == 'a') == Some((Nil, true))
    /// ```
    ///
    /// ```
    /// String.toList("a") |> char().map(c -> if (c == 'a') {
    ///     'b'
    /// } else {
    ///     'y'
    /// })
    /// ==
    /// Some((Nil, 'b'))
    /// ```
    ///
    /// ```
    /// String.toList("abc") |> seperatedAny(char(), tag('b')).map(List.toString)
    /// ==
    /// Some((Nil, "ac"))
    /// ```
    pub def map[a, b](
        p: Parser[a],
        f: a -> b
    ): Parser[b] = input -> match input.p() {
        case Some((xs, v)) => Some((xs, v.f()))
        case None => None
    }

    /// Given parsers `lhs` and `rhs`, returns a parser which first tries to run `lhs` on the input,
    /// and if successfull tries to run `rhs` on the rest of the input. The new parser returns a
    /// tuple of the result from each parser if both parsers succeed. The new parser fails if
    /// either `lhs` or `rhs` fail.
    ///
    /// # Examples
    ///
    /// ```
    /// String.toList("ab") |> (tag('a') `and` tag('b')) == Some((Nil, ('a', 'b')))
    /// ```
    ///
    /// ```
    /// String.toList("aa") |> (tag('a') `and` tag('b')) == None
    /// ```
    pub def and[a, b](
        lhs: Parser[a],
        rhs: Parser[b]
    ): Parser[(a, b)] = input -> match input.lhs() {
        case Some((xs, v)) => match xs.rhs() {
            case Some((ys, u)) => Some((ys, (v, u)))
            case None => None
        }
        case None => None
    }

    /// Given parsers `lhs` and `rhs`, returns a parser which first tries to run `lsh` on the input,
    /// and if it fails tries to run `rhs` on the input. The new parser returns the output of the
    /// first parser if it is successfull, else returns the output of the second parser.
    ///
    /// # Examples
    ///
    /// ```
    /// String.toList("ab") |> (tag('a') `or` tag('b')) == Some(('b' :: Nil, 'a'))
    /// ```
    ///
    /// ```
    /// String.toList("ba") |> (tag('a') `or` tag('b')) == Some(('a' :: Nil, 'b'))
    /// ```
    ///
    /// ```
    /// String.toList("zz") |> (tag('a') `or` tag('b')) == None
    /// ```
    pub def or[a](
        lhs: Parser[a],
        rhs: Parser[a]
    ): Parser[a] = input -> match input.lhs() {
        case None => input.rhs()
        case ret => ret
    }

    /// Given a parser `p`, and a function `f` from it's output to a new parser, creates a new
    /// parser which runs `p`, and if successful uses `f` to create a new parser `g`. `g` is then
    /// run on tht rest of the input.
    ///
    /// # Examples
    ///
    /// ```
    /// let parser = (char() `and` char())
    ///     .then(match (lhs, rhs) -> tag('+')
    ///     .map(v -> lhs :: v :: rhs :: Nil));
    /// String.toList("12+") |> parser == Some((Nil, '1' :: '+' :: '2' :: Nil))
    /// ```
    pub def then[a, b](
        p: Parser[a],
        f: a -> Parser[b]
    ): Parser[b] = input -> match input.p() {
        case Some((xs, v)) => {
            let res = f(v);
            res(xs)
        }
        case None => None
    }

    /// Given a parser `p`, creates a parser which fails whenever `p` would have succeded, and
    /// succeds whenever `p` would have failed.
    ///
    /// # Examples
    ///
    /// ```
    /// String.toList("a") |> not(tag('a')) == None
    /// ```
    ///
    /// ```
    /// String.toList("b") |> not(tag('a')) == Some(('b' :: Nil, ()))
    /// ```
    ///
    pub def not[a](p: Parser[a]): Parser[Unit] = input -> match input.p() {
        case Some(_) => None
        case None => Some((input, ()))
    }


    /// Given a parser `p`, returns a parser like `p`, except it never fails. If `p` would have
    /// failed, the new parser takes no characters from the input, and returns None.
    ///
    /// # Examples
    ///
    /// ```
    /// String.toList("a") |> optional(tag('a')) == Some((Nil, Some('a')))
    /// ```
    ///
    /// ```
    /// String.toList("b") |> optional(tag('a')) == Some(('b' :: Nil, None))
    /// ```
    pub def optional[a](
        p: Parser[a]
    ): Parser[Option[a]] = input -> match input.p() {
        case None => Some((input, None))
        case Some((xs, parsed)) => Some((xs, Some(parsed)))
    }

    /// Given a parser `p`, removes the characters `p` would have used from input and returns Unit
    ///
    /// # Examples
    ///
    /// ```
    /// String.toList("a") |> skip(char()) == Some((Nil, ()))
    /// ```
    pub def skip[a](
        p: Parser[a]
    ): Parser[Unit] = input -> match input.p() {
        case None => None
        case Some((xs, _)) => Some((xs, ()))
    }

    /// Given parsers `lhs` and `rhs`, creates a parse which runs both parsers on the input, but
    /// throws away the output of `lhs`.
    ///
    /// # Examples
    ///
    /// ```
    /// String.toList("abc") |> (char() `skipLeft` char())
    /// ==
    /// Some(('c' :: Nil, 'b'))
    /// ```
    ///
    /// ```
    /// String.toList("abc") |> (tag('b') `skipLeft` char()) == None
    /// ```
    pub def skipLeft[a, b](
        lhs: Parser[a],
        rhs: Parser[b]
    ): Parser[b] = input -> match input.lhs() {
        case None => None
        case Some((xs, _)) => xs |> rhs
    }

    /// Given parsers `lhs` and `rhs`, creates a parse which runs both parsers on the input, but
    /// throws away the output of `rhs`.
    ///
    /// # Examples
    ///
    /// ```
    /// String.toList("abc") |> (char() `skipRight` char())
    /// ==
    /// Some(('c' :: Nil, 'a'))
    /// ```
    ///
    /// ```
    /// String.toList("abc") |> (char() `skipRight` tag('a')) == None
    /// ```
    pub def skipRight[a, b](
        lhs: Parser[a],
        rhs: Parser[b]
    ): Parser[a] = input -> match input.lhs() {
        case None => None
        case Some((xs, parsed)) => xs |> rhs.map(_ -> parsed)
    }

    /// Given a parser `p`, and a number `n`, creates a parser which runs `p` `n` times on the
    /// input, and returns a list of the result. If `n` is zero, returns a parser which takes no
    /// elements from input, and returns `Nil`.
    ///
    /// NOTE: A negative `n` is treated as zero.
    ///
    /// # Examples
    ///
    /// ```
    /// String.toList("abc") |> char().take(2) == Some(('c' :: Nil, 'a' :: 'b' :: Nil))
    /// ```
    ///
    /// ```
    /// String.toList("a") |> char().take(2) == None
    /// ```
    ///
    /// ```
    /// String.toList("a") |> char().take(0) == Some(('a' :: Nil, Nil))
    /// ```
    pub def take[a](p: Parser[a], n: Int): Parser[List[a]] = if (n <= 0) {
        pure(Nil)
    } else {
        (p `and` take(p, n - 1)).map(match (last, rest) -> last :: rest)
    }

    /// Given parsers `p` and `sep`, and a number `n`, returns a parser which runs `p` `n` times on
    /// the input, interlaced with `sep`. The new parser returns a list of values parsed with 'p',
    /// and throws away the values parsed with `sep`. If `n` is zero, returns a parser which takes
    /// no elements from the input, and returns `Nil`.
    ///
    /// NOTE: A negative `n` is treated as zero.
    ///
    /// # Examples
    ///
    /// ```
    /// String.toList("a.b.c") |> seperatedTake(char(), tag('.'), 2)
    /// ==
    /// Some(('.' :: 'c' :: Nil, 'a' :: 'b' :: Nil))
    /// ```
    ///
    /// ```
    /// String.toList("a.") |> seperatedTake(char(), tag('.'), 2)
    /// ==
    /// None
    /// ```
    ///
    /// ```
    /// String.toList(".a") |> seperatedTake(char(), tag('.'), 0)
    /// ==
    /// Some(('.' :: 'a' :: Nil, Nil))
    /// ```
    pub def seperatedTake[a, b](
        p: Parser[a],
        sep: Parser[b],
        n: Int
    ): Parser[List[a]] = if (n <= 0) {
        pure(Nil)
    } else if (n == 1) {
        p.map(e -> e :: Nil)
    } else {
        (p.skipRight(sep) `and` seperatedTake(p, sep, n - 1)).map(match (e, rest) -> e :: rest)
    }

    /// Given a parsers `p` and `sep`, and a number `n`, returns a parser which runs `p` `n` times
    /// interlaced with `sep`. If `sep` can successfully be run after the last run of `p`, this is
    /// done. The new parser returns a list of the elements parsed by `p`, and throws away the
    /// elements parsed by `sep`. If `n` is zero, a parser is returned which will once take `sep`
    /// from the output, if it`s present, and return Nil.
    ///
    /// NOTE: A negative `n` is treated as zero.
    ///
    /// # Examples
    ///
    /// ```
    /// String.toList("a.b.c") |> tailSeperatedTake(char(), tag('.'), 2)
    /// ==
    /// Some(('c' :: Nil, 'a' :: 'b' :: Nil))
    /// ```
    ///
    /// ```
    /// String.toList("a.") |> tailSeperatedTake(char(), tag('.'), 2)
    /// ==
    /// None
    /// ```
    ///
    /// ```
    /// String.toList(".a") |> tailSeperatedTake(char(), tag('.'), 0)
    /// ==
    /// Some(('a' :: Nil, Nil))
    /// ```
    pub def tailSeperatedTake[a, b](
        p: Parser[a],
        sep: Parser[b],
        n: Int
    ): Parser[List[a]] = if (n <= 0) {
        optional(sep).map(_ -> Nil)
    } else if (n == 1) {
        (p `and` optional(sep)).map(match (e, _) -> e :: Nil)
    } else {
        (p.skipRight(sep) `and` tailSeperatedTake(p, sep, n - 1)).map(match (e, rest) -> e :: rest)
    }

    /// Given a parser `p`, returns a parser which runs `p` on the input until it returns None.
    /// The new parser returns a list of the values `p` parsed.
    ///
    /// # Examples
    ///
    /// ```
    /// String.toList("aab") |> takeAny(tag('a')) == Some(('b' :: Nil, 'a' :: 'a' :: Nil))
    /// ```
    ///
    /// ```
    /// String.toList("b") |> takeAny(tag('a')) == Some(('b' :: Nil, Nil))
    /// ```
    pub def takeAny[a](
        p: Parser[a]
    ): Parser[List[a]] = input -> Some(input.takeAnyInternal(p))

    def takeAnyInternal[a](
        input: List[Char],
        p: Parser[a]
    ): (List[Char], List[a]) = match input.p() {
        case Some((xs, v)) => {
            let (ys, u) = xs.takeAnyInternal(p);
            (ys, v :: u)
        }
        case None => (input, Nil)
    }

    /// Given a parser `p`, returns a parser which runs `p` on the input until it returns None.
    /// The new parser returns a list of the values `p` parsed. The new parser fails if `p` didn't
    /// accept the input any times.
    ///
    /// # Examples
    ///
    /// ```
    /// String.toList("aab") |> takeMany(tag('a')) == Some(('b' :: Nil, 'a' :: 'a' :: Nil))
    /// ```
    ///
    /// ```
    /// String.toList("b") |> takeMany(tag('a')) == None
    /// ```
    pub def takeMany[a](
        p: Parser[a]
    ): Parser[List[a]] = input -> match takeAnyInternal(input, p) {
        case (_, Nil) => None
        case ret => Some(ret)
    }

    /// Given parsers `p` and `sep`, returns a parser which uses `p` zero or more times, with `sep`
    /// interlaced. Returns a list of values parsed by `p`. Discards anything parsed by `sep`.
    ///
    /// # Examples
    ///
    /// ```
    /// String.toList("ababa") |> seperatedAny(tag('a'), tag('b'))
    /// ==
    /// Some((Nil, 'a' :: 'a' :: 'a' :: Nil))
    /// ```
    ///
    /// ```
    /// String.toList("abab") |> seperatedAny(tag('a'), tag('b'))
    /// ==
    /// Some(('b' :: Nil, 'a' :: 'a' :: Nil))
    /// ```
    ///
    /// ```
    /// String.toList("a") |> seperatedAny(tag('a'), tag('b')) == Some((Nil, 'a' :: Nil))
    /// ```
    ///
    /// ```
    /// String.toList("b") |> seperatedAny(tag('a'), tag('b')) == Some(('b' :: Nil, Nil))
    /// ```
    pub def seperatedAny[a, b](
        p: Parser[a],
        sep: Parser[b]
    ): Parser[List[a]] = seperatedMany(p, sep) `or` pure(Nil)

    /// Given parsers `p` and `sep`, returns a parser which uses `p` one or more times with `sep`
    /// interlaced. Returns a list of values parsed by `p`. Discards anything parsed by `sep`.
    /// Fails if the `p` couldn`t be run any times.
    ///
    /// # Examples
    ///
    /// ```
    /// String.toList("ababa") |> seperatedMany(tag('a'), tag('b'))
    /// ==
    /// Some((Nil, 'a' :: 'a' :: 'a' :: Nil))
    /// ```
    ///
    /// ```
    /// String.toList("abab") |> seperatedMany(tag('a'), tag('b'))
    /// ==
    /// Some(('b' :: Nil, 'a' :: 'a' :: Nil))
    /// ```
    ///
    /// ```
    /// String.toList("a") |> seperatedMany(tag('a'), tag('b')) == Some((Nil, 'a' :: Nil))
    /// ```
    ///
    /// ```
    /// String.toList("b") |> seperatedMany(tag('a'), tag('b')) == None
    /// ```
    pub def seperatedMany[a, b](
        p: Parser[a],
        sep: Parser[b]
    ): Parser[List[a]] = (p `and` takeAny(sep `skipLeft` p)).map(match (lhs, rhs) -> lhs :: rhs)

    /// Given parsers `p` and `sep`, returns a parser which uses `p` zero or more times with `sep`
    /// interlaced. If `sep` can be run after the last run of `p`, the new parser does so. Returns a
    /// list of values parsed by `p`. Discards anything parsed by `sep`.
    ///
    /// # Examples
    ///
    /// ```
    /// String.toList("ababa") |> tailSeperatedAny(tag('a'), tag('b'))
    /// ==
    /// Some((Nil, 'a' :: 'a' :: 'a' :: Nil))
    /// ```
    ///
    /// ```
    /// String.toList("abab") |> tailSeperatedAny(tag('a'), tag('b'))
    /// `assertEq!'
    /// Some((Nil, 'a' :: 'a' :: Nil))
    /// ```
    ///
    /// ```
    /// String.toList("a") |> tailSeperatedAny(tag('a'), tag('b'))
    /// ==
    /// Some((Nil, 'a' :: Nil))
    /// ```
    ///
    /// ```
    /// String.toList("b") |> tailSeperatedAny(tag('a'), tag('b'))
    /// ==
    /// Some(('b' :: Nil, Nil))
    /// ```
    pub def tailSeperatedAny[a, b](
        p: Parser[a],
        sep: Parser[b]
    ): Parser[List[a]] = tailSeperatedMany(p, sep) `or` pure(Nil)

    /// Given parsers `p` and `sep`, returns a parser which uses `p` one or more times with `sep`
    /// interlaced. If `sep` can be run after the last run of `p`, the new parser does so. Returns a
    /// list of values parsed by `p`. Discards anything parsed by `sep`. Fails if `p` couldn't be
    /// run any times.
    ///
    /// # Examples
    ///
    /// ```
    /// String.toList("ababa") |> tailSeperatedMany(tag('a'), tag('b'))
    /// ==
    /// Some((Nil, 'a' :: 'a' :: 'a' :: Nil))
    /// ```
    ///
    /// ```
    /// String.toList("abab") |> tailSeperatedMany(tag('a'), tag('b'))
    /// ==
    /// Some((Nil, 'a' :: 'a' :: Nil))
    /// ```
    ///
    /// ```
    /// String.toList("a") |> tailSeperatedMany(tag('a'), tag('b'))
    /// ==
    /// Some((Nil, 'a' :: Nil))
    /// ```
    ///
    /// ```
    /// String.toList("b") |> tailSeperatedMany(tag('a'), tag('b'))
    /// ==
    /// None
    /// ```
    pub def tailSeperatedMany[a, b](
        p: Parser[a],
        sep: Parser[b]
    ): Parser[List[a]] = seperatedMany(p, sep) `skipRight` optional(sep)

    /// Given three parsers `lhs`, `mid`, and `rhs`, returns new parser which starts by applying
    /// `lhs`, throws away the output, applies `mid`, saves the output, and applies `rhs` and
    /// returns the output from mid.
    ///
    /// # Examples
    ///
    /// ```
    /// String.toList("[abz]") |>
    ///     delimit(tag('['), takeAny(char().filter(Char.isLetter)), tag(']'))
    /// ==
    /// Some((Nil, 'a' :: 'b' :: 'z' :: Nil))
    /// ```
    ///
    /// ```
    /// String.toList("[a.bz]") |>
    ///     delimit(tag('['), takeAny(char().filter(Char.isLetter)), tag(']'))
    /// ==
    /// None
    /// ```
    ///
    /// ```
    /// String.toList("[]") |>
    ///     delimit(tag('['), takeAny(char().filter(Char.isLetter)), tag(']'))
    /// ==
    /// Some((Nil, Nil))
    /// ```
    pub def delimit[a, b, c](
        lhs: Parser[a],
        mid: Parser[b],
        rhs: Parser[c]
    ): Parser[b] = lhs `skipLeft` mid `skipRight` rhs

    /// Given a Parser `p`, an initial value `initial`, and a function `f`, generates a parser,
    /// which recursively applies `f` to initial, or the last output from `f`, and the next parsed
    /// value.
    ///
    /// # Examples
    ///
    /// ```
    /// String.toList("abc") |> fold(char(), Nil, l -> c -> c :: l)
    /// ==
    /// Some((Nil, 'c' :: 'b' :: 'a' :: Nil))
    /// ```
    pub def fold[a, b](
        p: Parser[a],
        initial: b,
        f: b -> a -> b
    ): Parser[b] = input -> Some(foldInternal(p, input, initial, f))

    def foldInternal[a, b](
        p: Parser[a],
        input: List[Char],
        initial: b,
        f: b -> a -> b
        // (List[_], Map[k, v]) -> a -> (List[_], Map[k, v])
    ): (List[Char], b) = match input.p() {
        case Some((xs, v)) => foldInternal(p, xs, f(initial, v), f)
        case None => (input, initial)
    }

}
