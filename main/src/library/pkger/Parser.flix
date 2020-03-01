pub type alias Parser[a] = String -> Option[(String, a)]

namespace Parser {
    pub def runParser(parser: Parser[a], s: String): Option[(String, a)] = parser(s)

    // Functor
    pub def fmap(f: a -> b, parser: Parser[a]): Parser[b] = input -> Option.flatMap(tup -> match tup { case (inputx, x) => Some((inputx, f(x))) }, runParser(parser, input))

    // Applicative
    pub def pure(x: a): Parser[a] = input -> Some((input, x))

    pub def ap(l: Parser[a -> b], r: Parser[a]): Parser[b] = input -> Option.flatMap(tup -> match tup { case (inputx, f) => Option.flatMap(tupx -> match tupx { case (inputxx, x) => Some((inputxx, f(x))) }, runParser(r, inputx)) }, runParser(l, input))

    pub def liftA2(f: a -> b -> c, x: Parser[a], y: Parser[b]): Parser[c] = ap(fmap(f, x), y)

    // Alternative
    pub def empty(): Parser[a] = _ -> None

    pub def choose(l: Parser[a], r: Parser[a]): Parser[a] = input -> Option.choose(runParser(l, input), runParser(r, input))

    def cons(x: a, xs: List[a]): List[a] = x :: xs

    pub def some(v: Parser[a]): Parser[List[a]] = liftA2(cons, v, many(v))

    pub def many(v: Parser[a]): Parser[List[a]] = some(v) <|> Parser.pure(Nil)

    def listToString(xs: List[char]): String = List.foldLeft((s, x) -> String.concat(s, Char.toString(x)), "", xs)

    // Parser
    pub def char(x: char): Parser[char] = input -> match String.toList(input) {
        case Cons(y, ys) => match (y == x) {
            case true => Some((listToString(ys), x))
            case _ => None
        }
        case Nil => None
    }
}

pub def <&>(f: a -> b, parser: Parser[a]): Parser[b] = Parser.fmap(f, parser)

pub def <&(x: a, parser: Parser[b]): Parser[a] = (_ -> x) <&> parser

pub def &>(parser: Parser[a], x: b): Parser[b] = x <& parser

pub def <*>(l: Parser[a -> b], r: Parser[a]): Parser[b] = Parser.ap(l, r)

pub def <*(l: Parser[a], r: Parser[b]): Parser[a] = Parser.liftA2((x, _) -> x, l, r)

pub def <|>(l: Parser[a], r: Parser[a]): Parser[a] = Parser.choose(l, r)
