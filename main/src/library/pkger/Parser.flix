// Could be generic in monad with type classes
pub type alias Parsec[s, a] = s -> Option[(s, a)]

pub type alias Parser[a] = Parsec[String, a]

namespace Parser {
    pub def runParser(v: Parser[a], s: String): Option[(String, a)] = v(s)

    pub def mapOption(v: Parser[a], f: a -> Option[b]): Parser[b] = input -> {
        Option.flatMap(tup -> match tup { case (inputx, x) => Option.map(y -> (inputx, y), f(x)) }, runParser(v, input))
    }

    // Functor
    pub def fmap(f: a -> b, parser: Parser[a]): Parser[b] = input -> Option.flatMap(tup -> match tup { case (inputx, x) => Some((inputx, f(x))) }, runParser(parser, input))

    // Applicative
    pub def pure(x: a): Parser[a] = input -> Some((input, x))

    pub def ap(l: Parser[a -> b], r: Parser[a]): Parser[b] = input -> Option.flatMap(tup -> match tup { case (inputx, f) => Option.flatMap(tupx -> match tupx { case (inputxx, x) => Some((inputxx, f(x))) }, runParser(r, inputx)) }, runParser(l, input))

    pub def liftA2(f: a -> b -> c, x: Parser[a], y: Parser[b]): Parser[c] = ap(fmap(f, x), y)

    // Monad
    pub def return(x: a): Parser[a] = pure(x)

    // TODO: `flatMap` instead?
    pub def bind(v: Parser[a], f: a -> Parser[b]): Parser[b] = input -> {
        Option.flatMap(tup -> match tup { case (inputx, x) => runParser(f(x), inputx) }, runParser(v, input))
    }

    // Alternative
    pub def empty(): Parser[a] = _ -> None

    pub def choose(l: Parser[a], r: Parser[a]): Parser[a] = input -> Option.choose(runParser(l, input), runParser(r, input))

    def cons(x: a, xs: List[a]): List[a] = x :: xs

    def flipCons(xs: List[a], x: a): List[a] = x :: xs

    pub def some(v: Parser[a]): Parser[List[a]] = cons <&> v <*> many(v)

    pub def many(v: Parser[a]): Parser[List[a]] = some(v) <|> Parser.pure(Nil)

    def listToString(xs: List[Char]): String = List.foldLeft((s, x) -> String.concat(s, Char.toString(x)), "", xs)

    // Parser
    pub def char(x: Char): Parser[Char] = input -> match String.toList(input) {
        case Cons(y, ys) => if (y == x) { Some((listToString(ys), x)) } else { None }
        case Nil => None
    }

    def sequenceA(xs: List[Parser[a]]): Parser[List[a]] = List.reverse <&> List.foldLeft(liftA2(flipCons), pure(Nil), xs)

    pub def string(s: String): Parser[String] = listToString <&> List.map(char,  String.toList(s)).sequenceA()

    // Made easier if 1) `Parsec` was used 2) `String` was `List[Char]`
    pub def span(p: Char -> Bool): Parser[String] = input -> {
        let (l, r) = List.span(p, String.toList(input));
        Some(listToString(r), listToString(l))
    }

    // TODO: Weird name.
    pub def failOn(v: Parser[a], p: a -> Bool): Parser[a] = mapOption(v, x -> if (p(x)) { None } else { Some(x) })

    pub def separatedBy(separator: Parser[a], element: Parser[b]): Parser[List[b]] = cons <&> element <*> many(separator *> element) <|> pure(Nil)

    pub def whitespace(): Parser[String] = span(c -> Char.isWhiteSpace(c))
}

// This is a large amount of namespace pollution. Right now this file is only loaded manually, so it's alright.
pub def <&>(f: a -> b, parser: Parser[a]): Parser[b] = Parser.fmap(f, parser)

pub def <&(x: a, parser: Parser[b]): Parser[a] = (_ -> x) <&> parser

pub def &>(parser: Parser[a], x: b): Parser[b] = x <& parser

pub def <*>(l: Parser[a -> b], r: Parser[a]): Parser[b] = Parser.ap(l, r)

pub def <*(l: Parser[a], r: Parser[b]): Parser[a] = ((x, _) -> x) <&> l <*> r

pub def *>(l: Parser[a], r: Parser[b]): Parser[b] = ((x -> x) <& l) <*> r

pub def <|>(l: Parser[a], r: Parser[a]): Parser[a] = Parser.choose(l, r)

pub def >>=(v: Parser[a], f: a -> Parser[b]): Parser[b] = Parser.bind(v, f)

pub enum JsonValue {
    case JsonNull
    case JsonBool(Bool)
    // Json spec allows for parser-defined limits on integer sizes
    case JsonNumber(Int)
    case JsonString(String)
    case JsonArray(List[JsonValue])
}

namespace JsonParser {
    pub def jsonNull(): Parser[JsonValue] = (_ -> JsonNull) <&> Parser.string("null")

    def jsonBoolHelper(s: String): JsonValue = match s {
        case "true" => JsonBool(true),
        case "false" => JsonBool(false)
        // TODO: Is there something akin to `unreachable!()` from Rust?
        case _ => ???
    }

    pub def jsonBool(): Parser[JsonValue] = jsonBoolHelper <&> (Parser.string("true") <|> Parser.string("false"))

    def jsonNumberHelper(s: String): Option[JsonValue] = Option.map(i -> JsonNumber(i),  Result.toOption(Int32.fromString(s)))

    pub def jsonNumber(): Parser[JsonValue] = Parser.mapOption(Parser.failOn(Parser.span(Char.isDigit), String.isEmpty), jsonNumberHelper)

    pub def jsonString(): Parser[JsonValue] = (s -> JsonString(s)) <&> (Parser.char('"') *> Parser.span(c -> c != '"') <* Parser.char('"'))

    // TODO: Tried to be smart in the `many` and `some` parsers and it backfired. Results in a stack overflow exception here
    pub def jsonArrayElementHelper(): Parser[List[JsonValue]] = Parser.separatedBy(Parser.whitespace() *> Parser.char(',') <* Parser.whitespace(), jsonValue())

    pub def jsonArray(): Parser[JsonValue] = (xs -> JsonArray(xs)) <&> (Parser.char('[') *> Parser.whitespace() *> jsonArrayElementHelper() <* Parser.whitespace() <* Parser.whitespace())

    pub def jsonValue(): Parser[JsonValue] = jsonBool() <|> jsonNumber() <|> jsonString()
}
