namespace PackageParser {
    pub enum Parsed {
        case Empty,
        case MapVariant(Map[String, Parsed]),
        case StringVariant(String),
        case ListVariant(List[Parsed])
    }

    opaque type ParseError = String

    pub def parse(input: String): Result[Parsed, ParseError] = {
        dontEvenKnowWhatTryThisIsAgain(String.toList(input))
    }

    def dontEvenKnowWhatTryThisIsAgain(input: List[Char]): Parsed = match List.head(input) {
        case Some(x) => if (Char.isWhiteSpace(x)) {
            dontEvenKnowWhatTryThisIsAgain(List.drop(1, input))
        } else if (x == '(') {
            let ret = parseMapEntry(List.drop(1, input), Parsed.Empty);
            match List.head(ret.fst()) {
                case Some(_) => {
                    //Dirty input, contans non-whitespace symbols after finished parsing
                    ???
                }
                case None => {
                    ret.snd()
                }
            }
        } else {
            //Parse error!
            ???
        }
        case None => ???
    }

    // State 1
    def parseMapEntry(
        input: List[Char],
        acc: Parsed
    ): (List[Char], Parsed) = match List.head(input) {
        case Some(x) => if (Char.isWhiteSpace(x)) {
            parseMapEntry(List.drop(1, input), acc)
        } else if (isAlphaNumric(x)) {
            parseKey(List.drop(1, input), x :: Nil, acc)
        } else {
            ???
        }
    }
    
    // State 2
    def parseKey(
        input: List[Char],
        keyAcc: List[Char],
        acc: Parsed
    ): (List[Char], Parsed) = match List.head(input) {
        case Some(x) => if (isAlphaNumric(x)) {
            parseKey(List.drop(1, input), x :: keyAcc)
        } else if (Char.isWhiteSpace(x)) {
            parseWhiteSpaceBeforeValue(List.drop(1, input), List.toString(keyAcc), acc)
        } else if (x == '"') {
            parseQuotedValue(List.drop(1, input), List.toString(keyAcc), Nil, acc)
        } else if (x == '(') {
            //Recursively calculate value
            let rec = parseRecValue(List.drop(1, input), List.toString(keyAcc), acc);
            //TODO: Not sure about this
            appendToMap(rec.fst(), List.toString(keyAcc), rec.snd(), acc)
        } else {
            // Parse error!
            ???
        }
        case None => ???
    }

    // State 4
    def parseRecValue(
        input: List[Char],
        key: String,
        acc: Parsed
    ): (List[Char], Parsed) = match List.head(input) {
        case Some(x) => if (Char.isWhiteSpace(x)) {
            parseValue(List.drop(1, input), key, acc)
        } else if (isAlphaNumric(x)) {
            // State 8
            let rec = parseMapEntry(List.drop(1, input), acc);
            parseWhiteSpaceAfterValue(rec.fst(), key, rec.snd(), acc)
        } else if (x == '(') {
            parseList(List.drop(1, input), key, Nil, acc)
        } else {
            // Parse error!
            ???
        }
        case None => ???
    }

    // State 9 
    def parseList(
        input: List[Char],
        key: String,
        listAcc: List[Parsed],
        acc: Parsed
    ): (List[Char], Parsed) = match List.head(input) {
        case Some(x) => {
            let rec = parseMapEntry(input, Parsed.Empty);
            state11(rec.fst(), key, rec.snd() :: listAcc, acc)
        }
        case None => ???
    }

    def state11(
        input: List[Char],
        key: String,
        listAcc: List[Parsed],
        acc: Parsed
    ): (List[Char], Parsed) = match List.head(input) {
        case Some(x) => if (Char.isWhiteSpace(x)) {
            state11(List.drop(1, input), key, listAcc, acc)
        } else if (x == ')') {
            state12(List.drop(1, input), key, listAcc, acc)
        } else {
            // Parse error!
            ???
        }
    }

    def state12(
        input: List[Char],
        key: String,
        listAcc: List[Parsed],
        acc: Parsed
    ): (List[Char], Parsed) = match List.head(input) {
        case Some(x) => if (Char.isWhiteSpace(x)) {
            state12(List.drop(1, input), key, listAcc, acc)
        } else if (x == ')') {
            appendToMap(List.drop(1, input), key, Parsed.ListVariant(listAcc), acc)
        } else if (x == '(') {
            state13(List.drop(1, input), key, listAcc, acc)
        } else {
            // Parse error!
            ???
        }
    }

    def state13(
        input: List[Char],
        key: String,
        listAcc: List[Parsed],
        acc: Parsed
    ): (List[Char], Parsed) = match List.head(input) {
        case Some(x) => if (Char.isWhiteSpace(x)) {
            state13(List.drop(1, input), listAcc, acc)
        } else if (x == '(') {
            parseList(List.drop(1, input), key, listAcc, acc)
        } else {
            //Parse error!
            ???
        }
    }

    // State 3
    def parseWhiteSpaceBeforeValue(
        input: List[Char],
        key: String,
        acc: Parsed
    ): (List[Char], Parsed) = match List.head(input) {
        case Some(x) => if (Char.isWhiteSpace(x)) {
            parseWhiteSpaceBeforeValue(List.drop(1, input), key)
        } else if (isAlphaNumric(x)) {
            parseValue(List.drop(1, x), key, x :: Nil, acc)
        } else if (x == '"') {
            parseQuotedValue(List.drop(1, x), key, Nil, acc)
        } else if (x == '(') {
            parseRecValue(List.drop(1, x), key, acc)
        } else {
            // Parse error!
            ???
        }
        case None => ???
    }

    // State 5
    def parseValue(
        input: List[Char],
        key: String,
        valueAcc: List[Char],
        acc: Parsed
    ): (List[Char], Parsed) = match List.head(input) {
        case Some(x) => if (Char.isWhiteSpace(x)) {
            parseWhiteSpaceAfterValue(List.drop(1, input), key, List.toString(valueAcc), acc)
        } else if (isAlphaNumric(x)) {
            parseValue(List.drop(1, input), key, x :: valueAcc, acc)
        } else if (x == ')') {
            appendToMap(List.drop(1, input), key, Parsed.StringVariant(List.toString(valueAcc)), acc)
        } else {
            // Parse error
            ???
        }
        case None => ???
    }

    // State 6
    def parseQuotedValue(
        input: List[Char],
        key: String,
        valueAcc: List[Char],
        acc: Parsed
    ): (List[Char], Parsed) = match List.head(input) {
        case Some(x) => if (x == '"') {
            parseWhiteSpaceAfterValue(List.drop(1, input), key, Parsed.StringVariant(List.toString(valueAcc)), acc)
        } else {
            parseQuotedValue(List.drop(1, input), key, x :: valueAcc, acc)
        }
        case None => ???
    }

    // State 7
    def parseWhiteSpaceAfterValue(
        input: List[Char],
        key: String,
        value: Parsed,
        acc: Parsed
    ): (List[Char], Parsed) = match List.head(input) {
        case Some(x) => if (Char.isWhiteSpace(x)) {
            parseWhiteSpaceAfterValue(List.drop(1), key, value, acc)
        } else if (x == ')') {
            appendToMap(List.drop(1), key, value, acc)
        } else {
            //Parse error
            ???
        }
        case None => ???
    }

    // State 9.5?
    def appendToMap(
        input: List[Char],
        key: String,
        value: Parsed,
        acc: Parsed
    ): (List[Char], Parsed) = match acc {
        case Empty => parseAfterAppend(input, Map.insert(key, value, Map.empty())) 
        case MapVariant(map) => parseAfterAppend(input, Map.insert(key, value, map))
        case StringVariant(_) => {
            // Unreachable!
            ???
        }
        case ListVariant(_) => {
            // Unreachable!
            ???
        }
    }

    def parseAfterAppend(
        input: List[Char],
        acc: Parsed
    ): (List[Char], Parsed) = match List.head(input) {
        case Some(x) => if (Char.isWhiteSpace(x)) {
            parseAfterAppend(List.drop(1, input), acc)
        } else if (x == '(') {
            parseMapEntry(List.drop(1, input), acc)
        } else {
            (input, acc)
        }
    }

    def isAlphaNumric(c: Char): Bool = Char.isLetter(c) || Char.isDigit(c)
}
