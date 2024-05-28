// String Splitting function
// EXAMPLE USAGE:
// split ' ', "Hello, my name is Peter!" = Hello, :: my :: name :: is :: Peter! :: Nil
// split '@', "name@address.dk" = name :: address.dk :: Nil
def split(c: Char, s: String): List[String] =
    match s {
        case "" => Nil,
        case str => 
            String.takeWhile((x -> x != c), str) :: split(c, String.drop(1, String.dropWhile((x -> x != c), str)))
    }

def merge(c: Char, l: List[String]): String =
    String.intercalateChar(c, l)

def examples(): List[(Char, String)] =
   (' ', "Hello, my name is Peter!") :: 
   ('@', "name@address.dk") :: 
   ('/',"C:/User/Username/Documents") :: 
   ('@', "asd@@bbe") ::
   ('a', "hla") ::
   Nil

def prop_splitInverse(c: Char, s: String): Bool = 
    merge(c, split(c, s)) == s

def prop_splitInverse2(s: String): Property = 
    let stringGen = 
        match List.toNel(String.toList(s)) {
            case None => return('-'),
            case Some(nel) => elements(nel)
        };
    forAll(stringGen, c -> merge(c, split(c, s)) == s)

// Pretty print a list of strings
def prettyList(l: List[String]): String = 
    "[\"" + String.intercalate("\", \"", l) + "\"]"


def tt(): Unit \ IO = 
    foreach(e <- examples()) {
        let (c, s) = e;
        println(prettyList(split(c, s)))
    }
    // ;
    // flixCheck(TFunc.TFunc(prop_splitInverse2))
