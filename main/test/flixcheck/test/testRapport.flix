// String Splitting function
// EXAMPLE USAGE:
// split ' ', "Hello, my name is Peter!" = Hello, :: my :: name :: is :: Peter! :: Nil
// split '@', "name@address.dk" = name :: address.dk :: Nil
def split(c: Char, str: String): List[String] =
    match str {
        case "" => Nil,
        case _ => 
            let prefix = String.takeWhile((x -> x != c), str);
            let rest = String.dropWhile((x -> x != c), str);
            prefix :: split(c, String.drop(1, rest))
    }

def split2(c: Char, str: String): List[String] = 
    let prefix = String.takeWhile((x -> x != c), str);
    let rest = String.dropWhile((x -> x != c), str);
    if (String.length(rest) == 0) {
        prefix :: Nil
    } else {
        prefix :: split2(c, String.drop(1, rest))
    }

def merge(c: Char, l: List[String]): String =
    String.intercalateChar(c, l)

@Test
def testSplit1(): Bool =
    Assert.eq(split('@', "name@address.dk"), "name" :: "address.dk" :: Nil)

@Test
def testSplit2(): Bool =
    Assert.eq(split('/',"C:/User/Username/Documents"), "C:" :: "User" :: "Username" :: "Documents" :: Nil)

@Test
def testSplit3(): Bool =
    Assert.eq(split('%', "#coding#haskell#flix"), "#coding#haskell#flix" :: Nil)

def printExample(): Unit \ IO =
    println("Running split with input 'V' and 'DoWhV'...");
    println("Expected output: [\"DoWh\", \"\"]");
    println("Actual output: " + prettyList(split('V', "DoWhV")))
    // prettyPrintList(split2('V', "DoWhV"))
    
// Pretty print a list of strings
def prettyList(l: List[String]): String = 
    "[\"" + String.intercalate("\", \"", l) + "\"]"
    
def prop_splitInverse(c: Char, s: String): Bool = 
    merge(c, split(c, s)) == s

def prop_splitInverse2(c: Char, s: String): Bool = 
    merge(c, split2(c, s)) == s

def prop_listLength(c: Char, s: String): Bool = 
    List.length(split2(c, s)) == String.countSubstring({substr = Char.toString(c)}, s) + 1
def examples(): List[(Char, String)] =
   ('#', "#coding#haskell#flix") :: 
   ('@', "name@address.dk") :: 
   ('/',"C:/User/Username/Documents") :: 
   ('@', "asd@@bbe") ::
   ('a', "hla") ::
   Nil

def printExamples(): Unit \ IO = 
    foreach(e <- examples()) {
        let (c, s) = e;
        println(prettyList(split2(c, s)))
    }

def main(): Unit \ IO =
    println("Testing property prop_splitInverse...");
    flixCheck(TFunc2.TFunc2(prop_splitInverse2));
    println("Testing property prop_listLength...");
    flixCheck(TFunc2.TFunc2(prop_listLength))
