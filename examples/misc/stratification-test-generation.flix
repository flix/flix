def main(_args: Array[String]): Int & Impure =
    println();
    println(testConcept00());
    0

def termFilteringTests(startNumber: Int32): List[String] =
    List.range(startNumber, )
    testTemplate(
        testConcept = "TermFiltering",
        conceptPrefix = 0,
        conceptTestNumber = 0,
        queryName = "A",
        queryArity = 2,
        program1 = x ->
                "A${x}(12, 13)." ::
                "A${x}(x, y) :- B${x}(y, x)." ::
                Nil,
        program2 = x ->
                "B${x}(12)." ::
                "B${x}(x) :- A${x}(x, x)." ::
                Nil
    )

enum Line {
    case Line(Int32, String)
}

pub def mkString(indent: String, l: Line): String =
    let Line(indentLevel, content) = l;
    let indentString = String.repeat(indentLevel, indent);
    "${indentString}${content}"


/// `testConcept` should be capitalized
def testTemplate(testConcept: {testConcept::String}, conceptPrefix: {conceptPrefix::Int32},
                  conceptTestNumber: {conceptTestNumber::Int32}, queryName: {queryName::String},
                  queryArity: {queryArity::Int32}, program1: {program1::String -> List[String]},
                  program2: {program2::String -> List[String]}): String =
    let wildcards = List.repeat(queryArity.queryArity - 1, "_") |> String.intercalate(", ");
    let _assertion = if (queryArity.queryArity < 1) bug!("cannot use arity below one") else ();
    let predicateTerms = String.concat(if(queryArity.queryArity == 1) "" else ", ", wildcards);
    let compNumber = "${conceptPrefix.conceptPrefix}${conceptTestNumber.conceptTestNumber}";
    let p1 = compNumber |> program1.program1;
    let p2 = compNumber |> program2.program2;
    let lines = {
        {
            Line(0, "@test") ::
            Line(0, "def test${testConcept.testConcept}${compNumber}(): Int32 = ") ::
            Line(1  , "let p1 = #{") ::
            Nil
        } :: {
            p1 |> List.map(l -> Line(2, l))
        } :: {
            Line(1  , "};") ::
            Line(1  , "let p2 = #{") ::
            Nil
        } :: {
            p2 |> List.map(l -> Line(2, l))
        } :: {
            Line(1  , "};") ::
            Line(1  , "let x1 = query p1 select x from ${queryName.queryName}${compNumber}(x${predicateTerms}) |> Array.length;") ::
            Line(1  , "let x2 = query p2 select x from ${queryName.queryName}${compNumber}(x${predicateTerms}) |> Array.length;") ::
            Line(1  , "x1 + x2") ::
            Nil
        } :: Nil
    } |> List.flatten;
    lines |> List.formatWith(mkString("    "), String.lineSeparator())
