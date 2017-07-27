/////////////////////////////////////////////////////////////////////////////
// Expression.{Match,Switch}Error                                          //
// Tested indirectly by switch expressions and pattern matching.           //
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// Switch expressions                                                      //
// These don't exist in the ExecutableAst because they're desugared to     //
// Expression.IfThenElse.                                                  //
/////////////////////////////////////////////////////////////////////////////

def f01(x: Bool): Int = switch {
  case x => 1
  case !x => 0
}

@test
def switchTest01(): Bool = assertEq!(f01(true), 1)

@test
def switchTest02(): Bool = assertEq!(f01(false), 0)

def f02(x: Bool): Int = switch {
  case x => 100
  case true => 20
}

@test
def switchTest03(): Bool = assertEq!(f02(true), 100)

@test
def switchTest04(): Bool = assertEq!(f02(false), 20)

def f03(x: Bool): Int = switch {
  case x => 0
  case !x => 1
  case true => 2
}

@test
def switchTest05(): Bool = assertEq!(f03(true), 0)

@test
def switchTest06(): Bool = assertEq!(f03(false), 1)

def f04(x: Int): Str = switch {
  case x < 0 => "negative"
  case x == 0 => "zero"
  case x == 1 => "one"
  case x == 2 => "two"
  case x >= 3 => "many"
}

@test
def switchTest06(): Bool = assertEq!(f04(-2), "negative")

@test
def switchTest06(): Bool = assertEq!(f04(-1), "negative")

@test
def switchTest06(): Bool = assertEq!(f04(0), "zero")

@test
def switchTest06(): Bool = assertEq!(f04(1), "one")

@test
def switchTest06(): Bool = assertEq!(f04(2), "two")

@test
def switchTest06(): Bool = assertEq!(f04(3), "many")

@test
def switchTest06(): Bool = assertEq!(f04(4), "many")

def f05(x: Bool): Int = switch {
  case x => 1
}

@test
def switchTest06(): Bool = assertEq!(f05(true), 1)





