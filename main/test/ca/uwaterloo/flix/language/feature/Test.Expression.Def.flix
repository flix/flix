/////////////////////////////////////////////////////////////////////////////
// LoadExpression and StoreExpression                                      //
/////////////////////////////////////////////////////////////////////////////

// TODO: LoadExpression and StoreExpression tests.
// {Load,Store}Expressions are generated, and not explicitly written in a Flix program

/////////////////////////////////////////////////////////////////////////////
// Expression.Var                                                          //
// Tested indirectly by Expression.{Lambda,Let}.                           //
/////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////////////////////////////////////
// Expression.Ref                                                          //
/////////////////////////////////////////////////////////////////////////////

namespace Foo01/Bar01 {
    pub def x(): Bool = false
    pub def f(): Str = "foo"
}

@test
def testDef01(): Bool = Foo01/Bar01.f() == "foo"

namespace Foo02 {
    pub def f(): Int = 5
    pub def g(): Int = f()
}

@test
def testDef02(): Bool = Foo02.g() == 5

namespace Foo03 {
    pub def x(): Bool = true
    pub def y(): Bool = false
    pub def f(): Bool = y()
}

@test
def testDef03(): Bool = Foo03.f() == false

namespace Foo04 {
    pub def x(): Str = "hello"
}
namespace Bar04 {
    pub def x(): Str = Foo04.x()
}

@test
def testDef04(): Bool = Bar04.x() == "hello"

def x05(): Int = 42

@test
def testDef05(): Bool = x05() == 42

namespace A06/B06 {
    pub def a(): Bool = false
}
namespace A06 {
    pub def b(): Bool = !A06/B06.a()
}
namespace A06 {
    namespace B06 {
        pub def c(): Int = 0

        namespace C06 {
            pub def d(): Int = 42
        }
    }
}
def e06(): Int = -1

@test
def testDef06(): Bool = A06/B06.a() == false

@test
def testDef07(): Bool = A06.b()

@test
def testDef08(): Bool = A06/B06.c() == 0

@test
def testDef09(): Bool = A06/B06/C06.d() == 42

@test
def testDef10(): Bool = e06() == -1

def trueFunc(): Bool = true

@test
def testDef11(): Bool = trueFunc()

def falseFunc(): Bool = true

@test
def testDef12(): Bool = falseFunc()

def nullFunc(): Bool = true

@test
def testDef13(): Bool = nullFunc()

def true123(): Bool = true

@test
def testDef14(): Bool = true123()
