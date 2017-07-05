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
    def x(): Bool = false
    def f(): Str = "foo"
}

@test
def refTest01(): Bool = assertEq!(Foo01/Bar01.f(), "foo")

namespace Foo02 {
    def f(): Int = 5
    def g(): Int = f()
}

@test
def refTest02(): Bool = assertEq!(Foo02.g(), 5)

namespace Foo03 {
    def x(): Bool = true
    def y(): Bool = false
    def f(): Bool = y()
}

@test
def refTest03(): Bool = assertEq!(Foo03.f(), false)

namespace Foo04 {
    def x(): Str = "hello"
}
namespace Bar04 {
    def x(): Str = Foo04.x()
}

@test
def refTest04(): Bool = assertEq!(Bar04.x(), "hello")

def x05(): Int = 42

@test
def refTest05(): Bool = assertEq!(x05(), 42)

namespace A06/B06 {
    def a(): Bool = false
}
namespace A06 {
    def b(): Bool = !A06/B06.a()
}
namespace A06 {
    namespace B06 {
        def c(): Int = 0

        namespace C06 {
            def d(): Int = 42
        }
    }
}
def e06(): Int = -1

@test
def refTest06(): Bool = assertEq!(A06/B06.a(), false)

@test
def refTest07(): Bool = A06.b()

@test
def refTest08(): Bool = assertEq!(A06/B06.c(), 0)

@test
def refTest09(): Bool = assertEq!(A06/B06/C06.d(), 42)

@test
def refTest10(): Bool = assertEq!(e06(), -1)