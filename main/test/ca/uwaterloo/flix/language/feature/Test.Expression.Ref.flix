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

namespace Foo_01/Bar_01 {
    def x(): Bool = false
    def f(): Str = "foo"
}

@test
def refTest_01(): Bool = assertEq!(Foo_01/Bar_01.f(), "foo")

namespace Foo_02 {
    def f(): Int = 5
    def g(): Int = f()
}

@test
def refTest_02(): Bool = assertEq!(Foo_02.g(), 5)

namespace Foo_03 {
    def x(): Bool = true
    def y(): Bool = false
    def f(): Bool = y()
}

@test
def refTest_03(): Bool = assertEq!(Foo_03.f(), false)

namespace Foo_04 {
    def x(): Str = "hello"
}
namespace Bar_04 {
    def x(): Str = Foo_04.x()
}

@test
def refTest_04(): Bool = assertEq!(Bar_04.x(), "hello")

def x_05(): Int = 42

@test
def refTest_05(): Bool = assertEq!(x_05(), 42)

namespace A_06/B_06 {
    def a(): Bool = false
}
namespace A_06 {
    def b(): Bool = !A_06/B_06.a()
}
namespace A_06 {
    namespace B_06 {
        def c(): Int = 0

        namespace C_06 {
            def d(): Int = 42
        }
    }
}
def e_06(): Int = -1

@test
def refTest_06(): Bool = assertEq!(A_06/B_06.a(), false)

@test
def refTest_07(): Bool = A_06.b()

@test
def refTest_08(): Bool = assertEq!(A_06/B_06.c(), 0)

@test
def refTest_09(): Bool = assertEq!(A_06/B_06/C_06.d(), 42)

@test
def refTest_10(): Bool = assertEq!(e_06(), -1)