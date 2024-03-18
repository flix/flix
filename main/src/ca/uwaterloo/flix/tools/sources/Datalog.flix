pub def unconnected(roads: List[(city, city)]): Vector[(city, city)] with Order[city] =
    let r = inject roads into Road;
    let lp = #{
        City(x) :- Road(x, _).
        City(y) :- Road(_, y).
        Path(x, y) :- Road(x, y).
        Path(x, z) :- Path(x, y), Road(y, z).
        Unconnected(x, y) :- City(x), City(y), not Path(x, y).
    };
    query r, lp select (x, y) from Unconnected(x, y)

def foo() : Int32 = Vector.length(unconnected((1, 1) :: (2, 2) :: Nil))

def printt() : Unit \ IO = unconnected((1, 1) :: (2, 2) :: Nil) |> println

@Test
def test1(): Bool = Assert.eq(2 , foo())
