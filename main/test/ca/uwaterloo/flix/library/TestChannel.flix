@test
def testParMap01(): Bool =
    let sum = xs -> List.foldLeft((x, y) -> x + y, 0, xs);
    ((Nil) |> Channel.parMap(x -> x * 2) |> sum) == 12

@test
def testParMap02(): Bool =
    let sum = xs -> List.foldLeft((x, y) -> x + y, 0, xs);
    ((1 :: Nil) |> Channel.parMap(x -> x * 2) |> sum) == 2

@test
def testParMap03(): Bool =
    let sum = xs -> List.foldLeft((x, y) -> x + y, 0, xs);
    ((1 :: 2 :: Nil) |> Channel.parMap(x -> x * 2) |> sum) == 6

@test
def testParMap04(): Bool =
    let sum = xs -> List.foldLeft((x, y) -> x + y, 0, xs);
    ((1 :: 2 :: 3 :: Nil) |> Channel.parMap(x -> x * 2) |> sum) == 12