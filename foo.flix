


// ArrayNew
/*
def f() : Array[Bool] = {
  let x = [true;1000];
  x
} */



// ArrayLoad
/*
def f() : Array[Int] = {
  let x = [[1,2,3], [1,2,3], [1,2,3]];
  x[2]
}
*/


// ArrayStore
/*
def f() : Int = {
  let x = [1,2,3,4,5];
  let _ = x[1] = 100;
  x[1]
}
*/

// ArrayLength
/*
def f() : Int = {
  let x = [1,2,3,4,5];
  length[x]
}
*/


// ArraySlice
/*
def f() : Int = {
  let x = [1,2,3,4,5];
  let y = x[0..2];
  y[1]
}
*/


// VectorLit
/*
def f() : Vector[Int, 5] = {
  let x = [|1, 2, 3, 4, 5|];
  x
}*/

// VectorNew
/*
def f() : Vector[Int, 5] = {
  let x = [|5;5|];
  x
}*/

// VectorLoad
/*
def f() : Int = {
  let x = [|5;5|];
  x[|3|]
}*/


// VectorStore
/*def f() : Unit = {
  let x = [|5;5|];
  x[|4|] = 6
}
*/

// VectorLit
/*
def f() : Array[Unit] = {
  let x = [];
  x
}*/


//VectorStore
/*
def f() : Unit = {
  let x = [|1,2,3,4,5|];
  x[|1|] = 100
}*/

// VectorLit
/*def f() : Vector[Int, 2] = {
  let x = [|1,2|];
  x
}*/


// VectorNew
/*
def f() : Vector[Unit, 0] = {
  let x = [|;|];
  x
}*/


// VectorLoad
/*
def f() : Int = {
  let x = [|10;10|];
  length[|x|]
}
*/


// VectorStore
/*
def f() : Int = {
  let x = [|10;10|];
  let _ = x[|9|] = 2;
  x[|9|]
}*/


// VectorLength
/*
def f() : Int = {
  let x = [1,2,3,4,5];
  length[x]
}*/

/*
def f() : Int = {
  let x = [|1, 2, 3|];
  length[|x|];
  head
}*/


// VectorSlice
/*
def f() : Vector[Str,10] = {
  let x = [|"Hello";10|];
  let y = x[|3..|] : Vector[Str, 5];
  y[|1..11|]
}*/


  // let y = x[|3..|];
  // y[|1..11|]
// }


/*
def f() : Vector[Int, 6] = {
  let x = [|1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20|];
  x[|15..|]
}*/

/*
def f() : Array[Array[Int]] = {
  let x = [[1,2,3],[1,2,3],[1,2,3],[1,2,3]];
  x
}*/

//-------------------------------Tests--------------------------------------------//



/*
@test
def testArrayLit00(): List[Int] = Nil

@test
def testArrayLit01(): List[Int] = 1 :: Nil

@test
def testArrayLit02(): List[Int] = 1 :: 2 :: Nil
*/

enum Color {
   case Red,
   case Blue,
   case Green
}



// def f(): Unit = let x = [|[Color.Red, Color.Green], [Color.Blue, Color.Red]|]; x[|1|] = [Color.Green, Color.Green]

// def f(): Unit = let x = [[(7 , 10)], [(7 , 10)]]; x[1] = [(19 , 10)]

// def f(): Unit = let x = [[(7 ; 10)], [(7 ; 10)]]; x[1] = [(19 ; 10)]

// def f(): Unit = let x = [[7 :: 10 :: Nil], [7 :: Nil]]; x[1] = [19 :: Nil]

// def f(): Unit = let x = [[Color.Red, Color.Green], [Color.Blue, Color.Red]]; x[1] = [Color.Green, Color.Green]

// def f(): Array[Vector[Int, 2]] = [[|12, 42|]]

// def f(): Array[Vector[Int, 2]] = [[|3 ; 2|]]

// def f(): Int = let x = [[|1, 2|], [|3, 4|]]; x[0][|0|]

// def f(): Int = let x = [[(1 :: Nil)], [(2 :: Nil)]]; x[1][0] ArrayLoad

// def f(): Vector[Array[Int], 2] = [|[2, 1], [2, 1]|]

// def f(): Vector[Array[Int], 1] = [|[3 ; 1]|]

// def f(): Array[List[Int]] = let x = [[1 :: Nil], [3 :: Nil]]; x[0]

// def f(): Int = let x = [|[1, 2], [3, 4]|]; x[|1|][0]

// def f(): Unit = let x = [|[7 :: 10 :: Nil], [7 :: Nil]|]; x[|1|] = [19 :: Nil]

// def f(): Unit = let x = [| [(1, 2)], [(3, 4)] |]; x[|1|] = [(19, 10)]

// def f(): Array[(Int, Int)] = let x = [(1, 2), (3, 4)]; let _ = x[0] = (2, 3); x Array





















