// ArrayLit
/*
def f() : Array[Int] = {
 let x = [1,2,3,4,5];
 x
}
*/

// ArrayNew
/*
def f() : Array[Int] = {
  let x = [10;10];
  x
}
*/

// ArrayLoad
/*
def f() : Int = {
  let x = [[1,2,3],[1,2,3],[1,2,3]];
  x[2][2]
}
*/

// ArrayStore
/*
def f() : Unit = {
  let x = [1,2,3,4,5];
  x[1] = 2
}
*/

//ArrayLength
/*
def f() : Int = {

  let x = [1,2,3,4,5,6,7,8,9,10];
  length[x]
}
*/

//ArraySlice
/*
def f() : Array[Int] = {

  let x = [0,1,2,3,4,5];
  x[2..4]
}
*/

// VectorLit
/*
def f() : Vector[Int, 5] = {
  let x = [|1,2,3,4,5|];
  x
}
*/

// VectorNew
/*
def f() : Vector[Int, 10] = {
  let x = [|10;10|];
  x
}
*/

// VectorLoad
/*
def f() : Int = {
  let x = [|10;10|];
  x[|2|]
}
*/

// VectorStore
/*
def f() : Unit = {
  let x = [|10;10|];
  x[|1|] = 2
}
*/

// VectorLength
/*
def f() : Int = {

  let x = [|10;10|];
  length[|x|]
}
*/

// VectorSlice
/*
def f() : Vector[Int, 4] = {

  let x = [|10;10|];
  x[|6..10|]
}
*/



def f() : Array[Array[Int]] = {
  let x = [[1,2,3],[1,2,3],[1,2,3],[1,2,3]];
  x
}


