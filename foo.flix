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
/*def f() : Int = {
  let x = [[1,2,3],[1,2,3],[1,2,3]];
  let _ =  x[1][1] = 100;
  x[1][1]
}*/

//ArrayLength
/*
def f() : Int = {
  let x = [[1,2,3], [1,2,3], [1,2,3], [1,2,3], [1,2,3], [1,2,3], [1,2,3], [1,2,3]];
  length[x]
}*/


//ArraySlice

/*def f() : Array[Int] = {
  let x = [1,2,3];
  x[0..]
}*/


// VectorLit
/*
def f() : Vector[Int, 5] = {
  let x = [|1, 2, 3, 4, 5|];
  x
}*/

//VectorNew
/*
def f() : Vector[Int, 5] = {
  let x = [|5;5|];
  x
}*/

//VectorLoad
/*
def f() : Int = {
  let x = [|5;5|];
  x[|3|]
}*/

//VectorStore
/*def f() : Unit = {
  let x = [|5;5|];
  x[|4|] = 6
}
*/

// VectorLit
/*
def f() : Vector[Int, 5] = {
  let x = [|1,2,3,4,5|];
  x
}*/


// VectorNew
/*
def f() : Vector[Int, 15] = {
  let x = [|10;15|];
  x
}
*/

// VectorLoad
/*
def f() : Int = {
  let x = [|10;10|];
  x[|9|]
}*/


// VectorStore
/*
def f() : Unit = {
  let x = [|10;10|];
  x[|9|] = 2
}*/


// VectorLength
/*
def f() : Int = {

  let x = [|[|1,2,3|], [|1,2,3|], [|1,2,3|]|];
  length[|x|]
}*/
/*
def f() : Int = {


  let x = [|1, 2, 3|];
  length[|x|]
}
*/
// VectorSlice
/*
def f() : Vector[Int, 55] = {
  let x = [|10;10|];
  x[|3..5|]
}*/

/*
def f() : Array[Array[Int]] = {
  let x = [[1,2,3],[1,2,3],[1,2,3],[1,2,3]];
  x
}*/
