// ArrayLit
/*
def f() : Array[Int] = {
 let x = [];
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
def f() : Int = {
  let x = [[[1,2,3],[1,2,3]], [[1,2,3],[1,2,3]], [[1,2,3],[1,2,3]]];
  let _ =  x[1][1][1] = 100;
  x[1][1][1]
}
/*
def f(): Int = {
  let x = [1,2,3,4,5];
  let _ = x[1] = 100;
  x[1]
}
*/

//ArrayLength
/*
def f() : Int = {
  let x = [[1,2,3], [1,2,3], [1,2,3], [1,2,3], [1,2,3], [1,2,3], [1,2,3], [1,2,3]];
  length[x]
}*/


//ArraySlice
/*
def f() : Array[Array[Int]] = {
  let x = [[1,2,3], [1,2,3], [1,2,3]];
  x[0..]
}
*/

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

//ArraySlice

/*def f() : Vector[Int, 1] = {

  let x = [|[|1|]|];
  x[|0..1|]
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
  x[|9|]
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

  let x = [|[|1,2,3|], [|1,2,3|], [|1,2,3|]|];
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

/*
def f() : Array[Array[Int]] = {
  let x = [[1,2,3],[1,2,3],[1,2,3],[1,2,3]];
  x
}*/
