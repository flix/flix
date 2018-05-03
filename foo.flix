// ArrayLit
/*
def f() : Array[Int] = {
 //let x = [(1,2,3), (1,2,3), (1,2,3)];
 let x = [1,2,3,4,5];
 x
}
*/

// ArrayNew
/*
def f() : Array[Bool] = {
  let x = [true;1000];
  x
}
*/


// ArrayLoad
def f() : Int = {
  let x = [1,2,3,4,5,6];
  x[2]
}


// ArrayStore
/*
def f() : Array[Int] = {
  let x = [1,2,3,4,5];
  let _ = x[1] = 100;
  x
}
*/

//ArrayLength
/*
def f() : Int = {
  let x = [1,2,3,4];
  length[x]
}
*/


//ArraySlice
/*
def f() : Array[Int] = {
  let x = [1,2,3,4,5];
  x[0..2]
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
def f() : Vector[Int, 2] = {
  let x = [|20;20|];
  x[|15..|]
}
*/

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
