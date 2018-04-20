// ArrayNew
/*
def f() : Array[Int] = {
  let x = [10;10];
  x
}
*/

// ArrayLoad
def f() : Int = {
  let x = [1,2,3,4,5];
  x[2]
}

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

//VectorSlice
/*def f() : Vector[Int, 3] = {
 let x =  [|5;5|];
  x[|0..3|]
}*/
// Test for alle 4 cases i slice, load og store.
