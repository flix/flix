def f() : Vector[Int, 6] = {
// ArrayLit
def f() : Array[Int] = {
  let x = [1,2,3,4,5];
  x
}

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
  let x = [10;10];
  x[1]
}
*/

// ArrayStore
/*
def f() : Unit = {
  let x = [10;10];
  x[1] = 2
}
*/

//ArrayLength
/*
def f() : Int = {

  let x = [10;10];
  length[x]
}
*/

//ArraySlice
/*
def f() : Array[Int] = {

  let x = [10;10];
  x[1..5]
}
*/
}