def f() : Int64 = {
 let x = [10i64;10];
 let _ = x[1] = 5i64;
 //x[2]
 x[1]
}

// ArrayNew
/*
def f() : Array[Bool] = {
  let x = [true;1000];
  x
}
*/


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

//ArrayLength
/*
def f() : Int = {
  let x = [1,2,3,4,5];
  length[x]
}
*/


//ArraySlice
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
def f() : Array[Unit] = {
  let x = [];
  x
}
*/

// VectorNew
/*
def f() : Vector[Int, 15] = {
  let x = [|10;15|];
  x
}*/


// VectorLoad
/*
def f() : Int = {
  let x = [|10;10|];
  x[|9|]
}*/


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

  let x = [|[|1,2,3|], [|1,2,3|], [|1,2,3|]|];
  length[|x|]
}*/


/*def f() : Int = {
  let x = [|1, 2, 3|];
  length[|x|]
}
*/

// VectorSlice
/*
def f() : Vector[Str,10] = {
  let x = [|"Hello";10|];
  let y = x[|3..|] : Vector[Str, 5];
  y[|1..11|]
}*/

  //let y = x[|3..|];
  //y[|1..11|]
//}


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


/*
// def testArrayTuple1(): Array[(Int, Str), (Int, Bool)] = [(12, "Hello")] // Error array can only contain one type

def testArrayTuple2(): Array[(Int, Int)] = [(12, 42)] // Works

def testArrayTuple3(): Array[(Int, Int, Int)] = [(12, 42, 34)] // Works

def testArrayTuple4(): Array[(Int, Int)] = [(12, 42), (12, 23)] //Error


*/



