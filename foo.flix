def f() : Int = {


  let x = [1,2,3];
  length[x]

  /* Array */
  // [1,2,3,4,5] // (ArrayLit)
  // [10;g()] // (ArrayNew)
  // 2[10] // ((ArrayLoad)
  // 2[2] = 2 // (ArrayStore)
  // length[2] // (ArrayLength)
  // 2[2..2] // (ArraySlice)
  // 2[2..] // (ArraySliceNoEndIndex)
  // 2[..2] // (ArraySliceNoStartIndex)

  /* Vector */
  // [|1|]
  //[|1;10|]
  //V[1]length
  //   2[|2 .. 3|]
  //[|1,123|]
  //[| 1 ; 10|]
  //2V[21] = 2
  //V[2]length
  //2V[2..2]



}

def g() : Int = 42

