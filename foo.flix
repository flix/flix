def f() : Int = {

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
   [|1|]
  //[|1;10|]
  //Vlength[|1|]

}
 //def g() : 47i32 = 47


