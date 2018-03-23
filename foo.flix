def f() : Unit = {

  /* Virker */
  //[|1,2,3,4,5|] // (ArrayLit)
  // [|10;g()|] // (ArrayNew)
  // 2[10] // ((ArrayLoad)
  //  2[2] = 2 // (ArrayStore)
  // [2]length // (ArrayLength)
  // 2[2..2] // (ArraySlice)

}

def g() : Int = 42

