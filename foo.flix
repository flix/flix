def f() : Unit = {

  /* Virker */
  // [|1,2,3,4,5|] // (ArrayLit)
  // [|10;0|] // (ArrayNew)
  // 2[10] // ((ArrayLoad)
   [2]length // (ArrayLength)



  /* Virker ikke */
  // A 2[10] = 2 // (ArrayStore)


}

