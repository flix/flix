def f() : Unit = {

  V[1,1]
  //V[1;10]
  //2V[21] = 2
  //V[2]length
  //2V[2..2]
  /* Virker */
  // [|1,2,3,4,5|] // (ArrayLit)
  // [|10;0|] // (ArrayNew)
  // 2[10] // ((ArrayLoad)
   //[2]length // (ArrayLength)



  /* Virker ikke */
  // A 2[10] = 2 // (ArrayStore)


}

def g() : Int = 42

