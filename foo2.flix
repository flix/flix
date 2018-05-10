
def f() : Int = {
  let x = unique [1; 3];
  let y = x[0..2]; // Lovlig
  //let a = x[2];    // Ulovlig
  //let z = x[0..1]; // Ulovlig

  1
}


/*
def f() : Vector[Int, 2] = {
  let x = unique [|1, 2|];
  //let x = unique [|1; 2|];
  let y = x[|0|];
  x
}
*/