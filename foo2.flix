/*
def f() : Int = {
  let x = unique [1, 2];
  let _ = x[0] = 2;
  //let a = x;
  //x[0]
  1
}
*/

/*
def f() : Int = {
  let x = unique [|1, 3|];
  let y = x[|0..2|];
  let a = 1 + 2;
  //x[|0|]
  1
}*/

/*
def f() : Int = {
  let x = (1 :: 2 :: 3 :: Nil);
  let y = x;
  //let z = x;
  1
}
*/

/*
def f(): List[Int] = {
  let x = (1 :: 2 :: 3 :: Nil);
  x
}
*/

def f() : Set[Int] = #{1}