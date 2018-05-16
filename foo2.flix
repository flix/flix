/*def f() : Unit = {
  let x = [|5;11|];
  x[|-5|] = 5
}*/

def f(): Vector[Str, 4] = {
let x = [|"Str";10|];
let y = x[|0..7|];
y[|3..7|]
}