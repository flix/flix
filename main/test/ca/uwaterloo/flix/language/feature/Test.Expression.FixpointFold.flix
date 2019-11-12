rel F(x: Int, y: Int)

def add(entry: (Int, Int), acc: Int): Int =
  let (x, y) = entry;
  acc + x + y

@test
def testFixpointFold01(): Int =
  let p = #{F(0, 1). F(1, 1).};
  fold F 0 add (solve p)

@test
def testFixpointFold02(): Int =
  let p = #{F(0, 1). F(1, 1).};
  fold F 0 ((_entry, acc) -> acc) (solve p)

@test
def testFixpointFold03(): Bool =
  let p = #{F(0, 1). F(1, 1).};
  let res = fold F 0 add (solve p);
  res == 3

@test
def testFixpointFold04(): Bool =
  let p = #{F(0, 1). F(1, 1).};
  (fold F 0 add (solve p)) == 3
