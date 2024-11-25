/// Returns the record `r` with a new value of its `x` label.
def setX(r: {x = Int32, y = Int32}, v: Int32): {x = Int32, y = Int32} =
    { x = v | r }

/// Returns the value 1 + 3 = 4.
def main2(): Int32 =
    let r1 = {x = 1, y = 2};
    let r2 = setX(r1, 3);
    r1#x + r2#x

/// Returns the record `r` with a new value of its `y` label.
/// Preserves (retains) all other labels polymorphically.
def setY(r: {y = Int32 | a}, v: Int32): {y = Int32 | a} =
    { y = v | r }

/// Returns the value 0 + 1 + 3 = 4.
def main(): Unit \ IO =
    let r1 = {x = 1, y = 2};
    let r2 = {x = 1, y = 2, z = 3};
    let r3 = setY(r1, 0);
    let r4 = setY(r2, 1);
    (r3#y + r4#y + r4#z) |> println
