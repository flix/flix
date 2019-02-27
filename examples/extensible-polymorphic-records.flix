/// Returns the record `r` with a new value of its `x` label.
def setX(r: {x: Int, y: Int}, v: Int): {x: Int, y: Int} =
    { x = v | r }

/// Returns the value 1 + 42 = 43.
def main(): Int =
    let r = {x = 1, y = 2};
    r.x + setX(r, 42).x

/// Returns the record `r` with a new value of its `x` label
/// polymorphically preserving all other labels.
def polySetX[a](r: {x: Int | a}, v: Int): {x: Int | a} =
    { x = v | r }

/// Returns the value 42 + 1 = 43.
def main2(): Int =
    let r = {x = 1, y = 2};
    polySetX(r, 42).x + polySetX(r, 42).y

/// Returns the record `r` extended with a label `z`.
def withZ[a](r: a, v: Int): {z: Int | a} =
    { +z = v | r }

/// Returns the value 1 + 2 + 3 = 6
def main3(): Int =
    let r = withZ({x = 1, y = 2}, 3);
    r.x + r.y + r.z
