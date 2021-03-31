/// Returns the area of the rectangle `r`.
/// The record `r` must have `x` and `y` labels, and no other labels.
def area(r: {x: Int, y: Int}): Int = r.x * r.y

/// Computes the area of various rectangle records.
/// Note that the order of labels is immaterial.
def areas(): List[Int] =
    area({x = 1, y = 2}) ::
    area({y = 2, x = 3}) :: Nil

/// Returns the area of the polymorphic record `r`.
/// Note that the use of the type variable `a` permits the record `r`
/// to have labels other than `x` and `y`.
def polyArea(r: {x: Int, y: Int | a}): Int = r.x * r.y

/// Computes the area of various rectangle records.
/// Note that some records have additional fields.
def polyAreas(): List[Int] =
    polyArea({x = 1, y = 2}) ::
    polyArea({x = 2, y = 3, z = 4}) :: Nil

def main(_args: Array[String]): Int32 & Impure =
    polyAreas() |> println;
    0 // exit code
