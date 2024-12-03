/// Returns the area of the rectangle `r`.
/// The record `r` must have `x` and `y` labels, and no other labels.
def area(r: {x = Int32, y = Int32}): Int32 = r#x * r#y

/// Computes the area of various rectangle records.
/// Note that the order of labels is immaterial.
def areas(): List[Int32] =
    area({x = 1, y = 2}) ::
    area({y = 2, x = 3}) :: Nil

/// Returns the area of the polymorphic record `r`.
/// Note that the use of the type variable `a` permits the record `r`
/// to have labels other than `x` and `y`.
def polyArea(r : {x = Int32, y = Int32 | a}): Int32 = r#x * r#y

/// Computes the area of various rectangle records.
/// Note that some records have additional labels.
def polyAreas(): List[Int32] =
    polyArea({x = 1, y = 2}) ::
    polyArea({x = 2, y = 3, z = 4}) :: Nil

def main(): Unit \ IO =
    areas() |> println;
    polyAreas() |> println
