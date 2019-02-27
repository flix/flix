/// Returns the area of the rectangle `r` represented as a record.
/// The record `r` must have `x` and `y` labels, and no other labels.
def area(r: {x: Int, y: Int}): Int = r.x * r.y

/// Computes the area of various rectangle records.
/// Note that the order of labels is immaterial.
def areas(): List[Int] =
    area({x = 2, y = 4}) ::
    area({y = 8, x = 5}) :: Nil

/// Returns the area of the polymorphic record `r`.
/// Note that the use of the type variable `a` permits the records
/// to have labels other than `x` and `y`.
def polyArea[a](r: {x: Int, y: Int | a}): Int = r.x * r.y

/// Computes the area of various rectangle records.
/// Note that some records have additional fields.
def polyAreas(): List[Int] =
    polyArea({x = 2, y = 4}) ::
    polyArea({x = 2, y = 4, z = 8}) :: Nil

def main(): List[Int] = polyAreas()
