@test
def unusedTypeVar01(_a: _): Bool = true

@test
def unusedTypeVar02(a: List[_]): Int = List.length(a)

@test
def unusedTypeVar03(a: {x: Int | _}): Int = a.x

@test
def unusedTypeVar04(_a: _, b: t): t = b

@test
def unusedTypeVar05(a: List[_], b: List[t]): Int = List.length(a) - List.length(b)

@test
def unusedTypeVar06(a: {x: t | _}): t = a.x

@test
def unusedTypeVar07(a: {x: Int | _}, b: List[_]): Bool = a.x == List.length(b)

@test
def unusedTypeVar08(_a: Int -> Bool & _, b: String): String = b
