@test
def eqClosure01(): Bool =
    let f = x -> x + 42i32;
    f == f

@test
def eqClosure02(): Bool =
    let f = (x, y) -> x + y + 42i32;
    f == f

@test
def eqClosure03(): Bool =
    let f = (x, y, z) -> x + y + z + 42i32;
    f == f

@test
def eqClosure04(): Bool = one != one

@test
def eqClosure05(): Bool = inc != inc

@test
def eqClosure06(): Bool = add != add

@test
def eqClosure07(): Bool = add(0) != add(0)

def one(): Int = 1
def inc(x: Int): Int = x + 1
def add(x: Int, y: Int): Int = x + y
