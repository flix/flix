@test
def unusedVarSym01(): Int =
    let _ = 123;
    456

@test
def unusedVarSym02(): Int =
    let _foo = 123;
    456

@test
def unusedVarSym03(): Int =
    let _foo = 123;
    let _bar = 456;
    789

@test
def unusedVarSym04(): Int =
    match 123 with {
        case _foo => 456
    }

@test
def unusedVarSym05(): Int =
    match (1, 2) with {
        case (x, _y) => x
    }

@test
def unusedVarSym06(): Int =
    match (1, 2) with {
        case (_x, y) => y
    }

@test
def unusedVarSym07(): Int =
    match (1, 2) with {
        case (_x, _y) => 123
    }

