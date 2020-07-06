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
    match 123 {
        case _foo => 456
    }

@test
def unusedVarSym05(): Int =
    match (1, 2) {
        case (x, _y) => x
    }

@test
def unusedVarSym06(): Int =
    match (1, 2) {
        case (_x, y) => y
    }

@test
def unusedVarSym07(): Int =
    match (1, 2) {
        case (_x, _y) => 123
    }

@test
def unusedVarSym08(): Int =
    match Some(123) {
        case None => 456
        case Some(_foo) => 789
    }

@test
def unusedVarSym09(): Int =
    match Ok(123) {
        case Ok(_foo) => 456
        case Err(_bar) => 789
    }

@test
def unusedVarSym10(): Int & Impure =
    let c = chan Int 1;
    c <- 0;
    select {
        case _foo <- c => 123
    }

@test
def unusedVarSym11(): Int & Impure =
    let c1 = chan Int 1;
    let c2 = chan Int 1;
    c1 <- 0;
    c2 <- 0;
    select {
        case _foo <- c1 => 123
        case _bar <- c2 => 456
    }

