@test
def guardedPatternMatch01: Bool =
    let r = match Some('a') with {
        case Some(x) if x == 'a' => 1
        case Some(x) if x == 'b' => 2
        case Some(x) if x == 'c' => 3
        case _                   => 42
    };
    r `assertEq!` 1

@test
def guardedPatternMatch02: Bool =
    let r = match Some('b') with {
        case Some(x) if x == 'a' => 1
        case Some(x) if x == 'b' => 2
        case Some(x) if x == 'c' => 3
        case _                   => 42
    };
    r `assertEq!` 2

@test
def guardedPatternMatch03: Bool =
    let r = match Some('c') with {
        case Some(x) if x == 'a' => 1
        case Some(x) if x == 'b' => 2
        case Some(x) if x == 'c' => 3
        case _                   => 42
    };
    r `assertEq!` 3

@test
def guardedPatternMatch04: Bool =
    let r = match Some('z') with {
        case Some(x) if x == 'a' => 1
        case Some(x) if x == 'b' => 2
        case Some(x) if x == 'c' => 3
        case _                   => 42
    };
    r `assertEq!` 42

@test
def guardedPatternMatch05: Bool =
    let r = match None with {
        case Some(x) if x == 'a' => 1
        case Some(x) if x == 'b' => 2
        case Some(x) if x == 'c' => 3
        case _                   => 42
    };
    r `assertEq!` 42

