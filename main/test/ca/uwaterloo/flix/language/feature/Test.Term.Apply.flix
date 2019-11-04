@test
def termApp01(): #{ R } = solve #{
    R(zer()).
    R(one()).
    R(two()).
}

@test
def termApp02(): #{ R } = solve #{
    R(inc(1)).
    R(dec(2)).
}

@test
def termApp03(): #{ R } = solve #{
    R(inc(dec(1))).
    R(dec(inc(2))).
}

@test
def termApp04(): #{ A, B, R } = solve #{
    A(0). A(1). A(3).
    B(1). B(2). B(3).

    R(inc(x)) :- A(x).
    R(dec(x)) :- B(x).
}

@test
def termApp05(): #{ A, B, R } = solve #{
    A(0). A(1). A(3).
    B(1). B(2). B(3).

    R(inc(dec(x))) :- A(x), B(x).
}

@test
def termApp06(): #{ A, B, R } = solve #{
    A(0). A(1). A(3).
    B(1). B(2). B(3).

    R(add(x, y)) :- A(x), B(y).
}

@test
def termApp07(): #{ A, B, R } = solve #{
    A(0). A(1). A(3).
    B(1). B(2). B(3).

    R(add(x)(y)) :- A(x), B(y).
}

@test
def termApp08(): #{ A, B, R } = solve #{
    A(0). A(1). A(3).
    B(1). B(2). B(3).

    R(add(inc(x), dec(y))) :- A(x), B(y).
}

@test
def termApp09(): #{ R } = solve #{
    R(identity(42)).
}

@test
def termApp10(): #{ R } = solve #{
    R(fst((1, 2))).
    R(snd((1, 2))).
}

@test
def termApp11(): Bool =
    let f = zer;
    let m = solve #{
        R(f()).
    };
    m |= R(0).

@test
def termApp12(): Bool =
    let f = one;
    let m = solve #{
        R(f()).
    };
    m |= R(1).

@test
def termApp13(): Bool =
    let f = two;
    let m = solve #{
        R(f()).
    };
    m |= R(2).

@test
def termApp14(): Bool =
    let f = inc;
    let m = solve #{
        A(1). A(2). A(3).
        R(f(x)) :- A(x).
    };
    m |= R(4).

@test
def termApp15(): Bool =
    let f = dec;
    let m = solve #{
        A(1). A(2). A(3).
        R(f(x)) :- A(x).
    };
    m |= R(0).

@test
def termApp16(): Bool =
    let f = add;
    let m = solve #{
        A(1). A(2). A(3).
        R(f(x, y)) :- A(x), A(y).
    };
    m |= R(6).

@test
def termApp17(): Bool =
    let m = solve ho1(inc);
    m |= R(4).

@test
def termApp18(): Bool =
    let m = solve ho1(dec);
    m |= R(0).

@test
def termApp19(): Bool =
    let m = solve ho2(add);
    m |= R(6).

@test
def termApp20(): Bool =
    let m = solve ho3(inc);
    m |= P(4).

@test
def termApp21(): Bool =
    let m = solve ho3(dec);
    m |= P(0).

@test
def termApp22(): Bool =
    let m = solve ho3(_ -> true);
    m |= P(true).

@test
def termApp23(): Bool =
    let m = solve ho3(_ -> "hello");
    m |= P("hello").

def zer(): Int = 0
def one(): Int = 1
def two(): Int = 2
def inc(x: Int): Int = x + 1
def dec(x: Int): Int = x - 1
def add(x: Int, y: Int): Int = x + y

pub def ho1(f: Int -> Int): #{A, R} = #{
    A(1). A(2). A(3).
    R(f(x)) :- A(x), A(y).
}

pub def ho2(f: (Int, Int) -> Int): #{A, R} = #{
    A(1). A(2). A(3).
    R(f(x, y)) :- A(x), A(y).
}

pub def ho3(f: Int -> a): #{A, P[a]} = #{
    A(1). A(2). A(3).
    P(f(x)) :- A(x).
}

rel A(x: Int)
rel B(x: Int)
rel R(x: Int)
rel P[a](a: a)
