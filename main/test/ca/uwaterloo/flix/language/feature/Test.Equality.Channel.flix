@test
def eqChannel01(): Bool =
    let c = chan Bool 0;
    c == c

@test
def eqChannel02(): Bool =
    let c = chan Int 0;
    c == c

@test
def eqChannel03(): Bool =
    let c = chan Str 0;
    c == c

@test
def eqChannel04(): Bool = (chan Bool 0) != (chan Bool 0)

@test
def eqChannel05(): Bool = (chan Int 0) != (chan Int 0)

@test
def eqChannel06(): Bool = (chan Str 0) != (chan Str 0)
