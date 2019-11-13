rel A(x: Int)
rel B(x: Bool, y: Int)
rel C[a](x: a, y: a)

type alias X = #{ A, B }

type alias Y = #{ A, B, C[Bool] }

type alias Z[r] = #{ A | r }

@test
def declRel01(): #{ A } = #{}

@test
def declRel02(): #{ A } = #{ A(123). }

@test
def declRel03(): #{ A } = #{ A(123). A(456). }

@test
def declRel04(): #{ A, B } = #{ }

@test
def declRel05(): #{ A, B } = #{ A(123). }

@test
def declRel06(): #{ A, B } = #{ A(123). B(true, 456). }

@test
def declRel07(): #{ A, B, C[Bool] } = #{ A(123). B(true, 456). C(true, true). }

@test
def declRel08(): X = #{ A(21). B(true, 42). }

@test
def declRel09(): Y = #{ A(21). A(42). C(true, true). }

@test
def declRel10(): Unit = f(#{})

@test
def declRel11(): Unit = f(#{ A(21). })

@test
def declRel12(): Unit = f(#{ A(21). B(true, 42). })

@test
def declRel13(): Unit = f(#{ A(21). B(true, 42). C(true, true). })

def f(_: Z[_]): Unit = ()
