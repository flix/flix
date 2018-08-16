@test
def testNewRelation01(): Bool =
    let a = new A;
    check {
        a.A(1).
    }

@test
def testNewRelation02(): Bool =
    let a = new A;
    check {
        a.A(1).
        a.A(2).
        a.A(3).
    }

@test
def testNewRelation03(): Bool =
    let a = new A;
    let b = new B;
    check {
        a.A(1).
        b.B(1).
    }

@test
def testNewRelation04(): Bool =
    let a = new A;
    let b = new B;
    check {
        a.A(1). a.A(2). a.A(3).
        b.B(1). b.B(2). b.B(3).
    }

@test
def testNewRelation05(): Bool =
    let a = new A;
    let b = new B;
    check {
        a.A(1). a.A(2). a.A(3).
        b.B(x) :- a.A(x).
    }

@test
def testNewRelation06(): Bool =
    let a = new A;
    let b = new B;
    let r = new R;
    check {
        a.A(1). a.A(2). a.A(3).
        b.B(4). b.B(5). b.B(6).
        r.R(x) :- a.A(x).
        r.R(x) :- b.B(x).
    }

@test
def testNewRelation07(): Bool =
    let a = new A;
    let b = new B;
    let r = new R;
    check {
        a.A(1). a.A(2). a.A(3).
        b.B(2). b.B(3). b.B(5).
        r.R(x) :- a.A(x), b.B(x).
    }

@test
def testNewRelation08(): Bool =
    let a1 = new A;
    let a2 = new A;
    check {
        a1.A(1). a1.A(2). a1.A(3).
        a2.A(1). a2.A(2). a2.A(3).
    }

@test
def testNewRelation09(): Bool =
    let a1 = new A;
    let a2 = new A;
    let r = new R;
    check {
        a1.A(1). a1.A(2). a1.A(3).
        a2.A(1). a2.A(2). a2.A(3).
        R(x) :- a1.A(x), a2.A(x).
    }

@test
def testNewRelation10(): Bool =
    let a = new A;
    let _ = check { a.A(1). a.A(2). a.A(3). };
    let _ = check { a.A(3). a.A(4). a.A(5). };
    check { a.A(1). }

@test
def testNewRelation11(): Bool =
    let a = new A;
    check f(a)



def f(a: A): ConstraintSet = { a.A(1). a.A(2). a.A(3). }

rel A(x: Int)
rel B(x: Int)
rel R(x: Int)
