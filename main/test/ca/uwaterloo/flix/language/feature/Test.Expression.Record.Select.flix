@test
def testRecordSelect01(): Str =
    let r = %{ fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
        r.fstName

@test
def testRecordSelect02(): Str =
    let r = %{ fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
        r.lstName

@test
def testRecordSelect03(): Int =
    let r = %{ fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
        r.age

@test
def testRecordSelect04(): Bool =
    let r = %{ fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
        r.cowboy

@test
def testRecordMultiSelect01(): Unit =
    let r = %{ fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
    let fstName = r.fstName;
    let lstName = r.lstName;
    let age = r.age;
    let cowboy = r.cowboy;
        ()

@test
def testRecordMultiSelect02(): Unit =
    let r = %{ fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
    let cowboy = r.cowboy;
    let age = r.age;
    let lstName = r.lstName;
    let fstName = r.fstName;
        ()

@test
def testRecordMultiSelect03(): Unit =
    let r = %{ fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
    let fstName1 = r.fstName;
    let fstName2 = r.fstName;
    let lstName1 = r.lstName;
    let lstName2 = r.lstName;
        ()

@test
def testRecordMultiSelect04(): Unit =
    let r = %{ fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
    let lstName1 = r.lstName;
    let fstName1 = r.fstName;
    let lstName2 = r.lstName;
    let fstName2 = r.fstName;
        ()

@test
def testNestedSelect01(): Unit =
    let r = %{ a = %{ x = 1, y = 2}, b = %{ x = 1, y = 4 } };
    let ax = r.a.x;
    let ay = r.a.y;
    let bx = r.b.x;
    let by = r.b.y;
        ()

@test
def testNestedSelect02(): Unit =
    let r = %{ a = %{ b = %{ c = %{ d = 42 } } } };
    let a = r.a;
    let b = r.a.b;
    let c = r.a.b.c;
    let d = r.a.b.c.d;
        ()

@test
def testSelectDuplicateLabels01(): Int =
    let r = %{ a = 21, a = 42, a = 84};
    let a1 = r.a;
    let a2 = %{ -a | r}.a;
    let a3 = %{ -a, -a | r}.a;
        a1 + a2 + a3

@test
def testSelectDuplicateLabels02(): Int =
    let r = %{ a = 21, b = true, a = 42, b = false, a = 84};
    let a1 = r.a;
    let a2 = %{ -a | r}.a;
    let a3 = %{ -a, -a | r}.a;
        a1 + a2 + a3

@test
def testSelectDuplicateLabels03(): Str =
    let r = %{ a = true, a = "hello world", a = 84};
    %{ -a | r }.a

@test
def testSelectDuplicateLabels04(): Int =
    let r = %{ a = true, a = "hello world", a = 84};
    %{ -a, -a | r }.a
