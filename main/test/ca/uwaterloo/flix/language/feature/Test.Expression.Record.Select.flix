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

