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
//
//@test
//def testRecordMultiSelect01(): Unit =
//    let r = %{ fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
//    let fstName = r.fstName;
//    let lstName = r.lstName;
//    let age = r.age;
//    let cowboy = r.cowboy;
//        ()
//
//@test
//def testRecordMultiSelect02(): Unit =
//    let r = %{ fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
//    let cowboy = r.cowboy;
//    let age = r.age;
//    let lstName = r.lstName;
//    let fstName = r.fstName;
//        ()
//
//@test
//def testRecordMultiSelect03(): Int =
//    let r = %{ fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
//    let fstName1 = r.fstName;
//    let fstName2 = r.fstName;
//    let lstName1 = r.lstName;
//    let lstName2 = r.lstName;
//        ()
//
//@test
//def testRecordMultiSelect04(): Int =
//    let r = %{ fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
//    let lstName1 = r.lstName;
//    let fstName1 = r.fstName;
//    let lstName2 = r.lstName;
//    let fstName2 = r.fstName;
//        ()
//
