// TODO: Currently broken
//@test
//def testRecordPolymorphism01[r](): %{ r } = %{ fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true }

@test
def testRecordPolymorphism02[r](): %{ fstName: Str | r } = %{ fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true }

@test
def testRecordPolymorphism03[r](): %{ lstName: Str | r } = %{ fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true }

@test
def testRecordPolymorphism04[r](): %{ age: Int | r } = %{ fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true }

@test
def testRecordPolymorphism05[r](): %{ cowboy: Bool | r } = %{ fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true }

@test
def testRecordPolymorphism06[r](): %{ fstName: Str, lstName: Str | r } = %{ fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true }

@test
def testRecordPolymorphism07[r](): %{ fstName: Str, age: Int | r } = %{ fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true }

@test
def testRecordPolymorphism08[r](): %{ fstName: Str, cowboy: Bool | r } = %{ fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true }

@test
def testRecordPolymorphism09[r](): %{ lstName: Str, fstName: Str | r } = %{ fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true }

// TODO: Currently broken
//
//@test
//def testRecordPolyExtend01(): Unit =
//    let p1 = %{ fstName = "Lucky", lstName = "Luke" };
//    let p2 = %{ fstName = "Gandalf", lstName = "the Grey" };
//    let p3 = withAge(p1, 42);
//    let p4 = withAge(p2, 42);
//    ()
//
//
//def withAge(rec: %{ fstName: Str, lstName: Str }, age: Int): %{ fstName: Str, lstName: Str, age: Int } = { age += age | rec }
