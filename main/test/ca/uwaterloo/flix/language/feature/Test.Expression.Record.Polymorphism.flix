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

@test
def testRecordNonPolyExtend01(): Unit =
    let p1 = %{ x = 0, y = 0 };
    let p2 = %{ x = 1, y = 2 };
    let p3 = %{ x = 7, y = 8 };
    let p4 = withZ(p1, 21);
    let p5 = withZ(p2, 42);
    let p6 = withZ(p2, 84);
    ()

def withZ(rec: %{ x: Int, y: Int }, z: Int): %{ x: Int, y: Int, z: Int } = %{ z += z | rec }

@test
def testRecordNonPolyExtend02(): Unit =
    let p1 = %{ fstName = "Luke", lstName = "Skywalker" };
    let p2 = %{ fstName = "Lucky", lstName = "Luke" };
    let p3 = %{ fstName = "Gandalf", lstName = "the Grey" };
    let p4 = withAge(p1, 21);
    let p5 = withAge(p2, 42);
    let p6 = withAge(p2, 84);
    ()

def withAge(rec: %{ fstName: Str, lstName: Str }, age: Int): %{ fstName: Str, lstName: Str, age: Int } = %{ age += age | rec }

