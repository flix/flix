@test
def testRecordRestrict01(): %{ lstName: Str, age: Int, cowboy: Bool } =
    let r0 = %{ fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
    let r1 = %{ -fstName | r0 };
    r1

@test
def testRecordRestrict02(): %{ age: Int, cowboy: Bool } =
    let r0 = %{ fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
    let r1 = %{ -fstName | r0 };
    let r2 = %{ -lstName | r1 };
    r2

@test
def testRecordRestrict03(): %{ cowboy: Bool } =
    let r0 = %{ fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
    let r1 = %{ -fstName | r0 };
    let r2 = %{ -lstName | r1 };
    let r3 = %{ -age | r2 };
    r3

@test
def testRecordRestrict04(): %{ } =
    let r0 = %{ fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
    let r1 = %{ -fstName | r0 };
    let r2 = %{ -lstName | r1 };
    let r3 = %{ -age | r2 };
    let r4 = %{ -cowboy | r3 };
    r4

@test
def testRecordRestrict05(): %{ } =
    let r0 = %{ fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
    %{ -fstName, -lstName, -age, -cowboy | r0 }

@test
def testRecordRestrictUnordered01(): %{ } =
    let r0 = %{ fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
    %{ -lstName, -fstName, -age, -cowboy | r0 }

@test
def testRecordRestrictUnordered02(): %{ } =
    let r0 = %{ fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
    %{ -lstName, -age, -fstName, -cowboy | r0 }

@test
def testRecordRestrictUnordered03(): %{ } =
    let r0 = %{ fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true };
    %{  -cowboy, -age, -lstName, -fstName | r0 }


@test
def testRecordRestrictDuplicateLabels01(): %{ a: Int, a: Int } =
    let r = %{ a = 1, a = 2, a = 3};
        %{ -a | r }

@test
def testRecordRestrictDuplicateLabels02(): %{ a: Int } =
    let r = %{ a = 1, a = 2, a = 3};
        %{ -a, -a | r }

@test
def testRecordRestrictDuplicateLabels03(): %{} =
    let r = %{ a = 1, a = 2, a = 3};
        %{ -a, -a, -a | r }

@test
def testRecordRestrictDuplicateLabels04(): %{ b: Int, a: Int, b: Int, a: Int } =
    let r = %{ a = 1, b = 42, a = 2, b = 84, a = 3};
        %{ -a | r }

@test
def testRecordRestrictDuplicateLabels05(): %{ a: Int, b: Int, a: Int } =
    let r = %{ a = 1, b = 42, a = 2, b = 84, a = 3};
        %{ -a, -b | r }

@test
def testRecordRestrictDuplicateLabels06(): %{ b: Int, a: Int } =
    let r = %{ a = 1, b = 42, a = 2, b = 84, a = 3};
        %{ -a, -b, -a | r }

@test
def testRecordRestrictDuplicateLabels07(): %{ a: Int } =
    let r = %{ a = 1, b = 42, a = 2, b = 84, a = 3};
        %{ -a, -b, -a, -b | r }

@test
def testRecordRestrictDuplicateLabels08(): %{} =
    let r = %{ a = 1, b = 42, a = 2, b = 84, a = 3};
        %{ -a, -b, -a, -b, -a | r }

