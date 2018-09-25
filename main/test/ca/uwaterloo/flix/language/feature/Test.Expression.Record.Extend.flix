@test
def testRecordExtend01(): %{ fstName: Str } =
    let r0 = %{};
    let r1 = %{ fstName += "Lucky" | r0 };
    r1

@test
def testRecordExtend02(): %{ fstName: Str, lstName: Str } =
    let r0 = %{};
    let r1 = %{ fstName += "Lucky" | r0 };
    let r2 = %{ lstName += "Luke" | r1 };
    r2

@test
def testRecordExtend03(): %{ fstName: Str, lstName: Str, age: Int } =
    let r0 = %{};
    let r1 = %{ fstName += "Lucky" | r0 };
    let r2 = %{ lstName += "Luke" | r1 };
    let r3 = %{ age += 42 | r2 };
    r3

@test
def testRecordExtend04(): %{ fstName: Str, lstName: Str, age: Int, cowboy: Bool } =
    let r0 = %{};
    let r1 = %{ fstName += "Lucky" | r0 };
    let r2 = %{ lstName += "Luke" | r1 };
    let r3 = %{ age += 42 | r2 };
    let r4 = %{ cowboy += true | r3 };
    r4

@test
def testRecordForkExtend01(): Unit =
    let r0 = %{ lstName = "Lannister" };
    let r1 = %{ fstName += "Tywin" | r0 };
    let r2 = %{ fstName += "Jaime" | r0 };
    let r3 = %{ lstName += "Cersei" | r0 };
    let r4 = %{ lstName += "Tyrion" | r0 };
    ()

@test
def testRecordForkExtend02(): Unit =
    let r0 = %{ x = 0, y = 0 };
    let r1 = %{ z += 0 | r0 };
    let r2 = %{ color += "red" | r1 };
    let r4 = %{ color += "blu" | r1 };
    ()

