@test
def testRecordUpdate01(): %{ x: Int, y: Int, z: Int } =
    let r = %{ x = 0, y = 0, z = 0 };
    %{ x := 123 | r }

@test
def testRecordUpdate02(): %{ x: Int, y: Int, z: Int } =
    let r = %{ x = 0, y = 0, z = 0 };
    %{ y := 123 | r }

@test
def testRecordUpdate03(): %{ x: Int, y: Int, z: Int } =
    let r = %{ x = 0, y = 0, z = 0 };
    %{ z := 123 | r }

@test
def testRecordUpdate04(): %{ x: Int, y: Int, z: Int } =
    let r = %{ x = 0, y = 0, z = 0 };
    %{ x := 123, y := 456, z := 789 | r }

@test
def testRecordUpdate05(): %{ x: Int, y: Int, z: Int } =
    let r = %{ x = 0, y = 0, z = 0 };
    %{ z := 789, y := 456, x := 123 | r }
