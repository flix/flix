@test
def testRecordLiteral01(): %{} = %{}

@test
def testRecordLiteral02(): %{ name: Str } = %{ name = "Lucky Luke" }

@test
def testRecordLiteral03(): %{ fstName: Str, lstName: Str } = %{ fstName = "Lucky", lstName = "Luke" }

@test
def testRecordLiteral04(): %{ fstName: Str, lstName: Str, age: Int } = %{ fstName = "Lucky", lstName = "Luke", age = 42 }

@test
def testRecordLiteral05(): %{ fstName: Str, lstName: Str, age: Int, cowboy: Bool } = %{ fstName = "Lucky", lstName = "Luke", age = 42, cowboy = true }

@test
def testRecordLiteral06(): %{ name: %{fstName: Str, lstName: Str}, age: Int } = %{ name = %{ fstName = "Lucky", lstName = "Luke" }, age = 42 }

