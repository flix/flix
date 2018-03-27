def f01(): Unit = ()

@test
spawn f01()

def f02(x: Int): Unit = ()

@test
spawn f02(42)

def f03(): Int = 42

@test
spawn f03()


def f04(x: Int): Int = x

@test
spawn f04(42)

def f05(): Int = 42

@test
let x = spawn f05()

def f06(): Int = 42

@test
spawn f06() + spawn f()

def f07(): Int = 42

@test
spawn f07() - spawn f()

def f08(): Int = 42

@test
spawn f08() * spawn f08()

def f09(): Int = 42

@test
spawn f09() spawn f09()

def f10(): Bool = True

@test
if (spawn f10())
    42

