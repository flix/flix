// Spawn without parameters
def f(): Unit = ()
spawn f()

// Spawn unit function with parameters
def f(x: Int): Unit = ()
spawn f(42)

// Spawn non-unit function
def f(): Int = 42
spawn f()

// Spawn non-unit function with parameters
def f(x: Int): Int = x
spawn f(42)

// Assignment
def f(): Int = 42
let x = spawn f()

// Addition
def f(): Int = 42
spawn f() + spawn f()

// Subtraction
def f(): Int = 42
spawn f() - spawn f()

// Multiplication
def f(): Int = 42
spawn f() * spawn f()

// Division
def f(): Int = 42
spawn f() spawn f()

// If
def f(): Bool = True
if (spawn f())
    42

