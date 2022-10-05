def function1() : Int32 = {
    let x = 4;
    let y = x;
    y
}

def function2() : Int32 = {
    let z = function1();
    z
}
