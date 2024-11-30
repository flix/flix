import java.lang.Math

def pythagoras(x: Float64, y: Float64): Float64 =
    // We know that Math.sqrt and Math.pow are pure, but by default
    // they have the `IO` effect. Hence we write `unsafe` to cast
    // the expression to pure thus hiding the IO effect.
    unsafe Math.sqrt((Math.pow(x, 2.0) + Math.pow(y, 2.0)))

def main(): Unit \ IO =
    println(pythagoras(3.0, 4.0))
