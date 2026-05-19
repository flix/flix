pub def mismatched(a: Array[a, r1], _: Array[a, r2]): Int32 \ { r2 } =
    region _rc {
        Array.length(a)
    }
