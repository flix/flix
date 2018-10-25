// TODO SJ: kan man lave tests med interpreter og ikke compiler?

def testAll(): Int =
    if (!test1()) 1 else
    if (!test2()) 2 else
    if (!test3()) 3 else
    if (!test4()) 4 else
    if (!test5()) 5 else
    if (!test6()) 6 else
    if (!test7()) 7 else
    if (!test8()) 8 else
    if (!test9()) 9 else
    if (!test10()) 10 else
    if (!test11()) 11 else
    -1


def test1(): Bool = newch Int; true

def test2(): Bool = newch Bool; true

def test3(): Bool = newch Unit; true

def test4(): Bool = newch Char; true

def test5(): Bool = newch Float32; true

def test6(): Bool = newch BigInt; true

def test7(): Bool = newch Str; true

def test8(): Bool = newch Array[Int]; true

def test9(): Bool = newch Array[Array[Int]]; true

def test10(): Bool = newch Channel[Int]; true

def test11(): Bool = true //newch Int != ()