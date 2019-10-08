/*
@test
def arrayPatternEmpty(): Bool = match [] {
case [] => true
case _ => false
}
*/

@test
def arrayPatternUnit1(): Bool = match [()] {
case [()] => true
case _ => false
}

@test
def arrayPatternInt1(): Bool = match [1] {
case [1] => true
case _ => false
}

@test
def arrayPatternInt2(): Bool = match [1,2] {
case [1] => false
case _ => true
}

@test
def arrayPatternInt3(): Bool = match [1,2] {
case [1,2] => true
case _ => false
}

@test
def arrayPatternReverseInt3(): Bool = match [2,1] {
case [1,2] => false
case _ => true
}

@test
def arrayPatternIntArrays1(): Bool = match [[1],[2]] {
case [[1],[2]] => true
case _ => false
}

@test
def arrayPatternIntArraysArrays1(): Bool = match [[[1]],[[2]]] {
case [[[1]],[[2]]] => true
case _ => false
}

@test
def arrayPatternIntArrays2(): Bool = match [[1],[2]] {
case [[1]] => false
case _ => true
}

@test
def arrayPatternIntArrays3(): Bool = match [[1,2,3],[1,2,3]] {
case [[1,2,3],[1,2,3]] => true
case _ => false
}

@test
def arrayPatternStr1(): Bool = match ["1"] {
case ["1"] => true
case _ => false
}

@test
def arrayPatternBool1(): Bool = match [true] {
case [true] => true
case _ => false
}

@test
def arrayPatternTuple1(): Bool = match [(1,"1")] {
case [(1,"1")] => true
case _ => false
}

@test
def arrayPatternFloat1(): Bool = match [1.1] {
case [1.1] => true
case _ => false
}

@test
def arrayPatternByte1(): Bool = match [1i8] {
case [1i8] => true
case _ => false
}

@test
def arrayPatternShort1(): Bool = match [1i16] {
case [1i16] => true
case _ => false
}

@test
def arrayPatterni32Int1(): Bool = match [1i32] {
case [1i32] => true
case _ => false
}

@test
def arrayPatternLong1(): Bool = match [1i64] {
case [1i64] => true
case _ => false
}


