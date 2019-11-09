@test
def matchRecord1(): Bool = match {x = 42, y = true} {
    case {x, y} => x == 42
    case _ => false
}

@test
def matchRecord2(): Bool = match {x = 42, y = true} {
    case {x} => x == 42
    case _ => false
}

@test
def matchRecord3(): Bool = match {x = 42, y = true} {
    case {y} => y
    case _ => false
}

@test
def matchRecord4(): Bool = match {z = 42, y = true} {
    case {x, y} => false
    case _ => true
}
