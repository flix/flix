enum MyList[a: Type, _: Bool] {
  case MNil
  case MCons(a, MyList[a, true])
}

def _tail(list: MyList[a, false]): MyList[a, true] = match list {
  case MyList.MCons(_, newList) => newList
  case MyList.MNil              => bug!("Error: Nil input not possible")
}

def _head(list: MyList[a, false]): a = match list {
  case MyList.MCons(elem, _) => elem
  case MyList.MNil           => bug!("Error: Nil input not possible")
}

def _mkNil(): MyList[a, true] = MyList.MNil

def _cons(elem: a, list: MyList[a, true]): MyList[a, false] = MyList.MCons(elem, list)
