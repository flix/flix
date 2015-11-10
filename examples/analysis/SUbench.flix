
//// Inputs
//// =========
  rel AddrOf(variable: Str, object: Str)
  rel Copy(to: Str, from: Str)
  rel Store(label: Str, to: Str, from: Str)
  rel Load(label: Str, to: Str, from: Str)
  rel CFG(label: Str, to: Str, from: Str)
  rel Multi(object: Str)
  rel Phi(label: Str)
  rel Clear(label: Str)
  rel FIStore(to: Str, from: Str, unknown: Str)
  rel FILoad(to: Str, from: Str, unknown: Str)

//// Outputs
//// =========
  rel Pt(variable: Str, target: Str)
  rel PtSU(location: Str, object: Str, target: Str)

//// Other types
//// =========
  enum SULattice {
    case Top,
    case Single(Str),
    case Bottom
  }

  def leq(e1: SULattice, e2: SULattice): Bool = match (e1, e2) with {
    case (SULattice.Bottom, _) => true;
    case (_, SULattice.Top) => true;
    case (SULattice.Single(s1), SULattice.Single(s2)) => s1 == s2;
    case _ => false;
  }
  def lub(e1: SULattice, e2: SULattice): SULattice = match (e1, e2) with {
    case (SULattice.Bottom, e2) => e2;
    case (e1, SULattice.Bottom) => e1;
    case (SULattice.Single(s1), SULattice.Single(s2)) => if (s1 == s2) SULattice.Single(s1) else SULattice.Top;
    case _ => SULattice.Top;
  }
  def glb(e1: SULattice, e2: SULattice): SULattice = match (e1, e2) with {
    case (SULattice.Top, e2) => e2;
    case (e1, SULattice.Top) => e1;
    case (SULattice.Single(s1), SULattice.Single(s2)) => if (s1 == s2) SULattice.Single(s1) else SULattice.Bottom;
    case _ => SULattice.Bottom;
  }
  let SULattice<> = (SULattice.Bottom, SULattice.Top, leq, lub, glb);

  lat SUBefore(location: Str, object: Str, target: SULattice<>)
  lat SUAfter(location: Str, object: Str, target: SULattice<>)

  rel PtH(object: Str, target: Str)
  rel KillEmpty(location: Str)
  rel KillNot(location: Str, object: Str)
  rel AllObjects(object: Str)
  rel NonStore(location: Str)

//// Rules
//// =========
//
//// AddrOf
//// ---------
Pt(p,a) :- AddrOf(p,a).
//
//// Copy
//// ---------
Pt(p,a) :- Copy(p,q), Pt(q,a).
//
//// Store
//// ---------
def f(x: Str): SULattice = SULattice.Single(x)
SUAfter(l,a,f(b)) :- Store(l,p,q), Pt(p,a), Pt(q,b).
//
PtH(a,b) :- Store(l,p,q), Pt(p,a), Pt(q,b).
PtH(a,b) :- FIStore(p,q,_), Pt(p,a), Pt(q,b).
//
//// Load
//// ---------
Pt(p,b) :- Load(l,p,q), Pt(q,a), PtSU(l,a,b).
Pt(p,b) :- FILoad(p,q,_), Pt(q,a), PtH(a,b).
//
//// CFlow
//// ---------
SUBefore(l2,a,t) :- CFG(l1,l2), SUAfter(l1,a,t).
//
//// Preserve
//// ---------
////su-after(l,a,t) :- su-before(l,a,t), NOT kill(l,a).
SUAfter(l,a,t) :- SUBefore(l,a,t), KillEmpty(l).
SUAfter(l,a,t) :- SUBefore(l,a,t), KillNot(l,a).
SUAfter(l,a,t) :- SUBefore(l,a,t), Multi(a).
SUAfter(l,a,t) :- SUBefore(l,a,t), Phi(l).
//
//// PtSu
//// ---------
def filter(e: SULattice, p: Str): Bool = match e with {
  case SULattice.Bottom => false
  case SULattice.Single(s) => p == s
  case SULattice.Top => true
}
PtSU(l,a,b) :- filter(t, b), SUBefore(l,a,t), PtH(a,b).
SUAfter(l,a,b) :- Clear(l), PtH(a,b).

//// Kill
//// ---------
AllObjects(a) :- AddrOf(_,a).
//// KillEmpty l is true if the kill set of l is the empty set
KillEmpty(l) :- Store(l,p,q), Pt(p,a), Pt(p,b), notEqual(a,b).
KillEmpty(l) :- NonStore(l).
//// the inverse of the kill set of l
def notEqual(a: Str, b: Str): Bool = a != b;
KillNot(l,a) :- Store(l,p,q), Pt(p,b), AllObjects(a), notEqual(a, b).
//
//// NonStore
//// ---------
NonStore(l) :- Load(l,_,_).
//

rel Foo(v: Str)
Foo(v) :- Pt(v, "2 <null>").

print PtH.
print Foo.
print Pt.
