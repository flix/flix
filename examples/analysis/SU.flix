namespace SU {

//// Inputs
//// =========
  rel AddrOf(variable: Str, object: Str)
  rel Copy(to: Str, from: Str)
  rel Store(label: Str, to: Str, from: Str)
  rel Load(label: Str, to: Str, from: Str)
  rel CFG(label: Str, to: Str, from: Str)

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
    case (SULattice.Single s1, SULattice.Single s2) => s1 == s2;
    case _ => false;
  }
  def lub(e1: SULattice, e2: SULattice): SULattice = match (e1, e2) with {
    case (Bottom, e2) => e2;
    case (e1, Botttom) => e1;
    case (SULattice.Single s1, SULattice.Single s2) => if (s1 == s2) SULattice.Single(s1) else SULattice.Top;
    case _ => SULattice.Top;
  }
  def glb(e1: SULattice, e2: SULattice): SULattice = match (e1, e2) with {
    case (Top, e2) => e2;
    case (e1, Top) => e1;
    case (SULattice.Single s1, SULattice.Single s2) => if (s1 == s2) SULattice.Single(s1) else SULattice.Bottom;
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
//
//// Load
//// ---------
Pt(p,b) :- Load(l,p,q), Pt(q,a), PtSU(l,a,b).
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
//
//// PtSu
//// ---------
def filter(e: SULattice, p: Str): Bool = match e with {
  case SULattice.Bottom => false
  case SULattice.Single(s) => p == s
  case SULattice.Top => true
}
PtSU(l,a,b) :- filter(t, b), SUBefore(l,a,t), PtH(a,b).

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
//// Example
//// =========
//
//// p = &a
//// mb = &b
//// *p = mb // l1
//// q = *p  // l2
//// mc = &c
//// *p = mc // l3
//// r = *p  // l4
//// if(*) {
//  // p2 = &d
//  // mf = &f
//  // *p2 = mf  // l5
//// }
//// p3 = phi(p, p2)
//// s = *p3 // l6
//// me = &e
//// *p3 = me // l7
//// t = *p3  // l8
//
//// p = &a
AddrOf("p","a").
//// mb = &b
AddrOf("mb","b").
//// *p = mb // l1
Store("l1","p","mb").
//// q = *p  // l2
Load("l2","q","p").
//// mc = &c
AddrOf("mc","c").
//// *p = mc // l3
Store("l3","p","mc").
//// r = *p  // l4
Load("l4","r","p").
//// if(*) {
//  // p2 = &d
AddrOf("p2","d").
//  // mf = &f
AddrOf("mf","f").
//  // *p2 = mf  // l5
Store("l5","p2","mf").
//// }
//// p3 = phi(p, p2).
Copy("p3","p").
Copy("p3","p2").
//// s = *p3 // l6
Load("l6","s","p3").
//// me = &e
AddrOf("me","e").
//// *p3 = me // l7
Store("l7","p3","me").
//// t = *p3  // l8
Load("l8","t","p3").
//
CFG("l1","l2").
CFG("l2","l3").
CFG("l3","l4").
CFG("l4","l5").
CFG("l4","l6").
CFG("l5","l6").
CFG("l6","l7").
CFG("l7","l8").
//

print Pt.
print PtSU.
print PtH.
print KillNot.
print KillEmpty.
print SUBefore.
print SUAfter.
}

