
// Inputs
// =========
  rel AddrOf(variable: String, object: String)
  rel Copy(to: String, from: String)
  rel Store(label: String, to: String, from: String)
  rel Load(label: String, to: String, from: String)
  rel CFG(label: String, to: String)
  rel Multi(object: String)
  rel Phi(label: String)
  rel Clear(label: String)
  rel FIStore(to: String, from: String, unknown: String)
  rel FILoad(to: String, from: String, unknown: String)

// Outputs
// =========
  rel Pt(variable: String, target: String)

// Other types
// =========
  enum SULattice {
    case Top, // kill nothing
    case Single(String),
    case Bottom // kill everything
  }

  def equ(e1: SULattice, e2: SULattice): Bool = e1 == e2

  def leq(e1: SULattice, e2: SULattice): Bool = match (e1, e2) {
    case (SULattice.Bottom, _) => true
    case (_, SULattice.Top) => true
    case (SULattice.Single(s1), SULattice.Single(s2)) => s1 == s2
    case _ => false
  }
  def lub(e1: SULattice, e2: SULattice): SULattice = match (e1, e2) {
    case (SULattice.Bottom, _) => e2
    case (_, SULattice.Bottom) => e1
    case (SULattice.Single(s1), SULattice.Single(s2)) => if (s1 == s2) SULattice.Single(s1) else SULattice.Top
    case _ => SULattice.Top
  }
  def glb(e1: SULattice, e2: SULattice): SULattice = match (e1, e2) {
    case (SULattice.Top, _) => e2
    case (_, SULattice.Top) => e1
    case (SULattice.Single(s1), SULattice.Single(s2)) => if (s1 == s2) SULattice.Single(s1) else SULattice.Bottom
    case _ => SULattice.Bottom
  }
  let SULattice<> = (SULattice.Bottom, SULattice.Top, equ, leq, lub, glb)

  instance LowerBound[SULattice] {
    def minValue(): SULattice = SULattice.Bottom
  }

  lat SU(location: String, object: String, target: SULattice)

  rel PtH(object: String, target: String)
  lat Kill(location: String, kill: SULattice)

// Rules
// =========
//
// AddrOf
// ---------
Pt(p,a) :- AddrOf(p,a).
//
// Copy
// ---------
Pt(p,a) :- Copy(p,q), Pt(q,a).
//
// Store
// ---------
def f(x: String): SULattice = SULattice.Single(x)
SU(l,a;f(b)) :- Store(l,p,q), Pt(p,a), Pt(q,b).
//
PtH(a,b) :- Store(l,p,q), Pt(p,a), Pt(q,b).
PtH(a,b) :- FIStore(p,q,_), Pt(p,a), Pt(q,b).
//
// Load
// ---------
Pt(p,b) :- Load(l,p,q), Pt(q,a), filter(t, b), PtH(a,b), SU(l,a;t).
Pt(p,b) :- FILoad(p,q,_), Pt(q,a), PtH(a,b).
//
// CFlow
// ---------
//SU(l2,a,t) :- CFG(l1,l2), SU(l1,a,t).
//
// Preserve
// ---------
//su-after(l,a,t) :- su-before(l,a,t), NOT kill(l,a).
def killNot(a: String, e: SULattice): Bool = match e {
  case SULattice.Bottom => false
  case SULattice.Single(s) => a != s
  case SULattice.Top => true
}
SU(l2,a;t) :- CFG(l1, l2), SU(l1,a;t), Multi(a).
SU(l2,a;t) :- CFG(l1, l2), SU(l1,a;t), killNot(a, k), Kill(l2;k).
//SU(l2,a,t) :- CFG(l1, l2), SU(l1,a,t), Phi(l2).
//
// PtSu
// ---------
def filter(e: SULattice, p: String): Bool = match e {
  case SULattice.Bottom => false
  case SULattice.Single(s) => p == s
  case SULattice.Top => true
}
SU(l,a;f(b)) :- Clear(l), PtH(a,b).

// Kill
// ---------
Kill(l;f(b)) :- Store(l,p,q), Pt(p,b).
Kill(l;Top) :- Phi(l).
