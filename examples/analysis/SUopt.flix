  enum SULattice {
    case Top, // kill nothing
    case Single(String),
    case Bottom // kill everything
  }

  instance LowerBound[SULattice] {
    def minValue(): SULattice = SULattice.Bottom
  }

  instance PreOrder[SULattice] {
    def equ(x: SULattice, y: SULattice): Bool = match (x, y) {
        case (Top, Top) => true
        case (Single(a), Single(b)) => a == b
        case (Bottom, Bottom) => true
        case _ => false
    }
  }

  instance PartialOrder[SULattice] {
    def partialCompare(x: SULattice, y: SULattice): Bool = match (x, y) {
        case (SULattice.Bottom, _) => true
        case (_, SULattice.Top) => true
        case (SULattice.Single(s1), SULattice.Single(s2)) => s1 == s2
        case _ => false
    }
  }

  instance JoinLattice[SULattice] {
    def leastUpperBound(x: SULattice, y: SULattice): SULattice = match (x, y) {
        case (SULattice.Bottom, _)                          => y
        case (_, SULattice.Bottom)                          => x
        case (SULattice.Single(s1), SULattice.Single(s2))   => if (s1 == s2) SULattice.Single(s1) else SULattice.Top
        case _                                              => SULattice.Top
    }
  }

  instance MeetLattice[SULattice] {
    def greatestLowerBound(x: SULattice, y: SULattice): SULattice = match (x, y) {
        case (SULattice.Top, _) => y
        case (_, SULattice.Top) => x
        case (SULattice.Single(s1), SULattice.Single(s2)) => if (s1 == s2) SULattice.Single(s1) else SULattice.Bottom
        case _ => SULattice.Bottom
    }
  }

  instance ToString[SULattice] {
    def toString(x: SULattice): String = match x {
        case Top        => "Top"
        case Single(n)  => "Single(${n})"
        case Bottom     => "Bottom"
      }
  }

def f(x: String): SULattice = SULattice.Single(x)

def killNot(a: String, e: SULattice): Bool = match e {
  case SULattice.Bottom => false
  case SULattice.Single(s) => a != s
  case SULattice.Top => true
}

def filter(e: SULattice, p: String): Bool = match e {
  case SULattice.Bottom => false
  case SULattice.Single(s) => p == s
  case SULattice.Top => true
}

def main(): #{ Pt(String, String) } = project Pt (solve #{
    // Rules
    // =========
    //
    // AddrOf
    // ---------
    Pt(p: String,a: String) :- AddrOf(p,a).
    //
    // Copy
    // ---------
    Pt(p,a) :- Copy(p,q), Pt(q,a).
    //
    // Store
    // ---------
    SU(l: String, a: String; f(b)) :- Store(l,p,q), Pt(p,a), Pt(q,b).
    //
    PtH(a: String,b: String) :- Store(l,p,q), Pt(p,a), Pt(q,b).
    PtH(a,b) :- FIStore(p,q), Pt(p,a), Pt(q,b).
    //
    // Load
    // ---------
    Pt(p,b) :- Load(l,p,q), Pt(q,a), filter(t, b), PtH(a,b), SU(l,a;t).
    Pt(p,b) :- FILoad(p,q), Pt(q,a), PtH(a,b).
    //
    // CFlow
    // ---------
    //SU(l2,a,t) :- CFG(l1,l2), SU(l1,a,t).
    //
    // Preserve
    // ---------
    //su-after(l,a,t) :- su-before(l,a,t), NOT kill(l,a).

    SU(l2,a;t) :- CFG(l1, l2), SU(l1,a;t), Multi(a).
    SU(l2,a;t) :- CFG(l1, l2), SU(l1,a;t), killNot(a, k), Kill(l2;k).
    //SU(l2,a,t) :- CFG(l1, l2), SU(l1,a,t), Phi(l2).
    //
    // PtSu
    // ---------
    SU(l,a;f(b)) :- Clear(l), PtH(a,b).

    // Kill
    // ---------
    Kill(l: String; f(b)) :- Store(l,p,q), Pt(p,b).
    Kill(l;Top) :- Phi(l).
})

