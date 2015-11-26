
//// Inputs
//// =========
  rel AddrOf(variable: Str, object: Str)
  rel Copy(to: Str, from: Str)
  rel Store(label: Str, to: Str, from: Str)
  rel Load(label: Str, to: Str, from: Str)
  rel CFG(label: Str, to: Str)
  rel Multi(object: Str)
  rel Phi(label: Str)
  rel Clear(label: Str)
  rel FIStore(to: Str, from: Str, unknown: Str)
  rel FILoad(to: Str, from: Str, unknown: Str)

//// Outputs
//// =========
  rel Pt(variable: Str, target: Str)
  rel PtSU(location: Str, object: Str, target: Str)

//// Indexes
//// =========
  index AddrOf({variable, object});
  index Copy({to, from}, {from});
  index Store({label, to, from}, {to});
  index Load({label, from}, {from});
  index CFG({label, to});
  index FIStore({to}, {from});

  index StoresToP({l, p}, {p});
  index MayWrite({c, l});
  index Weak({a, l}, {a});
  index IsTop({a, l}, {a});

  index HeapPointsTo({b, a}, {b});
  index StoreVar({p, a, l}, {p, a}, {a, l}, {p}, {a});
  index LoadSet({a, l});


rel StoresToP(l: Str, p: Str)
rel MayWrite(c: Str, l: Str)
rel Weak(a: Str, l: Str)
rel IsTop(a: Str, l: Str)

// pt(p) contains a
rel VarPointsTo(a: Str, p: Str)

// pt[l](a) contains b
rel LabelPointsTo(b: Str, a: Str, l: Str)

// pt[*](a) contains b
rel HeapPointsTo(b: Str, a: Str)
rel PtH(a: Str, b: Str)


// pt(p) subseteq pt[l](a)
rel StoreVar(p: Str, a: Str, l: Str)

rel MultiVar(p: Str, a: Str, l: Str)
rel LoadSet(b: Str, a: Str, l: Str)
rel Topped(a: Str, l: Str)

StoresToP(l, p) :- Store(l, p, _).

VarPointsTo(a, p) :- AddrOf(p, a).
VarPointsTo(a, p) :- VarPointsTo(a, q), Copy(p, q).
VarPointsTo(b, p) :- StoreVar(r, a, l), VarPointsTo(a, q), Load(l, p, q), VarPointsTo(b, r).
VarPointsTo(b, p) :- IsTop(a, l), HeapPointsTo(a, b), VarPointsTo(a, q), Load(l, p, q).

MultiVar(r, a, l) :-
    VarPointsTo(a, q),
    Load(l, _, q),
    StoreVar(r, a, l),
    StoreVar(s, a, l),
    r != s.

LoadSet(b, a, l) :-
    MultiVar(r, a, l),
    VarPointsTo(b, r).
    
/*
Topped(a, l) :-
    LoadSet(b, a, l),
    Multi(b).
    */

Topped(a, l) :-
    LoadSet(b, a, l),
    LoadSet(c, a, l),
    b != c.

VarPointsTo(d, p) :-
    Topped(a, l),
    Load(l, p, q),
    VarPointsTo(a, q),
    HeapPointsTo(a, d).

VarPointsTo(b, p) :- StoreVar(r, a, l), VarPointsTo(a, q), Load(l, p, q), VarPointsTo(b, r).

StoreVar(q, a, l) :- VarPointsTo(a, p), Store(l, p, q).

StoresToP(l, p) :- Store(l, p, _).

MayWrite(c, l) :- VarPointsTo(c, p), StoresToP(l, p).
Weak(a, l) :- Multi(a), MayWrite(a, l).
Weak(a, l) :- MayWrite(c, l), c != a, AddrOf(_, a).
StoreVar(p, a, l) :- Weak(a, l), StoreVar(p, a, lpred), CFG(lpred, l).
IsTop(a, l) :- Weak(a, l), IsTop(a, lpred), CFG(lpred, l).

StoreVar(p, a, l) :- Phi(l), StoreVar(p, a, lpred), CFG(lpred, l).
IsTop(a, l) :- Phi(l), IsTop(a, lpred), CFG(lpred, l).

IsTop(a, l) :- Clear(l), AddrOf(_, a).

HeapPointsTo(a, b) :- StoreVar(p, a, _), VarPointsTo(b, p).
HeapPointsTo(a, b) :- FIStore(p, q, _), VarPointsTo(a, p), VarPointsTo(b, q).
VarPointsTo(a, p) :- FILoad(p, q, _), VarPointsTo(b, q), HeapPointsTo(b, a).
