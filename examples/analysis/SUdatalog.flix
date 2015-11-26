
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
  index Copy({from});
  index Store({to, from});
  index Load({label, from});
  index CFG({label, to});
  index FIStore({to}, {from});

  index StoresToP({l}, {p});
  index MayWrite({c, l}, {c});
  index Weak({a, l}, {a});
  index IsTop({a, l}, {a});

  index VarPointsTo({a, p}, {a}, {p});
  index LabelPointsTo({b, a, l}, {a, l}, {a}, {l});
  index HeapPointsTo({b, a}, {a});


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

StoresToP(l, p) :- Store(l, p, _).

VarPointsTo(a, p) :- AddrOf(p, a).
VarPointsTo(a, p) :- VarPointsTo(a, q), Copy(p, q).
VarPointsTo(b, p) :- LabelPointsTo(b, a, l), VarPointsTo(a, q), Load(l, p, q).
VarPointsTo(b, p) :- IsTop(a, l), VarPointsTo(a, q), Load(l, p, q), HeapPointsTo(b, a).

LabelPointsTo(b, a, l) :- VarPointsTo(a, p), VarPointsTo(b, q), Store(l, p, q).

StoresToP(l, p) :- Store(l, p, _).

MayWrite(c, l) :- VarPointsTo(c, p), StoresToP(l, p).
Weak(a, l) :- Multi(a), MayWrite(a, l).
Weak(a, l) :- MayWrite(c, l), c != a, LabelPointsTo(_, a, lpred), CFG(lpred, l).
Weak(a, l) :- MayWrite(c, l), c != a, IsTop(a, lpred), CFG(lpred, l).
LabelPointsTo(b, a, l) :- Weak(a, l), LabelPointsTo(b, a, lpred), CFG(lpred, l).
IsTop(a, l) :- Weak(a, l), IsTop(a, lpred), CFG(lpred, l).


LabelPointsTo(b, a, l) :- Phi(l), LabelPointsTo(b, a, lpred), CFG(lpred, l).
IsTop(a, l) :- Phi(l), IsTop(a, lpred), CFG(lpred, l).

LabelPointsTo(b, a, l) :- Clear(l), HeapPointsTo(b, a).
IsTop(a, l) :- Clear(l), HeapPointsTo(_, a).

HeapPointsTo(b, a) :- LabelPointsTo(b, a, l), StoresToP(l, _).
HeapPointsTo(b, a) :- FIStore(p, q, _), VarPointsTo(a, p), VarPointsTo(b, q).
VarPointsTo(a, p) :- FILoad(p, q, _), VarPointsTo(b, q), HeapPointsTo(a, b).

IsTop(a, l) :- 
    LabelPointsTo(c, a, l),
    LabelPointsTo(d, a, l),
    c != d.

// Uncomment below for equivalent-to-flow-insensitive analysis
// LabelPointsTo(b, a, l) :- HeapPointsTo(b, a), Load(l, _, _).
