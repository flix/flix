Z 64

Load(x: Z, y: Z, z: Z) input
Store(x: Z, y: Z, z: Z) input
FILoad(x: Z, y: Z, z: Z) input
FIStore(x: Z, y: Z, z: Z) input
AddrOf(x: Z, y: Z) input
Copy(x: Z, y: Z) input
Multi(x: Z) input
CFG(x: Z, y: Z) input
Phi(x: Z) input
Clear(x: Z) input

StoresToP(x: Z, y: Z)
VarPointsTo(x: Z, y: Z)
LabelPointsTo(x: Z, y: Z, z: Z)
HeapPointsTo(x: Z, y: Z)
IsTop(x: Z, y: Z)
MayWrite(x: Z, y: Z)
Weak(x: Z, y: Z)

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

IsTop(a, l) :- LabelPointsTo(c, a, l), LabelPointsTo(d, a, l), c != d.
