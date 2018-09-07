rel Parent(p1: Str, p2: Str)
rel GrandParent(p1: Str, p2: Str)
rel Ancestor(p1: Str, p2: Str)

Parent("Magnus", "Inger").
Parent("Magnus", "Frits").
Parent("Bjarke", "Inger").
Parent("Bjarke", "Frits").
Parent("Frits", "Orla").

GrandParent(x, z) :- Parent(x, y), Parent(y, z).
Ancestor(x, z) :- Ancestor(x, y), Parent(y, z).

