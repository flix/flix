/*
 *  Copyright 2022 Stephen Tetley
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

///
/// The Identity Functor / Monad.
///

pub enum Identity[a] with Eq, Order, ToString, Hash, Sendable { case Identity(a) }

instance SemiGroup[Identity[a]] with SemiGroup[a] {
    pub def combine(x: Identity[a], y: Identity[a]): Identity[a] =
        let Identity.Identity(x1) = x;
        let Identity.Identity(y1) = y;
        Identity.Identity(SemiGroup.combine(x1, y1))
}

instance Monoid[Identity[a]] with Monoid[a] {
    pub def empty(): Identity[a] = Identity.Identity(Monoid.empty())
}

instance Functor[Identity] {
    pub def map(f: a -> b \ ef, x: Identity[a]): Identity[b] \ ef =
        let Identity.Identity(x1) = x;
        Identity.Identity(f(x1))
}

instance Applicative[Identity] {
    pub def point(x: a): Identity[a] =
        Identity.Identity(x)

    pub def ap(f: Identity[a -> b \ ef], x: Identity[a]): Identity[b] \ ef =
        let Identity.Identity(f1) = f;
        let Identity.Identity(x1) = x;
        Identity.Identity(f1(x1))
}

instance Monad[Identity] {
    pub def flatMap(f: a -> Identity[b] \ ef, x: Identity[a]): Identity[b] \ ef =
        let Identity.Identity(x1) = x;
        f(x1)
}

instance MonadZip[Identity] {
    pub def zipWith(f: (a, b) -> c \ ef, ma: Identity[a], mb: Identity[b]): Identity[c] \ ef =
        let Identity.Identity(a) = ma;
        let Identity.Identity(b) = mb;
        Identity.Identity(f(a, b))

    redef unzip(ma: Identity[(a, b)]): (Identity[a], Identity[b]) =
        let Identity.Identity((a, b)) = ma;
        (Identity.Identity(a), Identity.Identity(b))

    pub def zipWithA(f: (a, b) -> f[c] \ ef, x: Identity[a], y: Identity[b]): f[Identity[c]] \ ef with Applicative[f] =
        use Functor.{<$>};
        let Identity.Identity(x1) = x;
        let Identity.Identity(y1) = y;
        Identity.Identity <$> f(x1, y1)
}

instance Foldable[Identity] {
    pub def foldLeft(f: (b, a) -> b \ ef, s: b, x: Identity[a]): b \ ef =
        let Identity.Identity(x1) = x;
        f(s, x1)

    pub def foldRight(f: (a, b) -> b \ ef, s: b, x: Identity[a]): b \ ef =
        let Identity.Identity(x1) = x;
        f(x1, s)

    pub def foldRightWithCont(f: (a, Unit -> b \ ef) -> b \ ef, s: b, x: Identity[a]): b \ ef =
        let Identity.Identity(x1) = x;
        f(x1, constant(s))
}

instance UnorderedFoldable[Identity] {
    pub def foldMap(f: a -> b \ ef, x: Identity[a]): b \ ef with CommutativeMonoid[b] =
        let Identity.Identity(x1) = x;
        f(x1)
}

instance Traversable[Identity] {
    pub def traverse(f: a -> m[b] \ ef, x: Identity[a]): m[Identity[b]] \ ef with Applicative[m] =
        let Identity.Identity(x1) = x;
        Functor.map(Identity.Identity, f(x1))

    redef sequence(x: Identity[m[a]]): m[Identity[a]] with Applicative[m] =
        let Identity.Identity(x1) = x;
        Functor.map(Identity.Identity, x1)
}

instance Add[Identity[a]] with Add[a] {
    pub def add(x: Identity[a], y: Identity[a]): Identity[a] =
        let Identity.Identity(x1) = x;
        let Identity.Identity(y1) = y;
        Identity.Identity(Add.add(x1, y1))
}

instance Sub[Identity[a]] with Sub[a]  {
    pub def sub(x: Identity[a], y: Identity[a]): Identity[a] =
        let Identity.Identity(x1) = x;
        let Identity.Identity(y1) = y;
        Identity.Identity(Sub.sub(x1, y1))
}

instance Mul[Identity[a]] with Mul[a]  {
    pub def mul(x: Identity[a], y: Identity[a]): Identity[a] =
        let Identity.Identity(x1) = x;
        let Identity.Identity(y1) = y;
        Identity.Identity(Mul.mul(x1, y1))
}

instance Div[Identity[a]] with Div[a]  {
    pub def div(x: Identity[a], y: Identity[a]): Identity[a] =
        let Identity.Identity(x1) = x;
        let Identity.Identity(y1) = y;
        Identity.Identity(Div.div(x1, y1))
}

instance Neg[Identity[a]] with Neg[a] {
    pub def neg(x: Identity[a]): Identity[a] = Functor.map(Neg.neg, x)
}
