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

pub enum Identity[a] with Eq, Order, ToString, Hash, Boxable { case Identity(a) }

instance SemiGroup[Identity[a]] with SemiGroup[a] {
    pub def combine(x: Identity[a], y: Identity[a]): Identity[a] =
        let Identity(x1) = x;
        let Identity(y1) = y;
        Identity(SemiGroup.combine(x1, y1))
}

instance Monoid[Identity[a]] with Monoid[a] {
    pub def empty(): Identity[a] = Identity(Monoid.empty())
}

instance Functor[Identity] {
    pub def map(f: a -> b & ef, x: Identity[a]): Identity[b] & ef =
        let Identity(x1) = x;
        Identity(f(x1))
}

instance Applicative[Identity] {
    pub def point(x: a): Identity[a] =
        Identity(x)

    pub def ap(f: Identity[a -> b & ef], x: Identity[a]): Identity[b] & ef =
        let Identity(f1) = f;
        let Identity(x1) = x;
        Identity(f1(x1))
}

instance Monad[Identity] {
    pub def flatMap(f: a -> Identity[b] & ef, x: Identity[a]): Identity[b] & ef =
        let Identity(x1) = x;
        f(x1)
}

instance ForEach[Identity] {
    pub def foreach(f: a -> Unit & ef, x: Identity[a]): Unit & ef =
        let Identity(x1) = x;
        f(x1)
}

instance Foldable[Identity] {
    pub def foldLeft(f: (b, a) -> b & ef, s: b, x: Identity[a]): b & ef =
        let Identity(x1) = x;
        f(s, x1)

    pub def foldRight(f: (a, b) -> b & ef, s: b, x: Identity[a]): b & ef =
        let Identity(x1) = x;
        f(x1, s)

    pub def foldRightWithCont(f: (a, Unit -> b & ef) -> b & ef, s: b, x: Identity[a]): b & ef =
        let Identity(x1) = x;
        f(x1, _ -> s as & ef)
}

instance UnorderedFoldable[Identity] {
    pub def foldMap(f: a -> b & ef, x: Identity[a]): b & ef with CommutativeMonoid[b] =
        let Identity(x1) = x;
        f(x1)
}

instance Traversable[Identity] {
    pub def traverse(f: a -> m[b] & ef, x: Identity[a]): m[Identity[b]] & ef with Applicative[m] =
        let Identity(x1) = x;
        Functor.map(Identity, f(x1))

    pub override def sequence(x: Identity[m[a]]): m[Identity[a]] with Applicative[m] =
        let Identity(x1) = x;
        Functor.map(Identity, x1)
}

instance Add[Identity[a]] with Add[a] {
    pub def add(x: Identity[a], y: Identity[a]): Identity[a] =
        let Identity(x1) = x;
        let Identity(y1) = y;
        Identity(Add.add(x1, y1))
}

instance Sub[Identity[a]] with Sub[a]  {
    pub def sub(x: Identity[a], y: Identity[a]): Identity[a] =
        let Identity(x1) = x;
        let Identity(y1) = y;
        Identity(Sub.sub(x1, y1))
}

instance Mul[Identity[a]] with Mul[a]  {
    pub def mul(x: Identity[a], y: Identity[a]): Identity[a] =
        let Identity(x1) = x;
        let Identity(y1) = y;
        Identity(Mul.mul(x1, y1))
}

instance Div[Identity[a]] with Div[a]  {
    pub def div(x: Identity[a], y: Identity[a]): Identity[a] =
        let Identity(x1) = x;
        let Identity(y1) = y;
        Identity(Div.div(x1, y1))
}

instance Neg[Identity[a]] with Neg[a] {
    pub def neg(x: Identity[a]): Identity[a] = Functor.map(Neg.neg, x)
}
