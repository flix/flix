/*
 * Copyright 2021 Stephen Tetley
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

pub class Traversable[t : Type -> Type] with Functor[t] {

    pub def traverse(f: a -> m[b] & ef, t: t[a]): m[t[b]] & ef with Applicative[m]

    pub def sequence(t: t[m[a]]): m[t[a]] with Applicative[m] =
        Traversable.traverse(identity, t)

    ///
    /// Traversing with the identity function wrapped into an applicative preserves the container `t`.
    ///
    law identity: forall(t: t[a], f: a -> m[a]) with Eq[m[t[a]]], Applicative[m] . Traversable.traverse(f, t) == Applicative.point(t)

    ///
    /// sequence identity.
    ///
    law identity: forall(t: t[a], f: a -> m[a]) with Eq[m[t[a]]], Applicative[m] . Traversable.sequence(Functor.map(f, t)) == Applicative.point(t)

}


namespace Traversable {

    pub def for(t: t[a], f: a -> m[b] & ef): m[t[b]] & ef with Applicative[m], Traversable[t] =
        Traversable.traverse(f, t)


    pub def mapAccumLeft(f: acc -> a -> (acc, b), start: acc, t: t[a]): (acc, t[b]) with Traversable[t] =
        runStateL(traverse(x1 -> StateL(flip(f, x1)), t), start)

    opaque type StateL[s, a] = s -> (s, a)

    instance Functor[StateL[s]] {
        pub def map(f: a -> b & ef, x: StateL[s, a]): StateL[s, b] & ef=
            let StateL(x1) = x;
            StateL(s ->
                let (s1, v1) = x1(s);
                (s1, f(v1) as & Pure)
            ) as & ef
    }

    instance Applicative[StateL[s]] {
        pub def point(x: a): StateL[s, a] = StateL(s -> (s, x))

        pub def ap(f: StateL[s, a -> b & ef], x: StateL[s, a]): StateL[s, b] & ef =
            let StateL(f1) = f;
            let StateL(x1) = x;
            StateL(s ->
                let (s1, f2) = f1(s);
                let (s2, v) = x1(s1);
                (s2, f2(v) as & Pure)
            ) as & ef
    }

    def runStateL(x: StateL[s, a], state: s): (s, a) =
        let StateL(x1) = x;
        x1(state)

    pub def mapAccumRight(f: acc -> a -> (acc, b), start: acc, t: t[a]): (acc, t[b]) with Traversable[t] =
        runStateR(traverse(x1 -> StateR(flip(f, x1)), t), start)

    opaque type StateR[s, a] = s -> (s, a)

    instance Functor[StateR[s]] {
        pub def map(f: a -> b & ef, x: StateR[s, a]): StateR[s, b] & ef =
            let StateR(x1) = x;
            StateR(s ->
                let (s1, v1) = x1(s);
                (s1, f(v1) as & Pure)
            ) as & ef
    }

    instance Applicative[StateR[s]] {
        pub def point(x: a): StateR[s, a] = StateR(s -> (s, x))

        pub def ap(f: StateR[s, a -> b & ef], x: StateR[s, a]): StateR[s, b] & ef=
            let StateR(f1) = f;
            let StateR(x1) = x;
            StateR(s ->
                /// Eval order is reverse of StateL...
                let (s1, v) = x1(s);
                let (s2, f2) = f1(s1);
                (s2, f2(v) as & Pure)
            ) as & ef
    }

    def runStateR(x: StateR[s, a], state: s): (s, a) =
        let StateR(x1) = x;
        x1(state)

}

