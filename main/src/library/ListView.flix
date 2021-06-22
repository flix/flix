/*
 * Copyright 2019 Liam Palmer, Magnus Madsen
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

///
/// The ListView type.
///
pub enum ListView[t, a] {
		case VNil,
		case VCons(a, ListView[t, a]) // TODO: Seems this should just be a list....
		case LVFilter(a -> Bool, ListView[t, a])
}

namespace ListView {

		///
		/// Returns true if and only if `l` is the empty list view.
		///
		/// Forces evaluation of the view.
		///
		pub def isEmpty(l: ListView[_, a]): Bool = match l {
			case VNil => true
			case _    => false
		}

		///
		/// Returns `Some(x)` if `x` is the first element of `l`.
		///
		/// Returns `None` if `l` is empty.
		///
		/// Forces evaluation of the view `l`.
		///
		pub def head(l: ListView[_, a]): Option[a] = match l {
			case VNil 			 => None
			case VCons(x, _) 	 => Some(x)
			case LVFilter(f, xs) => ??? // TODO: How?
		}

		pub def headWithFilter(p: a -> Bool, l: ListView[_, a]): Option[a] = match l {
			case VNil 			 => None
			case VCons(x, xs) 	 => if (p(x)) Some(x) else headWithFilter(p, xs)
			case LVFilter(g, xs) => headWithFilter(x -> g(x) and p(x), xs)
		}

		///
		/// Returns a list view of every element in `l` that satisfies the predicate `p`.
		///
		/// Forces evaluation of the view if `p` is impure.
		///
		pub def filter(p: a -> Bool & ef, l: ListView[t, a]): ListView[t, a] & ef = ???

		// TODO: DOC
		pub def filterL(p: a -> Bool, l: ListView[t, a]): ListView[t, a] = match l {
			case VNil 				=> VNil
			case VCons(_, _)		=> LVFilter(p, l)
			case LVFilter(g, xs) 	=> LVFilter(x -> g(x) and p(x), xs)
		}

		pub def filterE(p: a -> Bool & ef, l: ListView[t, a]): ListView[t, a] & ef = match l {
			case VNil 				=> VNil
			case VCons(x, xs)		=> if (p(x)) VCons(x, filterE(p, xs)) else filterE(p, xs)
			case LVFilter(g, xs) 	=> filterE(x -> g(x) and p(x), xs)
		}

		///
		/// Returns the number of elements in `l` that satisfy the predicate `p`.
		///
		pub def count(p: a -> Bool & ef, l: ListView[_, a]): Int32 & ef = match l {
			case VNil 		  		=> 0
			case VCons(x, xs) 		=> (if (p(x)) 1 else 0) + count(p, xs)
			case LVFilter(f, xs) 	=> count(x -> f(x) and p(x), xs)
		}

		///
		/// Returns the result of applying `f` to every element in `l`.
		///
		/// That is, the result is of the form: `f(x1) :: f(x2) :: ...`.
		///
		pub def map(f: a -> b & ef, l: ListView[t, a]): ListView[t, b] = ???


		pub def mapWithFilterE(f1: a -> Bool, f: a -> b & ef, l: ListView[t, a]): ListView[t, b] & ef = match l {
			case VNil 		  		=> VNil
			case VCons(x, xs) 		=>
				if (f1(x))
					VCons(f(x), mapWithFilterE(f1, f, xs))
				else
					mapWithFilterE(f1, f, xs)
			case LVFilter(f2, xs) 	=> mapWithFilterE(x -> f1(x) and f2(x), f, xs)
		}

		// TODO: Compose filters.
		pub def andF(f: a -> Bool, g: a -> Bool): a -> Bool = x -> f(x) and g(x)

		///
		/// Returns `l` without the first `n` elements.
		///
		/// Returns `VNil` if `n > length(xs)`. Returns `l` if `n < 0`.
		///
		/// Does not force evaluation of `l`.
		///
		pub def drop(n: Int32, l: ListView[t, a]): ListView[t, a] = dropWithFilter(constant(true), n, l)

		pub def dropWithFilter(p: a -> Bool, n: Int32, l: ListView[t, a]): ListView[t, a] = match (n, l) {
			case (_, VNil) 				=> VNil
			case (0, _) 				=> l
			case (i, VCons(x, xs)) 		=> if (p(x)) dropWithFilter(p, i - 1, xs) else dropWithFilter(p, i, xs)
			case (i, LVFilter(f, xs)) 	=> dropWithFilter(x -> f(x) and p(x), i, xs)
		}

		pub def ispure(_f: a -> b & eff): Bool = true

}

