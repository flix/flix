/*
 * Copyright 2017 Magnus Madsen
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

package ca.uwaterloo.flix.util.terminal

import ca.uwaterloo.flix.util.terminal.Color.Txt


object ColorString {

  implicit def string2color(s: String): Color = Txt(s)

  implicit class Interpolator(val sc: StringContext) extends AnyVal {

    def color(args: Color*): ColorString = {

      // TODO:

      for (i <- 0 until sc.parts.length - 1) {
        print(sc.parts(i))
        print(args(i))
      }
      println(sc.parts.last)

      new ColorString(args.toList)
    }
  }

}

/**
  * A color string is simply a list of colored text fragments.
  */
class ColorString(xs: List[Color]) {

  /**
    * Formats `this` color string using the implicitly given color context.
    */
  def fmt(implicit ctx: ColorContext): String = xs.map(e => e.fmt).mkString("")

}
