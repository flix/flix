/*
 * Copyright 2015-2016 Magnus Madsen, Ming-Ho Yee
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

package ca.uwaterloo.flix.api

import ca.uwaterloo.flix.runtime.Model

import scala.collection.JavaConverters._

final class WrappedModel(val model: Model) extends IModel {

  def getConstant(name: String): IValue = ??? // TODO

  def getRelation(name: String): java.lang.Iterable[Array[IValue]] = {
    model.getRelationOpt(name) match {
      case None => throw new IllegalArgumentException(s"Unknown relation: '$name'.")
      case Some(iterable) => iterable.map {
        case row => row.map(v => new WrappedValue(v): IValue).toArray
      }.asJava
    }
  }

  def getLattice(name: String): java.lang.Iterable[Array[IValue]] = {
    model.getLatticeOpt(name) match {
      case None => throw new IllegalArgumentException(s"Unknown relation: '$name'.")
      case Some(iterable) => iterable.map {
        case (keys, elm) => (keys :: elm :: Nil).map(e => new WrappedValue(e): IValue).toArray
      }.asJava
    }
  }

}
