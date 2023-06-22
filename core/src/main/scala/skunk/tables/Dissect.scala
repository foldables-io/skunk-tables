/*
 * Copyright 2023 Foldables
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package skunk.tables

import scala.quoted.*

import org.typelevel.twiddles.Iso

import skunk.tables.internal.MacroDissect

/** 
  * A type class proving that `T` can be converted into a Tuple of elements,
  * each of which has `IsColumn[x]` instance
  *
  * It's very similar to `Iso`, but unlike `Iso`:
  * 1. It's recursive, while `Iso` is always single level
  * 2. Respects `IsColumn` as a base case
  */
trait Dissect[T]:
  self =>

  type Out <: Tuple

  type Twiddled <: Tuple

  def to(t: T): Out

  def from(out: Out): T

  def untwiddle(t: Twiddled): Out

  def twiddle(t: Out): Twiddled


  def iso: Iso[T, Out] =
    new Iso[T, Out]:
      def to(t: T): Out = self.to(t)
      def from(o: Out): T = self.from(o)


object Dissect:

  type Aux[T, O] = Dissect[T] { type Out = O }
  
  type AuxT[T, O, OO] = Dissect[T] { type Out = O; type Twiddled = OO }

  transparent inline given [T]: Dissect[T] = build[T]


  transparent inline def build[T] =
    ${ buildImpl[T] }

  def buildImpl[T: Type](using Quotes) =
    import quotes.reflect.*

    val macroDissect = MacroDissect.build[T]

    (macroDissect.outType, macroDissect.twiddled.asType) match
      case ('[outType], '[twiddledType]) =>

        val macroTo =
          macroDissect.destruct.asExprOf[T => Any]   // outType doesn't work :(
        val macroFrom =
          macroDissect.construct.asExprOf[outType => Any]

        val twiddleTo =
          macroDissect.twiddle.asExpr
        val twiddleFrom =
          macroDissect.untwiddle.asExpr

        '{ (new Dissect[T] { self =>
          def to(t: T): self.Out =
            ${ macroTo }.apply(t).asInstanceOf[self.Out]
          def from(t: self.Out): T =
            ${ macroFrom }.apply(t.asInstanceOf[outType]).asInstanceOf[T]
          def twiddle(t: self.Out): self.Twiddled =
            ${ twiddleTo }.asInstanceOf[self.Out => self.Twiddled].apply(t)
          def untwiddle(t: self.Twiddled): self.Out =
            ${ twiddleFrom }.asInstanceOf[self.Twiddled => self.Out].apply(t)

        }).asInstanceOf[Dissect[T] { type Out = outType; type Twiddled = twiddledType }] }

