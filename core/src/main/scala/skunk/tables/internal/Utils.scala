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

package skunk.tables.internal

import scala.quoted.*

object Utils:
  /** Get arity of tuple */
  def getArity(using q: Quotes)(tuple: q.reflect.TypeRepr): Int =
    import quotes.reflect.*

    tuple match
      case AppliedType(TypeRef(_, t), _) =>
        t match
          case "class EmptyTuple" => 0
          case _                  => t.dropWhile(c => !c.isDigit).toInt

  /** Transform a `TypeRepr` of a single `Constraint` into `String` representation */
  def getConstraints[Q <: Quotes & Singleton](q: Q)(repr: q.reflect.TypeRepr): Option[String] =
    import q.reflect.*

    repr match
      case TypeRef(ThisType(TypeRef(ThisType(TypeRef(_, _)),_)), c) =>
        Some(c)
      case a =>
        None

  /** Get a (compile-time) type of `constraints` tuple and transform them into a run-time `String`
    */
  def materializeConstraints[Q <: Quotes & Singleton](q: Q)(repr: q.reflect.TypeRepr): List[String] =
    import q.reflect.*

    repr match
      case AppliedType(_, cts) =>
        cts.map(getConstraints(q)).map {
          case Some(c) => c
          case None    => report.errorAndAbort(s"Invalid State: materializeConstraints got invalid constraint with $cts")
        }
      case TermRef(_, _) =>
        Nil // EmptyTuple
      case TypeRef(_, _) =>
        Nil


  /** Append an element to tuple on `TypeRepr`-level */
  def appendTuple
    (using quotes: Quotes)
    (tup: quotes.reflect.TypeRepr, toAdd: quotes.reflect.TypeRepr)
    : quotes.reflect.TypeRepr =
    import quotes.reflect.*

    tup match
      case TypeRef(TermRef(_, _), "Nothing") =>
        report.errorAndAbort("Your TypedColumn is missing constraints type parameter")
      case TypeRef(TermRef(_, _), "EmptyTuple") =>
        val arity = 1
        val tuple = Symbol.requiredClass(s"scala.Tuple${arity}").typeRef
        AppliedType(tuple, toAdd :: Nil)
      case TermRef(TermRef(_, _), "EmptyTuple") =>
        val arity = 1
        val tuple = Symbol.requiredClass(s"scala.Tuple${arity}").typeRef
        AppliedType(tuple, toAdd :: Nil)
      case AppliedType(TypeRef(thisType, name), types) =>
        val arity = name.drop("Tuple".length).toInt + 1
        val tuple = Symbol.requiredClass(s"scala.Tuple${arity}").typeRef
        AppliedType(tuple, toAdd :: types)
