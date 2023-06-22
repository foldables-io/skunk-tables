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

import java.time.LocalDateTime

import scala.quoted.*

import _root_.io.github.iltotore.iron.IronType

import skunk.{Fragment, Void, Encoder, ~}
import skunk.implicits.*


/**
  * TypedColumn contains compile-time information about columns,
  * such as name, original table, Scala type, associated `IsColumn` type class instance
  * (which is Posgres type) and set of Postgres constraints
  *
  * Both run-time values `name` and `isColumn` can be materialized from type info
  */
final case class TypedColumn[N <: Singleton, A, T, C <: Tuple](name: N, primitive: IsColumn[A]):

  val n: String = name.toString

  infix def ==(c: A): TypedColumn.Op[A] =
    TypedColumn.Op(sql"#$n = ${primitive.codec}", c)

  inline transparent def from[From](get: From => A): TypedColumn.In[N, From, A] =
    TypedColumn.In(get, primitive)

  object low:
    def name: Fragment[Void] =
      sql"#$n"

    /** `column_name = ?` */
    def eql: Fragment[A] =
      sql"$name = ${primitive.codec}"

    /** `p.column_name = ?` */
    def eqlAs(prefix: String): Fragment[A] =
      sql"#$prefix.$name = ${primitive.codec}"

    def currentTimestamp(using A =:= Option[LocalDateTime]): Fragment[Void] =
      sql"$name = current_timestamp"

    def isTrue(using A =:= Boolean): Fragment[Void] =
      sql"$name = true"
    def isFalse(using A =:= Boolean): Fragment[Void] =
      sql"$name = false"

    def increment[C](using A =:= IronType[Int, C]): Fragment[Void] =
      sql"$name = $name + 1"



object TypedColumn:

  /** Postgres column constraints */
  enum Constraint:
    case Primary
    case Default
    case Unique
    case Nullable

  /** Query operation on `TypedColumn` */
  final case class Op[A](fragment: Fragment[A], a: A):
    /** SQL `AND` operator */
    infix def and[B](other: Op[B]): Op[A ~ B] =
      Op((fragment <~ const" AND ").product(other.fragment), (a, other.a))
    /** SQL `OR` operator */
    infix def or[B](other: Op[B]): Op[A ~ B] =
      Op((fragment <~ const" OR ").product(other.fragment), (a, other.a))

  object Op:
    def void: Op[Void] =
      Op(Fragment.empty, Void)

  final case class In[N <: Singleton, A, B](get: A => B, primitive: IsColumn[B]):
    def use(a: A): B = get(a)

