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

import scala.compiletime.{erasedValue, constValueTuple, summonInline}
import scala.quoted.*

import skunk.*
import skunk.Codec
import skunk.implicits.*

import skunk.tables.internal.{AllRequired, TwiddleIn, GetInNames}

/** `CanInsert` is an evidence that a value of type `A` can be inserted into
  * `Table[T]` In nutshell it's a mapping from members of `A` into columns of
  * `Table[T]`. `A` can have more members than `Table[T]` columns, but every
  * non-nullable, non-default member of `Table[T]` must have a counterpart in
  * `A`
  *
  * It exists to handle different scenarios, where table has `DEFAULT` or `NULL`
  * columns and we need to reliably map class members to their respective
  * columns. Each table can have multiple user-defined instances of `CanInsert`
  * for different classes
  */
trait CanInsert[A, T]:

  /** Tuple (a twiddled list to be precise) of elements `A` (not necessarily
    * isomorphic to `A` or `T`)
    */
  type Twiddled

  /** List of columns of `A` that this instance can fulfill. Used in `INSERT`
    * statement Order of columns does NOT represent order of members of `A`, nor
    * columns of `T` It matches to order of `Twiddled`, which is specified by
    * `via` contructor function
    */
  def columns: List[String]

  def columnsFragment: Fragment[Void] =
    sql"#${columns.mkString(", ")}"

  def encoder: Encoder[Twiddled]

  /** Every time we insert `a` into a table we need to transform it into a
    * twiddle list, ordered for `columns`
    */
  def transform(a: A): Twiddled

object CanInsert:

  /** Entry-point for `CanInsert` generation */
  inline transparent def apply[A]: CanInsertPartialInit[A] =
    new CanInsertPartialInit[A] {}

  trait CanInsertPartialInit[A]:
    inline transparent def into[T <: Product, S, C <: NonEmptyTuple](table: Table[T] {
      type Select = S; type TypedColumns = C
    }): CanInsertPartialFinal[A, T, S, C] =
      new CanInsertPartialFinal[A, T, S, C]:
        val tableOfT = table

        override def toString: String =
          s"CanInsertIntoPartialFinal($tableOfT)"

  trait CanInsertPartialFinal[A, T <: Product, S, C <: NonEmptyTuple]:

    val tableOfT: Table[T] {
      type Select       = S // Selectable
      type TypedColumns = C // Tuple of all Columns
    }

    /** Last stage of `CanInsert` generation Here we map members of `A` into
      * columns of `Table[T]`
      *
      * @warn
      *   for single-element mapping you should wrap it into `Tuple1.apply` and
      *   *not* use `x *: EmptyTuple` constructor
      */
    inline transparent def via[I <: NonEmptyTuple](map: S => I)(using AllRequired[C, I]): CanInsert[A, T] =
      new CanInsert[A, T]:
        type Twiddled =
          TwiddleIn[I]

        def columns: List[String] =
          constValueTuple[GetInNames[I]].toList.map(_.toString)

        def encoder: Codec[Twiddled] =
          CanInsert.getCodec[I]

        def transform(a: A): Twiddled =
          val mapping: I = map(tableOfT.select)
          buildTwiddled(mapping, a).asInstanceOf[Twiddled]

        override def toString: String =
          s"""CanInsert(${tableOfT.name}, [${columns.mkString(", ")}])"""

    // TODO: I need to implement an `auto[N]` constroctor that works if
    // `N`'s members are strict subset of `T` *and* covers all non-nullable,
    // non-default members of the table

  /** Summon all instances of `Codec` (via `IsColumn`) into a twiddled tuple
    * generated from `T`
    */
  inline def getCodec[I <: NonEmptyTuple]: Codec[TwiddleIn[I]] =
    val codec = inline erasedValue[I] match
      case _: (TypedColumn.In[?, ?, h1] *: EmptyTuple) =>
        summonInline[IsColumn[h1]].codec.imap(h1 => Tuple(h1))(_.head)
      case _: (TypedColumn.In[?, ?, h1] *: TypedColumn.In[?, ?, h2] *: t) =>
        getCodecGo[t, Codec[h1 ~ h2]](summonInline[IsColumn[h1]].codec ~ summonInline[IsColumn[h2]].codec)

    codec.asInstanceOf[Codec[TwiddleIn[I]]]

  private inline def getCodecGo[T <: Tuple, A](a: A): Any =
    inline erasedValue[T] match
      case _: EmptyTuple => a
      case _: (TypedColumn.In[?, ?, t] *: ts) =>
        getCodecGo[ts, Codec[A ~ t]](a.asInstanceOf[Codec[A]] ~ summonInline[IsColumn[t]].codec)

  /** Convert an object of `A` into a twiddle list, described by `C` Where `C`
    * is a tuple of `TypedColumn.In`, containing functions for destructuring `A`
    */
  transparent inline def buildTwiddled[C <: NonEmptyTuple, A](c: C, a: A): Any =
    ${ buildTwiddledImpl[C, A]('c, 'a) }
  private def buildTwiddledImpl[C <: NonEmptyTuple: Type, A: Type](using
      Quotes
  )(cExpr: Expr[C], aExpr: Expr[A]): Expr[Any] =
    import quotes.reflect.*

    TypeRepr.of[C] match
      case AppliedType(TypeRef(_, _), listOfIns) =>
        val inTypes = dropTupleNil(listOfIns)

        val parts = inTypes match
          case _ =>
            dropTupleNil(inTypes).zipWithIndex.map { (_, x) =>
              val in = Select.unique(cExpr.asTerm, s"_${x + 1}")
              Select.unique(in, "use").appliedTo(aExpr.asTerm).asExpr
            }

        val tuple = Expr.ofTupleFromSeq(parts)
        tuple.asTerm.tpe.asType match
          case '[tuple] =>
            Expr.summon[Dissect[tuple]] match
              case Some(dissect) =>
                // Everything below is called only when `ci.transform` was called
                '{
                  ${ dissect }
                    .asInstanceOf[Dissect.AuxT[tuple, Any, Any]]
                    .twiddle(${ dissect }
                      .to(${ tuple.asExprOf[tuple] }))
                }
              case None =>
                report.errorAndAbort(s"Couldn't summon given for `Dissect[${TypeRepr.of[tuple].show}]`")

  /** `Tuple[1]` is represented as `List(tpe, scala.Tuple$package.EmptyTuple)`,
    * so we drop tail
    */
  def dropTupleNil(using quotes: Quotes)(ins: List[quotes.reflect.TypeRepr]): List[quotes.reflect.TypeRepr] =
    ins match
      case List(h, t) if t.show.endsWith("EmptyTuple") => List(h)
      case _                                           => ins

  // Stub for later use, I want to constrain `TypedColumns`
  private object Later:
    case class MappedColumn[N <: Singleton, A, B](primitive: IsColumn[B]):
      def from(getter: A => B): TypedColumn.In[N, A, B] =
        TypedColumn.In[N, A, B](getter, primitive)

    object MappedColumn:
      def apply[A, N <: Singleton, B, T, C <: Tuple](column: TypedColumn[N, B, T, C]): MappedColumn[N, A, B] =
        MappedColumn[N, A, B](column.primitive)

      type Build[A, TC <: TypedColumn[?, ?, ?, ?]] =
        TC match
          case TypedColumn[n, b, ?, ?] => MappedColumn[n, A, b]
