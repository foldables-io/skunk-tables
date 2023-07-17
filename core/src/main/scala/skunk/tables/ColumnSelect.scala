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

import scala.deriving.Mirror
import scala.quoted.*

import cats.data.NonEmptyList

import quotidian.{MacroMirror, MirrorElem}

import skunk.tables.internal.{MacroTable, Constants, MacroColumn}

/** `Columns` is a "selectable" trait, which means the members of it are created dynamically at
  * compile-time. Every member maps a member of case class (`T` param from `Table[T]`) into a
  * Postgres column, also preservin the type information.
  *
  * It's typically created as part of `Table` and used in its API, but nothing stops you from
  * accessing typed columns of `table.columns` directly
  */
trait ColumnSelect extends Selectable:

  /** Either `TypedColumn` or `TypedColumn.Insert` */
  type In

  /** A function to extract column name */
  def getName(in: In): String

  // TODO: do the same about `codec` and use it in `toString`

  /** Homongenous tuple of `In`s */
  type TypedColumns <: NonEmptyTuple

  /** A tuple of `TypedColumn`. The actual type is inferred dur synthesis `all` is reserved word in
    * Postgres, so should not cause collisions
    */
  def all: TypedColumns

  /** A list of columns. Used as a shortcur only `get` is reserved word in Postgres, so should not
    * cause collisions
    */
  def get: List[In] =
    all.toList.asInstanceOf[List[In]]

  transparent inline def selectDynamic(name: String): Any =
    get.find(column => getName(column) == name).get

  override def toString: String =
    s"ColumnSelect(${get.map(getName).mkString(", ")})"

object ColumnSelect:

  /** A constructor to get a `Selectable` for all known columns */
  private[tables] def buildAllImpl[T: Type]
    (using quotes: Quotes)
    (macroTable: MacroTable.FinalPhase[quotes.type, T])
    : Expr[ColumnSelect] =
    import quotes.reflect.*

    val nameTypeMap = macroTable.columnMap

    val typedColumns: Expr[Tuple] = macroTable.getTypedColumns

    val tableName = ConstantType(StringConstant(macroTable.tableName))

    val refinement = nameTypeMap
      .map { case (name, tpr) => (name, tpr) }
      .foldLeft(TypeRepr.of[ColumnSelect])
      .apply { case (acc, (name, tpr)) =>
        val constraint = macroTable.getConstraints(name)
        (tpr.asType, Singleton(Expr(name).asTerm).tpe.asType) match
          case ('[tpe], '[name]) =>
            Refinement(parent = acc,
                       name = name,
                       info = TypeRepr
                         .of[TypedColumn]
                         .appliedTo(List(Singleton(Expr(name).asTerm).tpe, TypeRepr.of[tpe], tableName, constraint))
            )
      }

    (typedColumns.asTerm.tpe.asType, refinement.asType) match
      case ('[typedColumns], '[refinement]) =>
        '{
          (new ColumnSelect:
            self =>
            type In = TypedColumn[?, ?, ?, ?] // Necessary for `getName`
            val all                                  = ${ typedColumns }.asInstanceOf[self.TypedColumns]
            def getName(in: TypedColumn[?, ?, ?, ?]) = in.n
          ).asInstanceOf[
            ColumnSelect { type In = TypedColumn[?, ?, ?, ?]; type TypedColumns = typedColumns } & refinement
          ]
        }

  /** A constructor to get a `Selectable` only for default and primary columns. It will be used in
    * `table.get`
    */
  private[tables] def buildGetImpl[T: Type]
    (using quotes: Quotes)
    (macroTable: MacroTable.FinalPhase[quotes.type, T])
    : Expr[ColumnSelect] =
    import quotes.reflect.*

    val nameTypeMap = macroTable.columnMap

    val typedColumns: Expr[Tuple] = macroTable.getPrimUniqColumns

    val tableName = ConstantType(StringConstant(macroTable.tableName))

    val refinement = nameTypeMap
      .foldLeft(TypeRepr.of[ColumnSelect])
      .apply { case (acc, (name, tpr)) =>
        if macroTable.isPrimUniq(name) then
          val constraint = macroTable.getConstraints(name)
          (tpr.asType, Singleton(Expr(name).asTerm).tpe.asType) match
            case ('[tpe], '[name]) =>
              Refinement(parent = acc,
                         name = name,
                         info = TypeRepr
                           .of[TypedColumn]
                           .appliedTo(List(Singleton(Expr(name).asTerm).tpe, TypeRepr.of[tpe], tableName, constraint))
              )
        else acc
      }

    (typedColumns.asTerm.tpe.asType, refinement.asType) match
      case ('[typedColumns], '[refinement]) =>
        '{
          (new ColumnSelect:
            self =>
            type In = TypedColumn[?, ?, ?, ?] // Necessary for `getName`
            val all                                  = ${ typedColumns }.asInstanceOf[self.TypedColumns]
            def getName(in: TypedColumn[?, ?, ?, ?]) = in.n
          ).asInstanceOf[
            ColumnSelect { type In = TypedColumn[?, ?, ?, ?]; type TypedColumns = typedColumns } & refinement
          ]
        }

  transparent inline def buildInsert[TT, Insert](table: TT) =
    ${ buildInsertImpl[TT, Insert]('table) }

  import CanInsert.CanInsertPartialFinal

  private def buildInsertImpl[T: Type, Insert: Type](using Quotes)(tableExpr: Expr[T]) =
    import quotes.reflect.*

    val macroTable = MacroTable.buildFromExpr(tableExpr)

    val columnSelect              = buildColumnSelect[T, Insert](tableExpr)
    val typedColumns: Expr[Tuple] = Expr.ofTupleFromSeq(macroTable.getInsertColumnsList[Insert])

    (macroTable.tpe, columnSelect.asTerm.tpe.asType, typedColumns.asTerm.tpe.asType) match
      case ('[t], '[s], '[c]) =>
        '{
          new CanInsertPartialFinal[Insert, t, s, c & NonEmptyTuple]($columnSelect.asInstanceOf[s]) {}
            .asInstanceOf[CanInsertPartialFinal[Insert, t, s, c & NonEmptyTuple]]
        }

  private def buildColumnSelect[T: Type, Insert: Type](using Quotes)(tableExpr: Expr[T]) =
    import quotes.reflect.*

    val macroTable = MacroTable.buildFromExpr(tableExpr)

    val nameTypeMap = macroTable.columns.map(column => (column.name, column.tpe, column.constraints))

    val typedColumns: Expr[Tuple] = Expr.ofTupleFromSeq(macroTable.getInsertColumnsList[Insert])

    val refinement = nameTypeMap
      .foldLeft(TypeRepr.of[ColumnSelect])
      .apply { case (acc, (name, tpr, c)) =>
        (tpr.asType, Singleton(Expr(name).asTerm).tpe.asType, c.asType) match
          case ('[tpe], '[name], '[constraints]) =>
            Refinement(parent = acc,
                       name = name,
                       info = TypeRepr
                         .of[TypedColumn.Insert]
                         .appliedTo(
                           List(Singleton(Expr(name).asTerm).tpe,
                                TypeRepr.of[tpe],
                                TypeRepr.of[constraints],
                                TypeRepr.of[Insert]
                           )
                         )
            )
      }

    (typedColumns.asTerm.tpe.asType, refinement.asType) match
      case ('[typedColumns], '[refinement]) =>
        '{
          (new ColumnSelect:
            self =>
            type In = TypedColumn.Insert[?, ?, ?, Insert] // Necessary for `getName`
            def getName(in: TypedColumn.Insert[?, ?, ?, Insert]) = in.n
            val all                                              = ${ typedColumns }.asInstanceOf[self.TypedColumns]
          ).asInstanceOf[
            ColumnSelect {
              type In = TypedColumn.Insert[?, ?, ?, Insert]; type TypedColumns = typedColumns
            } & refinement
          ]
        }
