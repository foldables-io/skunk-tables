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

import skunk.tables.internal.MacroTable

/**
  * `Columns` is a "selectable" trait, which means the members of it
  * are created dynamically at compile-time. Every member maps a member
  * of case class (`T` param from `Table[T]`) into a Postgres column,
  * also preservin the type information.
  *
  * It's typically created as part of `Table` and used in its API,
  * but nothing stops you from accessing typed columns of `table.columns`
  * directly
  */
trait ColumnSelect extends Selectable:

  /** Homongenous tuple of `TypedColumn` */
  type TypedColumns <: NonEmptyTuple

  /**
   * A tuple of `TypedColumn`. The actual type is inferred dur synthesis
   * `all` is reserved word in Postgres, so should not cause collisions
   */
  def all: TypedColumns

  /**
   * A list of `TypedColumn`. Used as a shortcur only
   * `get` is reserved word in Postgres, so should not cause collisions
   */
  def get: List[TypedColumn[?, ?, ?, ?]] =
    all.toList.asInstanceOf[List[TypedColumn[?, ?, ?, ?]]]

  transparent inline def selectDynamic(name: String): Any =
    get.find(_.n == name).get

  override def toString: String =
    s"ColumnSelect(${get.map(c => s"${c.n}: ${c.primitive.codec.types.mkString(",")}").mkString(", ")})"
    

object ColumnSelect:

  private[tables] def buildImpl[T: Type](using quotes: Quotes)(macroTable: MacroTable.FinalPhase[quotes.type, T]): Expr[ColumnSelect] =
    import quotes.reflect.*

    val nameTypeMap = macroTable.columnMap

    val typedColumns: Expr[Tuple] = macroTable.getTypedColumns

    val tableName = ConstantType(StringConstant(macroTable.tableName))

    val refinement = nameTypeMap
      .map { case (name, (tpr, _)) => (name, tpr) }
      .foldLeft(TypeRepr.of[ColumnSelect])
      .apply { case (acc, (name, tpr)) =>
        val constraint = macroTable.getConstraint(name)
        (tpr.asType, Singleton(Expr(name).asTerm).tpe.asType) match
          case ('[tpe], '[name]) =>
            Refinement(
              parent = acc,
              name = name,
              info = TypeRepr.of[TypedColumn].appliedTo(List(Singleton(Expr(name).asTerm).tpe, TypeRepr.of[tpe], tableName, constraint))
            )
      }

    (typedColumns.asTerm.tpe.asType, refinement.asType) match
      case ('[typedColumns], '[refinement]) =>
        '{
          (new ColumnSelect { self =>
            val all = ${ typedColumns }.asInstanceOf[self.TypedColumns]
          }).asInstanceOf[ColumnSelect { type TypedColumns = typedColumns } & refinement ]
        }

