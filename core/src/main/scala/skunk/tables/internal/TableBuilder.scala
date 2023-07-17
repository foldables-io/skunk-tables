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

import cats.data.NonEmptyList

import skunk.tables.{TypedColumn, Table, ColumnSelect, Dissect}

/** A factory, providing set of configuration methods, such as `withUnique`, `withName` to configure
  * the actual table
  *
  * The class itself supposed to be internal. Most members exist only at type-level
  */
trait TableBuilder[T <: Product]:
  self =>

  type Name <: String

  /** Union of all column names */
  type ColumnName
  type Columns <: NonEmptyTuple

  type Primary <: Tuple
  type Unique <: Tuple
  type Default <: Tuple
  type Nullable <: Tuple

  transparent inline def withName[N <: String & Singleton](label: N): TableBuilder[T] =
    new TableBuilder[T]:
      type Name       = N
      type ColumnName = self.ColumnName
      type Columns    = self.Columns
      type Primary    = self.Primary
      type Unique     = self.Unique
      type Default    = self.Default
      type Nullable   = self.Nullable

  transparent inline def withPrimary[Label <: self.ColumnName & Singleton](label: Label): TableBuilder[T] =
    new TableBuilder[T]:
      type Name       = self.Name
      type ColumnName = self.ColumnName
      type Columns    = self.Columns
      type Primary    = Label *: self.Primary
      type Unique     = self.Unique
      type Default    = self.Default
      type Nullable   = self.Nullable

  transparent inline def withUnique[Label <: self.ColumnName & Singleton](label: Label): TableBuilder[T] =
    new TableBuilder[T]:
      type Name       = self.Name
      type ColumnName = self.ColumnName
      type Columns    = self.Columns
      type Primary    = self.Primary
      type Unique     = Label *: self.Unique
      type Default    = self.Default
      type Nullable   = self.Nullable

  transparent inline def withDefault[Label <: self.ColumnName & Singleton](label: Label): TableBuilder[T] =
    new TableBuilder[T]:
      type Name       = self.Name
      type ColumnName = self.ColumnName
      type Columns    = self.Columns
      type Primary    = self.Primary
      type Unique     = self.Unique
      type Default    = Label *: self.Default
      type Nullable   = self.Nullable

object TableBuilder:

  /** This where `Table.of` leads to */
  def init[T <: Product: Type](using Quotes): Expr[TableBuilder[T]] =
    import quotes.reflect.*

    val macroTable = MacroTable.build[T]

    val namesUnion   = macroTable.getNamesUnion.asType
    val typedColumns = macroTable.getTypedColumns.asTerm.tpe.asType

    val nullableColumns = macroTable.getNullableColumns.asTerm.tpe

    (namesUnion, typedColumns, nullableColumns.asType) match
      case ('[namesUnion], '[typedColumns], '[nullableColumns]) =>
        type Init = TableBuilder[T] {
          type ColumnName = namesUnion
          type Columns    = typedColumns
          type Primary    = EmptyTuple
          type Unique     = EmptyTuple
          type Default    = EmptyTuple
          type Nullable   = nullableColumns
        }

        '{ (new TableBuilder[T] {}).asInstanceOf[Init] }

  extension [P <: Product, N, A, U, D, C, O]
    (builder: TableBuilder[P] {
      type Name    = N; type Primary  = A; type Unique = U; type Default = D;
      type Columns = C; type Nullable = O
    })
    inline transparent def build: Table[P] =
      ${ buildImpl[P, N, A, U, D, C, O] }

  /** @tparam P
    *   the product we're describing a table for
    * @tparam N
    *   a name of the table (a string singleton)
    * @tparam A
    *   a tuple of names which we'd like to make primary keys
    * @tparam U
    *   a tuple of names which we'd like to make unique
    * @tparam D
    *   a tuple of names which have defaults for
    * @tparam C
    *   a tuple of all column names in the table
    * @tparam O
    *   a tuple of nullable (optional) column names in the table
    */
  private def buildImpl[P <: Product: Type, N: Type, A: Type, U: Type, D: Type, C: Type, O: Type](using q: Quotes) =
    import quotes.reflect.*

    def addConstraint(constraint: TypedColumn.Constraint, labels: List[String])
      (columns: NonEmptyList[MacroColumn.FinalPhase[q.type]])
      : NonEmptyList[MacroColumn.FinalPhase[q.type]] =
      columns.map(col => if labels.contains(col.name) then col.addConstraint(constraint) else col)


    val tableName = TypeRepr.of[N] match
      case ConstantType(StringConstant(name)) => name

    val primary  = materializeTuple(TypeRepr.of[A])
    val unique   = materializeTuple(TypeRepr.of[U])
    val default  = materializeTuple(TypeRepr.of[D])
    val nullable = materializeTuple(TypeRepr.of[O])

    val unconstrainedColumns =
      MacroColumn.InitPhase.fromTypedColumns(TypeRepr.of[C]).map(_.next(tableName))

    val columns = addConstraint(TypedColumn.Constraint.Primary, primary)
      .andThen(addConstraint(TypedColumn.Constraint.Unique, unique))
      .andThen(addConstraint(TypedColumn.Constraint.Default, default))
      .andThen(addConstraint(TypedColumn.Constraint.Nullable, nullable))
      .apply(unconstrainedColumns)

    val macroTable = MacroTable.build[P].next(columns, tableName)

    val allColumnsSelect = ColumnSelect.buildAllImpl[P](macroTable)
    val getColumnsSelect = ColumnSelect.buildGetImpl[P](macroTable)

    val namesUnion: TypeRepr = macroTable.getNamesUnion

    val mTypedColumns = macroTable.getTypedColumns

    val macroDissect = MacroDissect.build[P]

    val dissectS = Dissect.buildImpl[P]

    (allColumnsSelect.asTerm.tpe.asType,
     getColumnsSelect.asTerm.tpe.asType,
     namesUnion.asType,
     mTypedColumns.asTerm.tpe.asType,
     macroDissect.outType) match
      case ('[allSelectType], '[getSelectType], '[namesUnion], '[typedColumnsType], '[dissectOutType]) =>
        type Final = Table[P] {
          type TypedColumns = typedColumnsType
          type Select       = allSelectType
          type SelectGet    = getSelectType
          type ColumnName   = namesUnion
          type Columns      = dissectOutType
        }

        '{
          (new Table[P]:
            self =>
            def typedColumns =
              ${ mTypedColumns }.asInstanceOf[self.TypedColumns]
            val select    = ${ allColumnsSelect }.asInstanceOf[self.Select]
            val selectGet = ${ getColumnsSelect }.asInstanceOf[self.SelectGet]
            val dissect = ${ dissectS }
              .asInstanceOf[Dissect.AuxT[P, self.Columns, TwiddleTCN[self.TypedColumns]]]
            val name = Table.Name(${ Expr(tableName) })
          ).asInstanceOf[Final]

        }


  def deconstruct(using quotes: Quotes)(columns: quotes.reflect.TypeRepr): NonEmptyList[quotes.reflect.TypeRepr] =
    import quotes.reflect.*

    columns match
      case AppliedType(_, typedColumns) =>
        val list = typedColumns.map { case AppliedType(_, List(_, _, _, constraints)) => constraints }

        NonEmptyList.fromList(list) match
          case Some(nel) => nel
          case None      => report.errorAndAbort("Columns cannot be an EmptyTuple")

  /** Transform tuple of literal string types into `List` */
  def materializeTuple(using Quotes)(repr: quotes.reflect.TypeRepr): List[String] =
    import quotes.reflect.*

    repr match
      case AppliedType(_, ConstantType(StringConstant(c)) :: Nil) =>
        c :: Nil
      case ConstantType(StringConstant(c)) =>
        c :: Nil
      case AppliedType(_, List(ConstantType(StringConstant(c)), at: AppliedType)) =>
        c :: materializeTuple(at)
      case AppliedType(_, cts) =>
        cts.flatMap(materializeTuple)
      case TermRef(_, _) =>
        Nil // EmptyTuple
      case TypeRef(_, _) =>
        Nil
