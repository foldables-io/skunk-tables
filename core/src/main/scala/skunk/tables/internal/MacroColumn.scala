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

import quotidian.{MacroMirror, MirrorElem}

import skunk.tables.IsColumn

/** Compile-time counterpart of `TypedColumn` */
final class MacroColumn[Q <: Quotes & Singleton]
  (val quotes: Q,
   val name: String,
   val tpe: quotes.reflect.TypeRepr,
   val constraints: quotes.reflect.TypeRepr,
   val tableName: String,
   val isColumn: Expr[IsColumn[?]]
  ):

  def toColumnType: quotes.reflect.TypeRepr =
    import quotes.reflect.*

    val tycon = Symbol.requiredClass(s"skunk.${Constants.TablesPackageName}.${Constants.TypedColumnName}").typeRef
    AppliedType(tycon, List(ConstantType(StringConstant(name)), tpe, ConstantType(StringConstant(tableName)), constraints))

  def forColumnMap: (String, (quotes.reflect.TypeRepr, Expr[IsColumn[?]])) =
    name -> (tpe, isColumn)
  def forConstraints: (String, quotes.reflect.TypeRepr) =
    (name, constraints)

object MacroColumn:
  def isTypedColumn(using q: Quotes)(typeRef: q.reflect.TypeRepr): Boolean =
    import q.reflect.*

    typeRef match
      case TypeRef(ThisType(TypeRef(_, Constants.TablesPackageName)), Constants.TypedColumnName) => true
      case _                                                                                     => false

  def fromTypedColumn(using q: Quotes)(typedColumn: q.reflect.TypeRepr): MacroColumn[q.type] =
    import q.reflect.*

    typedColumn match
      case AppliedType(tpe, columns) if isTypedColumn(tpe) =>
        columns match
          case List(ConstantType(StringConstant(name)),
                    typeRepr,
                    ConstantType(StringConstant(tableName)),
                    constraints
              ) =>
            typeRepr.asType match
              case '[tpe] =>
                Expr.summon[IsColumn[tpe]] match
                  case Some(isColumn) =>
                    new MacroColumn(q, name, typeRepr, constraints, tableName, isColumn)
                  case None => report.errorAndAbort(s"Cannot summon IsColumn for ${typeRepr.show}")
          case _ =>
            report.errorAndAbort(s"Applied types of ${typedColumn.show} don't TypedColumn structure with 4 type holes")
      case _ =>
        report.errorAndAbort(s"TypeRepr ${typedColumn.show} doesn't match expected TypedColumn structure")

  def fromTypedColumns(using q: Quotes)(appliedType: q.reflect.TypeRepr): NonEmptyList[MacroColumn[q.type]] =
    import q.reflect.*

    appliedType match
      case AppliedType(_, columns) =>
        NonEmptyList.fromList(columns.map(fromTypedColumn)) match
          case Some(nel) => nel.asInstanceOf[NonEmptyList[MacroColumn[q.type]]]
          case None      => report.errorAndAbort("Resulting table has no columns")
      case _ =>
        report.errorAndAbort(
          s"TypeRepr ${appliedType.show} doesn't match expected structure of tuple of $Constants.TypedColumnsName"
        )
