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

import skunk.tables.{IsColumn, TypedColumn}

/** Compile-time counterpart of `TypedColumn` */
sealed trait MacroColumn[Q <: Quotes & Singleton]:
  val quotes: Q

  import quotes.reflect.*

  def name: String
  def tpe: TypeRepr
  def isColumn: Expr[IsColumn[?]]

object MacroColumn:
  final class InitPhase[Q <: Quotes & Singleton]
    (val q: Q, val name: String, val tpe: q.reflect.TypeRepr, val isColumn: Expr[IsColumn[?]]):
    def next(tableName: String): FinalPhase[Q] =
      new FinalPhase(q, name, tpe, isColumn, Nil, tableName)
    override def toString: String = s"MacroColumn.InitPhase($name, ${tpe.show})"

  object InitPhase:
    def fromTypedColumn(using q: Quotes)(typedColumn: q.reflect.TypeRepr): MacroColumn.InitPhase[q.type] =
      import q.reflect.*

      typedColumn match
        case AppliedType(tpe, columns) if isTypedColumn(tpe) =>
          columns match
            case List(ConstantType(StringConstant(name)), typeRepr, _, _) =>
              typeRepr.asType match
                case '[tpe] =>
                  Expr.summon[IsColumn[tpe]] match
                    case Some(isColumn) =>
                      new MacroColumn.InitPhase(q, name, typeRepr, isColumn)
                    case None => report.errorAndAbort(s"Cannot summon IsColumn for ${typeRepr.show}")
            case _ =>
              report.errorAndAbort(
                s"Applied types of ${typedColumn.show} don't TypedColumn structure with 4 type holes"
              )
        case _ =>
          report.errorAndAbort(s"TypeRepr ${typedColumn.show} doesn't match expected TypedColumn structure")

    def fromTypedColumns
      (using q: Quotes)
      (appliedType: q.reflect.TypeRepr)
      : NonEmptyList[MacroColumn.InitPhase[q.type]] =
      import q.reflect.*

      appliedType match
        case AppliedType(_, columns) =>
          NonEmptyList.fromList(columns.map(InitPhase.fromTypedColumn)) match
            case Some(nel) => nel.asInstanceOf[NonEmptyList[MacroColumn.InitPhase[q.type]]]
            case None      => report.errorAndAbort("Resulting table has no columns")
        case _ =>
          report.errorAndAbort(
            s"TypeRepr ${appliedType.show} doesn't match expected structure of tuple of $Constants.TypedColumnsName"
          )

  final class FinalPhase[Q <: Quotes & Singleton]
    (val quotes: Q,
     val name: String,
     val tpe: quotes.reflect.TypeRepr,
     val isColumn: Expr[IsColumn[?]],
     val constraints: List[TypedColumn.Constraint],
     val tableName: String
    ) extends MacroColumn[Q]:

    def toColumnType: quotes.reflect.TypeRepr =
      import quotes.reflect.*

      val tycon = Symbol.requiredClass(s"skunk.${Constants.TablesPackageName}.${Constants.TypedColumnName}").typeRef
      AppliedType(tycon,
                  List(ConstantType(StringConstant(name)),
                       tpe,
                       ConstantType(StringConstant(tableName)),
                       constraintsTuple(quotes)(constraints)
                  )
      )

    def addConstraint(constraint: TypedColumn.Constraint): FinalPhase[Q] =
      import quotes.reflect.*
      given Quotes = quotes
      tpe.asType match
        case '[t] =>
          new FinalPhase(quotes, name, TypeRepr.of[t], isColumn, (constraint :: constraints).distinct, tableName)

    override def toString: String = s"MacroColumn.FinalPhase($name, ${tpe.show}, ${constraints}, $tableName)"

  object FinalPhase:
    def fromTypedColumn(using q: Quotes)(typedColumn: q.reflect.TypeRepr): MacroColumn.FinalPhase[q.type] =
      import q.reflect.*

      def getConstraints(tpe: TypeRepr): List[TypedColumn.Constraint] =
        Utils.materializeConstraints(q)(tpe).map(TypedColumn.Constraint.valueOf)

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
                      new MacroColumn.FinalPhase(q,
                                                 name,
                                                 typeRepr,
                                                 isColumn,
                                                 getConstraints(constraints),
                                                 tableName
                      ) // TODO!!!
                    case None => report.errorAndAbort(s"Cannot summon IsColumn for ${typeRepr.show}")
            case _ =>
              report.errorAndAbort(s"Applied types of ${typedColumn.show} don't TypedColumn structure with 4 type holes")
        case _ =>
          report.errorAndAbort(s"TypeRepr ${typedColumn.show} doesn't match expected TypedColumn structure")

    def fromTypedColumns(using q: Quotes)(appliedType: q.reflect.TypeRepr): NonEmptyList[MacroColumn.FinalPhase[q.type]] =
      import q.reflect.*

      appliedType match
        case AppliedType(_, columns) =>
          NonEmptyList.fromList(columns.map(fromTypedColumn)) match
            case Some(nel) => nel.asInstanceOf[NonEmptyList[MacroColumn.FinalPhase[q.type]]]
            case None      => report.errorAndAbort("Resulting table has no columns")
        case _ =>
          report.errorAndAbort(
            s"TypeRepr ${appliedType.show} doesn't match expected structure of tuple of $Constants.TypedColumnsName"
          )


  def constraintsTuple[Q <: Quotes & Singleton](q: Q)(cs: List[TypedColumn.Constraint]): q.reflect.TypeRepr =
    import q.reflect.*
    given Quotes = q

    val ConstraintObj = TypeRepr.of[TypedColumn.Constraint.type].classSymbol.get
    val typeRefs      = cs.map(item => ConstraintObj.fieldMember(item.toString).termRef)

    if typeRefs.isEmpty then TypeRepr.of[EmptyTuple]
    else defn.TupleClass(typeRefs.length).typeRef.dealias.appliedTo(typeRefs)

  def isTypedColumn(using q: Quotes)(typeRef: q.reflect.TypeRepr): Boolean =
    import q.reflect.*

    typeRef match
      case TypeRef(ThisType(TypeRef(_, Constants.TablesPackageName)), Constants.TypedColumnName) => true
      case _                                                                                     => false

