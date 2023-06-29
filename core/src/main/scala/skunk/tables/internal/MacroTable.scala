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

import scala.Singleton as SSingleton
import scala.quoted.*

import cats.data.NonEmptyList

import quotidian.{MacroMirror, MirrorElem}

import skunk.tables.{IsColumn, TypedColumn}

/** MacroTable is a class containing all information necessary for `Table` synthezis. It can be of
  * two phases, depending on how much information we have about type `A`
  */
sealed trait MacroTable[Q <: Quotes & Singleton, A]:
  val quotes: Q

  import quotes.reflect.*

  given Q = quotes

  /** User-provided type of table contents */
  def tpe: Type[A]

  def columnMap: NonEmptyList[(String, (quotes.reflect.TypeRepr, Expr[IsColumn[?]]))]

  /** Get a list of all known table columns. At different stages it would return different amount of
    * info. At init stage we know only name and `IsColumn` instance. At final stage we know all
    * constraints.
    */
  def getTypedColumnsList: List[Expr[TypedColumn[?, ?, ?, ?]]]

  /** List of normalized column names, guaranteed to be distinct and non-empty */
  def getNames: NonEmptyList[String] =
    columnMap.map(_._1)

  /** Get union type of all possible column names */
  def getNamesUnion: TypeRepr =
    getNames match
      case NonEmptyList(a, b :: rest) =>
        rest
          .foldLeft(OrType(ConstantType(StringConstant(a)), ConstantType(StringConstant(b))))
          .apply((orType, name) => OrType(orType, ConstantType(StringConstant(name))))
      case NonEmptyList(a, Nil) =>
        ConstantType(StringConstant(a))

  /** Get ordered tuple of all column names */
  def getNamesTuple: Expr[Tuple] =
    Expr.ofTupleFromSeq(getNames.toList.map(name => Expr(name)))

  /** Get ordered tuple of all columns as `TypedColumn` Note: it creates only values here, types for
    * `Columns` created there within refinement
    */
  def getTypedColumns: Expr[Tuple] =
    Expr.ofTupleFromSeq(getTypedColumnsList)

  /** Get a list of column names where type is some form of `Option` */
  def getNullableColumns: Expr[Tuple] =
    Expr.ofTupleFromSeq(columnMap.toList.flatMap { case (n, (t, p)) =>
      val nameExpr = Expr(n)
      t.dealias.show match
        case t if t.startsWith("scala.Option") =>
          Some(nameExpr)
        case _ =>
          None
    })

object MacroTable:

  /** Init phase is when `TableBuilder` knows only information derived from `A` type */
  class InitPhase[Q <: Quotes & Singleton, A]
    (val quotes: Q,
     val tpe: Type[A],
     val columnMap: NonEmptyList[(String, (quotes.reflect.TypeRepr, Expr[IsColumn[?]]))]
    ) extends MacroTable[Q, A]:
    import quotes.reflect.*

    def next
      (constraints: List[(String, quotes.reflect.TypeRepr, quotes.reflect.TypeRepr)], tableName: String)
      : FinalPhase[Q, A] =
      new FinalPhase[Q, A](quotes, tpe, columnMap, constraints, tableName)

    def getTypedColumnsList: List[Expr[TypedColumn[?, ?, ?, ?]]] =
      columnMap.toList.map { case (n, (t, p)) =>
        val nameExpr = Expr(n)

        (nameExpr.asTerm.tpe.asType, t.asType) match
          case ('[name], '[tpe]) =>
            val name = nameExpr.asExprOf[name & SSingleton]
            '{
              new TypedColumn[name & SSingleton, tpe, Nothing, EmptyTuple](${ name }, ${ p.asExprOf[IsColumn[tpe]] })
            }
      }

  /** Final phase is when `TableBuilder` went through its methods and got more information from user
    *
    * @param Constraint
    *   a list of triplets where first element is a name of a column, second is a `TypedColumn` (so
    *   contains name AND constranits, full info) and third is just a tupled of constraints
    *   extracted from second element
    */
  class FinalPhase[Q <: Quotes & Singleton, A]
    (val quotes: Q,
     val tpe: Type[A],
     val columnMap: NonEmptyList[(String, (quotes.reflect.TypeRepr, Expr[IsColumn[?]]))],
     val constraints: List[(String, quotes.reflect.TypeRepr, quotes.reflect.TypeRepr)],
     val tableName: String
    ) extends MacroTable[Q, A]:
    import quotes.reflect.*

    /** Get a tuple of fully-typed constraints for a particular column */
    def getConstraints(label: String): quotes.reflect.TypeRepr =
      constraints.find((name, _, _) => name == label) match
        case Some((_, _, tpr)) =>
          tpr
        case None if constraints.isEmpty =>
          TypeRepr
            .of[EmptyTuple] // It means the method being called on first stage
        case None =>
          report.errorAndAbort(
            s"Column with name $label was not found. Check consistency of Table building. Known columns"
          )

    /** Check if a label has `Unique` or `Primary` constraint */
    def isPrimUniq(label: String): Boolean =
      val materialized = materializeConstraints[Q](quotes)(getConstraints(label))
      materialized.contains(TypedColumn.Constraint.Primary.toString) || materialized.contains(
        TypedColumn.Constraint.Unique.toString
      )

    def getTypedColumnsList: List[Expr[TypedColumn[?, ?, ?, ?]]] =
      val tableNameType = Singleton(Expr(tableName).asTerm).tpe.asType

      columnMap.toList.map { case (n, (t, p)) =>
        val nameExpr = Expr(n)

        val constraint = getConstraints(n)

        (nameExpr.asTerm.tpe.asType, t.asType, tableNameType, constraint.asType) match
          case ('[name], '[tpe], '[tableName], '[constr]) =>
            val name = nameExpr.asExprOf[name & SSingleton]
            '{
              new TypedColumn[name & SSingleton, tpe, tableName, constr & Tuple](${ name },
                                                                                 ${ p.asExprOf[IsColumn[tpe]] }
              )
            }
      }

    def getPrimUniqColumns: Expr[Tuple] =
      Expr.ofTupleFromSeq(getPrimUniqTypedColumnsList)

    def getPrimUniqTypedColumnsList: List[Expr[TypedColumn[?, ?, ?, ?]]] =
      val tableNameType = Singleton(Expr(tableName).asTerm).tpe.asType

      columnMap.toList.flatMap { case (n, (t, p)) =>
        val nameExpr = Expr(n)

        if isPrimUniq(n) then
          val constraint = getConstraints(n)
          val result = (nameExpr.asTerm.tpe.asType, t.asType, tableNameType, constraint.asType) match
            case ('[name], '[tpe], '[tableName], '[constr]) =>
              val name = nameExpr.asExprOf[name & SSingleton]
              '{
                new TypedColumn[name & SSingleton, tpe, tableName, constr & Tuple](${ name },
                                                                                   ${ p.asExprOf[IsColumn[tpe]] }
                )
              }

          Some(result)
        else None
      }

  /** Transform a `TypeRepr` of a single `Constraint` into `String` representation */
  def getConstraints[Q <: Quotes & Singleton](q: Q)(repr: q.reflect.TypeRepr): Option[String] =
    import q.reflect.*

    repr match
      case TermRef(TermRef(TermRef(TermRef(ThisType(_), _), "TypedColumn"), "Constraint"), c) =>
        Some(c)
      case _ =>
        None

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

  /** Build an init phase of `MacroTable`. At this point there's no info about table (name or
    * constraints). Use `.next` to get to the final phase
    */
  def build[T: Type](using quotes: Quotes): MacroTable.InitPhase[quotes.type, T] =
    import quotes.reflect.*

    val mirror = MacroMirror.summonProduct[T]

    val columnMap =
      NonEmptyList.fromList(flattenProduct[T](Nil)(mirror.elems).map((path, tpe) => snakeCase(path.last) -> tpe)) match
        case Some(nel) => nel
        case None =>
          report.errorAndAbort("Could not derive columns. A Columns must contain at least one Column")

    val names = columnMap.map(_._1)
    if names.distinct.length != names.length
    then report.errorAndAbort(s"Not all column names are unique (${names.toList.mkString(", ")})")

    new MacroTable.InitPhase(quotes, Type.of[T], columnMap)

  def flattenProduct[T]
    (using quotes: Quotes)
    (root: List[String])
    (ls: List[MirrorElem[quotes.type, T, ?]])
    : List[(NonEmptyList[String], (quotes.reflect.TypeRepr, Expr[IsColumn[?]]))] =
    import quotes.reflect.*

    ls.flatMap { elem =>
      elem.asType match
        case '[t] =>
          Expr.summon[IsColumn[t]] match
            case Some(p) =>
              List(NonEmptyList(elem.label, root).reverse -> (elem.typeRepr, p))
            case None =>
              MacroMirror.summon[t] match
                case Some(pm: MacroMirror.ProductMacroMirror[quotes.type, t]) =>
                  flattenProduct[t](elem.label :: root)(pm.elems)
                case _ =>
                  quotes.reflect.report.errorAndAbort(
                    s"Couldn't synthesize IsColumn instance for ${TypeRepr.of[t].show}, which also happens to be not a Product type"
                  )
    }

  def snakeCase(str: String): String =
    str
      .replaceAll("([A-Z]+)([A-Z][a-z])", "$1_$2")
      .replaceAll("([a-z\\d])([A-Z])", "$1_$2")
      .toLowerCase
