package skunk.tables.internal

import scala.annotation.tailrec
import scala.quoted.*

import cats.implicits.*

import skunk.tables.Column.Constraint

class Matchers[Q <: Quotes & Singleton](val q: Q):
  import q.reflect.*

  def unwrapTupleCons(tprs: List[TypeRepr]): Option[List[TypeRepr]] =
    @tailrec
    def go(acc: List[TypeRepr], tprsGo: List[TypeRepr]): Option[List[TypeRepr]] =
      tprsGo match
        case h :: EmptyTupleType() :: Nil =>
          Some(h :: acc)
        case h :: AppliedType(TypeRef(TermRef(ThisType(IsRoot()), "scala"), "*:"), next) :: Nil =>
          go(h :: acc, next)
        case EmptyTupleType() :: Nil =>
          Some(acc)
        case _ =>
          None

    go(Nil, tprs).map(_.reverse)

  object IsRoot:
    def unapply(tpr: TypeRepr): Boolean = tpr match
      case NoPrefix() => true
      case TypeRef(NoPrefix(), "<root>") => true
      case _ => false

  object IsInt:
    def unapply(tpr: TypeRepr): Boolean = tpr match
      case TypeRef(TermRef(ThisType(IsRoot()), "scala"), "Int") => true
      case _ => false

  object IsString:
    def unapply(tpr: TypeRepr): Boolean = tpr match
      case TypeRef(ThisType(TypeRef(NoPrefix(), "lang")), "String") => true
      case _ => false

  object EmptyTupleType:
    def unapply(tpr: TypeRepr): Boolean = tpr match
      case TypeRef(TermRef(TermRef(ThisType(IsRoot()), "scala"), "Tuple$package"), "EmptyTuple") => true
      case TermRef(TermRef(TermRef(ThisType(IsRoot()), "scala"), "Tuple$package"), "EmptyTuple") => true
      case _ => false

  object TupleOfArity:
    def unapply(tpr: TypeRepr): Option[Int] =
      tpr match
        case AppliedType(TypeRef(ThisType(TypeRef(IsRoot(), "scala")), name), members) if name.startsWith("Tuple") =>
          scala.util.Try(name.dropWhile(c => !c.isDigit).toInt).toOption match
            case Some(arity) if arity == members.length => Some(arity)
            case Some(arity) => report.errorAndAbort(s"Tuple's arity ${arity} doesn't match amount of members ${members.length}")
            case None => None
        case EmptyTupleType() =>
          Some(0)
        case AppliedType(TypeRef(TermRef(ThisType(IsRoot()), "scala"), "*:"), next) =>
          unwrapTupleCons(next).map(_.length)
        case _ =>
          None

  object TupleMembers:
    def unapply(tpr: TypeRepr): Option[List[TypeRepr]] =
      tpr match
        case AppliedType(TypeRef(ThisType(TypeRef(IsRoot(), "scala")), name), members) if name.startsWith("Tuple") =>
          scala.util.Try(name.dropWhile(c => !c.isDigit).toInt).toOption match
            case Some(arity) if arity == members.length => Some(members)
            case Some(arity) => report.errorAndAbort(s"Tuple's arity ${arity} doesn't match amount of members ${members.length}")
            case None => None
        case EmptyTupleType() =>
          Some(Nil)
        case AppliedType(TypeRef(TermRef(ThisType(IsRoot()), "scala"), "*:"), next) =>
          unwrapTupleCons(next)
        case _ =>
          None


  // My own matchers
  object UnappliedColumn:
    def unapply(tpr: TypeRepr): Option[Unit] =
      tpr match
        case TypeRef(TermRef(ThisType(TypeRef(NoPrefix(), "skunk")), "tables"), "Column") =>
          Some(())
        case TypeRef(ThisType(TypeRef(NoPrefix(), "tables")),"Column") =>
          Some(())
        case _ =>
          None

  object ConstraintType:
    def unapply(tpr: TypeRepr): Option[Constraint] =
      tpr match
        case TermRef(TermRef(TermRef(TermRef(ThisType(TypeRef(NoPrefix(), "skunk")),"tables"),"Column"),"Constraint"),name) =>
          Some(Constraint.valueOf(name))
        case _ =>
          None


  object Constraints:
    def unapply(tpr: TypeRepr): Option[List[Constraint]] =
      tpr match
        case TupleMembers(tprs) =>
          tprs.traverse:
            case ConstraintType(c) => Some(c)
            case _ => None
        case _ =>
          None


  object ColumnType:
    def unapply(tpr: TypeRepr): Option[(String, List[Constraint])] =
      tpr match
        case AppliedType(UnappliedColumn(_), List(ConstantType(StringConstant(name)), tp, Constraints(constraints))) =>
          println(tp)
          Some((name, constraints))
        case z =>
          None


object Matchers:
  def mk[Q <: Quotes & Singleton](using Q): Matchers[Q] =
    new Matchers(quotes)

object MacroQuery:

  inline transparent def build[T](inline t: T): Any =
    ${ buildImpl[T]('{t}) }

  // AppliedType(_,List(AppliedType(TypeRef(),List(ConstantType(Constant(hello)), TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object scala),class Int), AppliedType(TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object scala),class *:),List(TermRef(TermRef(TermRef(ThisType(TypeRef(NoPrefix,module class tables)),object Column),object Constraint),val Default), TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class scala)),object Tuple$package),type EmptyTuple))))), AppliedType(TypeRef(ThisType(TypeRef(NoPrefix,module class tables)),class Column),List(ConstantType(Constant(hello)), TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object scala),class Int), AppliedType(TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class <root>)),object scala),class *:),List(TermRef(TermRef(TermRef(ThisType(TypeRef(NoPrefix,module class tables)),object Column),object Constraint),val Default), TypeRef(TermRef(ThisType(TypeRef(NoPrefix,module class scala)),object Tuple$package),type EmptyTuple)))))))
  private def buildImpl[T: Type](t: Expr[T])(using Quotes) =
    import quotes.reflect.*

    val m = Matchers.mk

    t.asTerm match
      case Inlined(a, b, c) =>
        println(a)
        println(b)
        println(c)
      case tt =>
        println(tt.show)


    TypeRepr.of[T] match
      case m.TupleMembers(List(m.ColumnType(a, b), _)) =>
        println((a, b))
      case other =>
        println("oh no")
        println(other)

    '{ () }
