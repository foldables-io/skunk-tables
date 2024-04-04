package skunk.tables

import scala.compiletime.{constValue, constValueTuple}
import scala.Tuple.Concat

import java.util.UUID

import skunk.Codec
import skunk.codec.text.varchar
import skunk.codec.numeric.int4
import skunk.codec.uuid.uuid

import Column.{Constraint, IsColumn}


type AppendUnique[T <: Tuple, A] <: Tuple = T match
  case EmptyTuple => A *: T
  case A *: t => A *: t
  case h *: t => h *: AppendUnique[t, A]

type ContainsMatch[T <: Tuple, A] <: Boolean = T match
  case EmptyTuple => false
  case A *: t => true
  case h *: t => ContainsMatch[t, A]

type IsOptionMatch[T] <: Boolean = T match
  case Option[?] => true
  case _ => false

type ContainsNot[T <: Tuple, A] = ContainsMatch[T, A] =:= false

type IsOption[T] = IsOptionMatch[T] =:= true

final case class Column[N <: String & Singleton, A, C <: Tuple] private[tables] (ev: IsColumn[A]):
  /** When Database provides a default value */
  def withDefault(using ContainsNot[C, Constraint.Default.type]): Column[N, A, Constraint.Default.type *: C] =
    Column[N, A, Constraint.Default.type *: C](ev)

object Column:

  // A hacky workaround until SIP-47 is implemented
  object of:
    trait Partial[A]:
      protected def isColumn: IsColumn[A]

      def apply[N <: String & Singleton](name: N) =
        Column[N, A, EmptyTuple](isColumn)

    def apply[A](using ev: IsColumn[A]) = new Partial[A]:
      def isColumn: IsColumn[A] = ev

  trait IsColumn[A]:
    def codec: Codec[A]

    override def toString: String = s"IsColumn.of(${codec.types.mkString(", ")})"

  object IsColumn:
    def ofCodec[A](c: Codec[A]) = new IsColumn[A]:
      val codec = c

    given IsColumn[UUID]     = ofCodec(uuid)
    given IsColumn[Int]      = ofCodec(int4)
    given IsColumn[String]   = ofCodec(varchar)

    given [A] (using IsColumn[A]): IsColumn[Option[A]] =
      new IsColumn[Option[A]]:
        val codec = summon[IsColumn[A]].codec.opt

  enum Constraint:
    case Nullable
    case Primary
    case Default
    case Unique