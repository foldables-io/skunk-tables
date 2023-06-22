package skunk.tables

import scala.compiletime.{ erasedValue, constValue, summonInline, constValueTuple }

import skunk.*
import skunk.implicits.*

import skunk.tables.internal.{ TwiddleTC, Pick }


/**
 * An evidence holding info for selecting a set of columns (or labels - `RL`)
 * from a table (`C`). Holds type info, decoder and column names.
 *
 * It has single instance and gets generated automatically if all
 * labels in `RL` have a matching counterpart in table columns `C`
 */
trait FromTable[C <: NonEmptyTuple, RL <: Tuple]:

  type Out

  type Columns <: Tuple

  def decoder: Decoder[Out]

  /** Column names in proper order */
  def columns: List[String]

  def columnsFragment: Fragment[Void] =
    sql"#${columns.mkString(", ")}"


object FromTable:

  inline transparent given [C <: NonEmptyTuple, RL <: Tuple]: FromTable[C, RL]  =
    new FromTable[C, RL]:
      /** A homogenous tuple of `TypedColumn` */
      type Columns =
        Pick[C, RL]

      /** A tuple derivated from `Columns` by extracting `A` type from each `TypedColumn` */
      type Out =
        TwiddleTC[Columns]

      def columns: List[String] =
        constValueTuple[RL].toList.map(_.toString)
      
      def decoder =
        getCodec[Columns]


  /**
   * Summon all instances of `Codec` (via `IsColumn`) into a twiddled tuple
   * generated from `T`
   */
  inline def getCodec[T <: Tuple]: Decoder[TwiddleTC[T]] =
    val codec = inline erasedValue[T] match
      case EmptyTuple =>
        Void.codec
      case _: (TypedColumn[?, h1, ?, ?] *: EmptyTuple) =>
        summonInline[IsColumn[h1]].codec
      case _: (TypedColumn[?, h1, ?, ?] *: t) =>
        summonInline[IsColumn[h1]].codec ~ getCodec[t]

    codec.asInstanceOf[Decoder[TwiddleTC[T]]]

