package skunk.tables.internal

import scala.annotation.implicitNotFound

/**
 * A marker type class confirming that all non-null, non-default `TypedColumns` of `C`
 * are present in `I` (as `TypedColumn.In`)
 * In other words, if there's non-null, non-default column in `C` that is *not* in `C`
 * the compilation will abort
 */
@implicitNotFound("Cannot proove that all required fields (${C} tuple) are listed in user-provided ${I} tuple")
trait AllRequired[C <: NonEmptyTuple, I <: NonEmptyTuple]

object AllRequired:

  inline given [C <: NonEmptyTuple, I <: NonEmptyTuple](using AllMapped[C, I] =:= true): AllRequired[C, I] =
    new AllRequired[C, I] { }

  /** Confirm that all required columns from `C` are present in `I` */
  type AllMapped[C <: NonEmptyTuple, I <: NonEmptyTuple] =
    GetInNames[I] HasAll Required[C]
