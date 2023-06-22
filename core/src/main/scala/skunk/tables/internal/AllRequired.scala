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
