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

import skunk.{~, Void}

import scala.Tuple.Append
import scala.compiletime.{erasedValue, constValue, summonInline}

import skunk.tables.{TypedColumn, IsColumn}

// TypedColumn operations

/** Transform `(TypedColumn[?, a, ?, ?], TypedColumn[?, b, ?, ?])` into `a ~ b` */
type TwiddleTC[C <: Tuple] =
  C match
    case EmptyTuple => Void
    // Below returns single item, not a tuple, which allows us to do `returning["id" *: EmptyTuple]`
    // and get a single value, not a whole tuple. Might be unexpected
    case TypedColumn[?, b, ?, ?] *: EmptyTuple => b
    case TypedColumn[?, b1, ?, ?] *: tail      => b1 ~ TwiddleTCGo[tail, EmptyTuple]
type TwiddleTCGo[C <: NonEmptyTuple, A <: Tuple] =
  (C, A) match
    case (TypedColumn[?, b, ?, ?] *: EmptyTuple, EmptyTuple) => b
    case (TypedColumn[?, b1, ?, ?] *: TypedColumn[?, b2, ?, ?] *: EmptyTuple, EmptyTuple) =>
      b1 ~ b2
    case (TypedColumn[?, b1, ?, ?] *: TypedColumn[?, b2, ?, ?] *: tail, EmptyTuple) =>
      TwiddleTCGo[tail, b1 ~ b2]
    case (TypedColumn[?, b, ?, ?] *: tail, acc) => TwiddleTCGo[tail, acc ~ b]

/** Same as `TwiddleTC`, but for non-empty tuples */
type TwiddleTCN[C <: NonEmptyTuple] <: Product =
  C match
    case TypedColumn[?, c, ?, ?] *: EmptyTuple => c *: EmptyTuple
    case TypedColumn[?, c1, ?, ?] *: TypedColumn[?, c2, ?, ?] *: t =>
      TwiddleTCNGo[t, c1 ~ c2]
type TwiddleTCNGo[C <: Tuple, A <: Tuple] <: Tuple =
  (C, A) match
    case (EmptyTuple, acc)                   => A
    case (TypedColumn[?, c, ?, ?] *: t, acc) => TwiddleTCNGo[t, acc ~ c]

type NonRequiredConstraints =
  (TypedColumn.Constraint.Nullable.type, TypedColumn.Constraint.Default.type)

// Ideally we'd check if type is in Option right here, but falling back to `Nullable` because
// the match won't work with opaque types: https://github.com/lampepfl/dotty/issues/17211
/** All items from `A` are present in `C` */
type Required[C <: NonEmptyTuple] <: Tuple =
  C match
    case TypedColumn.Insert[n, ?, c, ?] *: t =>
      RequiredGo[t, IfInM[c, NonRequiredConstraints, EmptyTuple, n *: EmptyTuple]]
type RequiredGo[C <: Tuple, A <: Tuple] <: Tuple =
  C match
    case EmptyTuple => A
    case TypedColumn.Insert[n, ?, c, ?] *: t =>
      RequiredGo[t, IfInM[c, NonRequiredConstraints, A, Append[A, n]]]

/** Get a `TypedColumn` by its label */
type Find[C <: Tuple, Label <: Singleton] =
  C match
    case TypedColumn[Label, a, t, c] *: ? => TypedColumn[Label, a, t, c]
    case TypedColumn[?, ?, ?, ?] *: t     => Find[t, Label]

inline def findT[C <: Tuple, Label <: Singleton]: Find[C, Label] =
  inline erasedValue[C] match
    case _: (TypedColumn[Label, a, t, c] *: ?) =>
      TypedColumn[Label, a, t, c](constValue[Label], summonInline[IsColumn[a]])
    case _: (TypedColumn[?, ?, ?, ?] *: tail) =>
      findT[tail, Label]

/** Pick only `TypedColumns` with certain labels */
type Pick[Columns <: Tuple, Labels <: Tuple] <: Tuple =
  Labels match
    case EmptyTuple          => EmptyTuple
    case IsSingleton[h] *: t => Find[Columns, h] *: Pick[Columns, t]

inline def pickT[C <: Tuple, Labels <: Tuple]: Pick[C, Labels] =
  inline erasedValue[Labels] match
    case _: EmptyTuple            => EmptyTuple
    case _: (IsSingleton[h] *: t) => findT[C, h] *: pickT[C, t]

type Reify[C <: Tuple] <: Tuple =
  C match
    case EmptyTuple =>
      EmptyTuple
    case TypedColumn[IsSingleton[n], a, t, c] *: tail =>
      TypedColumn[n, a, t, c] *: Reify[tail]

// WARNING: it doesn't work if there's a type with given instance not
// in usual scope (companion object)
/** Reify a homogenous tuple of `TypedColumn` */
inline def reifyT[C <: Tuple]: Reify[C] =
  inline erasedValue[C] match
    case _: EmptyTuple =>
      EmptyTuple
    case _: (TypedColumn[IsSingleton[n], a, t, c] *: tail) =>
      TypedColumn[n, a, t, c](constValue[n], summonInline[IsColumn[a]]) *: reifyT[tail]

type ReifyN[C <: NonEmptyTuple] <: NonEmptyTuple =
  C match
    case TypedColumn[IsSingleton[n], a, t, c] *: EmptyTuple =>
      TypedColumn[n, a, t, c] *: EmptyTuple
    case TypedColumn[IsSingleton[n], a, t, c] *: h2 *: tail =>
      TypedColumn[n, a, t, c] *: ReifyN[h2 *: tail]

/** Reify a homogenous tuple of `TypedColumn` */
inline def reifyNT[C <: NonEmptyTuple]: ReifyN[C] =
  inline erasedValue[C] match
    case _: (TypedColumn[IsSingleton[n], a, t, c] *: EmptyTuple) =>
      TypedColumn[n, a, t, c](constValue[n], summonInline[IsColumn[a]]) *: EmptyTuple
    case _: (TypedColumn[IsSingleton[n], a, t, c] *: h2 *: tail) =>
      TypedColumn[n, a, t, c](constValue[n], summonInline[IsColumn[a]]) *: reifyNT[h2 *: tail]

type GetNames[C <: Tuple] <: Tuple =
  C match
    case EmptyTuple                   => EmptyTuple
    case TypedColumn[n, ?, ?, ?] *: t => n *: GetNames[t]

inline def getNamesT[C <: Tuple]: GetNames[C] =
  inline erasedValue[C] match
    case _: EmptyTuple =>
      EmptyTuple
    case _: (TypedColumn[n, ?, ?, ?] *: t) =>
      constValue[n] *: getNamesT[t]

// TypedColumn.In operations

type GetInNames[I <: NonEmptyTuple] <: NonEmptyTuple =
  I match
    case TypedColumn.In[IsSingleton[n], ?, ?] *: EmptyTuple => n *: EmptyTuple
    case TypedColumn.In[IsSingleton[n], ?, ?] *: t          => n *: GetInNames[t]

/** Match type to transform `(a, b, c, d)` of `TypedColumn.In` into `(((a, b), c), d)` (or `a ~ b ~
  * c ~ d`)
  */
type TwiddleIn[I <: NonEmptyTuple] =
  I match
    case TypedColumn.In[?, ?, b] *: EmptyTuple => Tuple1[b]
    case TypedColumn.In[?, ?, b1] *: TypedColumn.In[?, ?, b2] *: t =>
      TwiddleInGo[t, b2 ~ b1]
type TwiddleInGo[I <: Tuple, A <: Tuple] =
  (I, A) match
    case (EmptyTuple, acc)                      => acc
    case (TypedColumn.In[?, ?, b] *: tail, acc) => TwiddleInGo[tail, acc ~ b]

// Generic operations

/** Type-level unapply, checking if `T` is a singleton */
type IsSingleton[T <: Singleton] = T

/** If `A` in tuple `T` - return `True` branch, otherwise `False` branch */
type IfIn[T <: Tuple, A, True, False] <: True | False = T match
  case EmptyTuple => False
  case A *: t     => True
  case ? *: t     => IfIn[t, A, True, False]

/** Same as `IfIn`, but works for multiple inputs */
type IfInM[T <: Tuple, A <: Tuple, True, False] <: True | False =
  A match
    case EmptyTuple => False
    case a *: tail  => IfIn[T, a, True, IfInM[T, tail, True, False]]

/** Confirm that `Set` has all elements from `SubSet` */
infix type HasAll[Set <: NonEmptyTuple, SubSet <: Tuple] <: Boolean =
  SubSet match
    case EmptyTuple => true
    case h *: t     => IfIn[Set, h, HasAll[Set, t], false]
