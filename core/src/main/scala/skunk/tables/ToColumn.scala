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

package skunk.tables

import java.time.LocalDateTime

import scala.quoted.*

import _root_.io.github.iltotore.iron.IronType

import skunk.{Fragment, Void, Encoder, ~}
import skunk.implicits.*

/** ToColumn is similar to `TypedColumn`, but used only for inserting
  *
  * @tparam N
  *   string singleton with column name
  * @tparam A
  *   the actual Scala type of the class member
  * @tparam T
  *   table type
  * @tparam C
  *   a list of column constraints
  */
final case class ToColumn[N <: Singleton, A, T, C <: Tuple](name: N, primitive: IsColumn[A]):

  val n: String = name.toString

  def from(get: T => A): ToColumn.In[N, T, A] =
    ToColumn.In(get, primitive)

object ToColumn:

  final case class In[N <: Singleton, A, B](get: A => B, primitive: IsColumn[B]):
    def use(a: A): B = get(a)
