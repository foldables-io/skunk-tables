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

import cats.implicits.*

import skunk.*

import munit.CatsEffectSuite

class FromTableSuite extends CatsEffectSuite:

  test("FromTable successfully synthesized for subset of columns in wrong order") {
    type Columns = (TypedColumn["one", Boolean, "foo", EmptyTuple],
                    TypedColumn["two", Int, "foo", EmptyTuple],
                    TypedColumn["three", String, "foo", EmptyTuple])

    val instance = summon[FromTable[Columns, ("three", "one")]]

    assert(instance.columns == List("three", "one"))
  }
