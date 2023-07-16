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

import scala.compiletime.{constValue, constValueTuple}
import scala.compiletime.ops.any.==

import skunk.tables.TypedColumn

import munit.FunSuite

class TypeOpsSuite extends FunSuite:

  type Default            = Tuple1[TypedColumn.Constraint.Default.type]
  type Nullable           = Tuple1[TypedColumn.Constraint.Nullable.type]
  type PrimaryWithDefault = Tuple2[TypedColumn.Constraint.Default.type, TypedColumn.Constraint.Primary.type]
  type UniqueWithDefault  = Tuple2[TypedColumn.Constraint.Default.type, TypedColumn.Constraint.Unique.type]

  test("IfIn checks literal type for existence") {
    type Result = IfIn[("one", "two", "three"), "two", 1, 0]

    assert(constValue[Result] == 1)
  }

  test("IfIn checks literal type for absence") {
    type Result = IfIn[("one", "two", "three"), "four", 1, 0]

    assert(constValue[Result] == 0)
  }

  test("HasAll returns true if second argument is a subset") {
    type True = HasAll[("one", "x", "two", "three"), ("one", "two")]

    assert(constValue[True])
  }

  test("HasAll returns true if second argument is an equal set") {
    type True = HasAll[("one", "two", "three"), ("one", "two", "three")]

    assert(constValue[True])
  }

  test("HasAll returns false if second argument is not a subset") {
    type False = HasAll[("one", "two", "three"), ("one", "two", "four")]

    assert(!constValue[False])
  }

  test("HasAll returns false if same amount, but different elements") {
    type False = HasAll[("foo", "two"), ("foo", "three")]

    assert(!constValue[False])
  }

  test("Required returns all non-Option fields in their original order") {
    type Columns = (TypedColumn.Insert["one", Boolean, EmptyTuple, Nothing],
                    TypedColumn.Insert["two", Int, EmptyTuple, Nothing],
                    TypedColumn.Insert["three", String, EmptyTuple, Nothing])

    assert(constValueTuple[Required[Columns]] == ("one", "two", "three"))
  }

  // @see https://github.com/lampepfl/dotty/issues/17211
  test("Required ignores Option types") {
    type Columns = (TypedColumn.Insert["one", Option[Boolean], EmptyTuple, Nothing],
                    TypedColumn.Insert["two", Int, EmptyTuple, Nothing],
                    TypedColumn.Insert["three", Option[String], Nullable, Nothing])

    assert(constValueTuple[Required[Columns]] == "one" *: "two" *: EmptyTuple)
  }

  test("Required drops fields with Default Constraint") {
    type Columns = (TypedColumn.Insert["one", Boolean, EmptyTuple, Unit],
                    TypedColumn.Insert["two", Int, Default, Unit],
                    TypedColumn.Insert["three", String, EmptyTuple, Unit])

    assert(constValueTuple[Required[Columns]] == "one" *: "three" *: EmptyTuple)
  }

  test("Required can return EmptyTuple") {
    type Columns =
      (TypedColumn.Insert["one", Option[Boolean], Nullable, Nothing],
       TypedColumn.Insert["two", Option[Int], Default, Nothing],
       TypedColumn.Insert["three", String, UniqueWithDefault, Nothing])

    assert(constValueTuple[Required[Columns]] == EmptyTuple)
  }

  test("AllMapped returns true if missing columns has `Default` constraint") {
    type Columns =
      (TypedColumn.Insert["one", Boolean, EmptyTuple, Nothing],
       TypedColumn.Insert["two", Int, EmptyTuple, Nothing],
       TypedColumn.Insert["three", String, UniqueWithDefault, Nothing])

    type Ins = (TypedColumn.In["one", Int, Boolean], TypedColumn.In["two", Int, Int])

    assert(constValue[AllRequired.AllMapped[Columns, Ins]])
  }

  test("AllMapped returns false if one column is missing") {
    type Columns =
      (TypedColumn.Insert["one", Boolean, EmptyTuple, Nothing],
       TypedColumn.Insert["two", Int, EmptyTuple, Nothing],
       TypedColumn.Insert["three", String, UniqueWithDefault, Nothing])

    type Ins = (TypedColumn.In["one", Int, Boolean],
                // Missing required "two"
                TypedColumn.In["three", Int, Int])

    type False = AllRequired.AllMapped[Columns, Ins]

    assert(!constValue[False])
  }

  test("AllMapped should not be synthesized if some columns are missing") {
    type Columns =
      (TypedColumn.Insert["one", Boolean, EmptyTuple, Nothing],
       TypedColumn.Insert["two", Int, EmptyTuple, Nothing],
       TypedColumn.Insert["three", String, UniqueWithDefault, Nothing])

    type Ins = (TypedColumn.In["one", Int, Boolean], TypedColumn.In["two", Int, Int])

    summon[AllRequired[Columns, Ins]]
  }
