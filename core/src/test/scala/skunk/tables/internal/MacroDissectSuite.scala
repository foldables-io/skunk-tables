/*
 * Copyright 2024 Foldables
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

import skunk.tables.Dissect

import munit.FunSuite

class MacroDissectSuite extends FunSuite:

  test("Single Leaf to") {
    case class One(a: Int, b: String)

    val dissect = Dissect.build[One]

    val input  = One(42, "one")
    val result = dissect.to(input)

    assert(result == (42, "one"))
  }

  test("Shallow Branch to") {
    case class One(a: Int, b: String)
    case class Two(a: Long, one: One)

    val dissect = Dissect.build[Two]

    val input  = Two(42L, One(142, "one"))
    val result = dissect.to(input)
    assert(result == (42L, 142, "one"))
  }

  test("Shallow Branch from") {
    case class One(a: Int, b: String)
    case class Two(a: Long, one: One)

    val dissect = Dissect.build[Two]

    val input  = (42L, 142, "one")
    val result = dissect.from(input)

    assert(result == Two(42L, One(142, "one")))
  }

  test("Double Branch from") {
    case class One(a: Int, b: String)
    case class Two(a: Long, one1: One, one2: One)

    val dissect = Dissect.build[Two]

    val input  = (42L, 141, "one", 142, "two")
    val result = dissect.from(input)

    assert(result == Two(42L, One(141, "one"), One(142, "two")))
  }

  test("Deep Branch from") {
    case class One(a: Int, b: String)
    case class Two(a: Int, one: One)
    case class Three(a: Int, one: One, two: Two)

    val dissect = Dissect.build[Three]
    val input   = (1, 20, "one-1", 3, 4, "one-2")
    val result  = dissect.from(input)

    assert(result == Three(1, One(20, "one-1"), Two(3, One(4, "one-2"))))
  }

  test("Deep Branch and 1-arity constructor from") {
    case class Zero(s: Int)
    case class One(a: Int, b: String, z: Zero)
    case class Two(a: Int, one: One)
    case class Three(a: Int, one: One, two: Two, o1: One)

    val dissect = Dissect.build[Three]
    val input   = (1, 20, "one-1", 0, 3, 4, "one-2", 0, 5, "one-3", 0)
    val result  = dissect.from(input)

    assert(result == Three(1, One(20, "one-1", Zero(0)), Two(3, One(4, "one-2", Zero(0))), One(5, "one-3", Zero(0))))
  }

  test("Tuple2 to") {
    type One = (Int, String)

    val dissect = Dissect.build[One]

    val input  = (42, "one")
    val result = dissect.to(input)

    assert(result == (42, "one"))
  }

  test("Tuple1 to") {
    type One = Tuple1[Int]

    val dissect = Dissect.build[One]

    val input  = Tuple1(42)
    val result = dissect.to(input)

    assert(result == Tuple1(42))
  }
