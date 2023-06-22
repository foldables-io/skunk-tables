package skunk.tables

import cats.implicits.*

import skunk.*

import munit.CatsEffectSuite

class FromTableSuite extends CatsEffectSuite:

  test("FromTable successfully synthesized for subset of columns in wrong order") {
    type Columns = (
      TypedColumn["one",   Boolean, "foo", EmptyTuple],
      TypedColumn["two",   Int,     "foo", EmptyTuple],
      TypedColumn["three", String,  "foo", EmptyTuple],
    )

    val instance = summon[FromTable[Columns, ("three", "one")]]

    assert(instance.columns == List("three", "one"))
  }

