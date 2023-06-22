package skunk.tables.internal

import scala.compiletime.{ constValue, constValueTuple }
import scala.compiletime.ops.any.==

import skunk.tables.TypedColumn

import munit.FunSuite

class AllRequiredSuite extends FunSuite:

  test("foo") {
    object Foo:
      opaque type Foo = Int

    type Columns = (
      TypedColumn["id",          Int,            "tasks_items", (TypedColumn.Constraint.Default.type, TypedColumn.Constraint.Primary.type)], 
      TypedColumn["created_at",  Int,            "tasks_items", Tuple1[TypedColumn.Constraint.Default.type]], 
      TypedColumn["created_by",  Int,            "tasks_items", EmptyTuple.type], 
      TypedColumn["row_version", Int,            "tasks_items", Tuple1[TypedColumn.Constraint.Default.type]], 
      TypedColumn["row_status",  Option[Foo.Foo],        "tasks_items", EmptyTuple.type], 
      TypedColumn["updated_at",  Option[Int],    "tasks_items", EmptyTuple.type], 
      TypedColumn["updated_by",  Option[Int],    "tasks_items", EmptyTuple.type], 
      TypedColumn["title",       String,         "tasks_items", EmptyTuple.type], 
      TypedColumn["assignee_id", Option[Int],    "tasks_items", EmptyTuple.type], 
      TypedColumn["status",      Int,            "tasks_items", Tuple1[TypedColumn.Constraint.Default.type]], 
      TypedColumn["due",         Option[String], "tasks_items", EmptyTuple.type], 
      TypedColumn["repeating",   Option[Int],    "tasks_items", EmptyTuple.type]
    )

    assert(true)
  }

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
    type Columns = (
      TypedColumn["one",   Boolean, "foo", EmptyTuple],
      TypedColumn["two",   Int,     "foo", EmptyTuple],
      TypedColumn["three", String,  "foo", EmptyTuple],
    )

    assert(constValueTuple[Required[Columns]] == ("one", "two", "three"))
  }

  // @see https://github.com/lampepfl/dotty/issues/17211
  test("Required ignores Option types") {
    type Columns = (
      TypedColumn["one",   Option[Boolean], "foo", EmptyTuple],
      TypedColumn["two",   Int,             "foo", EmptyTuple],
      TypedColumn["three", Option[String],  "foo", TypedColumn.Constraint.Nullable.type *: EmptyTuple],
    )

    assert(constValueTuple[Required[Columns]] == "one" *: "two" *: EmptyTuple)
  }

  test("Required drops fields with Default Constraint") {
    type Columns = (
      TypedColumn["one",   Boolean, "foo", EmptyTuple],
      TypedColumn["two",   Int,     "foo", TypedColumn.Constraint.Default.type *: EmptyTuple],
      TypedColumn["three", String,  "foo", EmptyTuple],
    )

    assert(constValueTuple[Required[Columns]] == "one" *: "three" *: EmptyTuple)
  }

  test("Required can return EmptyTuple") {
    type Columns = (
      TypedColumn["one",   Option[Boolean], "foo", TypedColumn.Constraint.Nullable.type *: EmptyTuple],
      TypedColumn["two",   Option[Int],     "foo", TypedColumn.Constraint.Default.type *: EmptyTuple],
      TypedColumn["three", String,          "foo", (TypedColumn.Constraint.Unique.type, TypedColumn.Constraint.Default.type)],
    )

    assert(constValueTuple[Required[Columns]] == EmptyTuple)
  }

  test("AllMapped returns true if missing columns has `Default` constraint") {
    type Columns = (
      TypedColumn["one",   Boolean, "foo", EmptyTuple],
      TypedColumn["two",   Int,     "foo", EmptyTuple],
      TypedColumn["three", String,  "foo", (TypedColumn.Constraint.Unique.type, TypedColumn.Constraint.Default.type)],
    )

    type Ins = (
      TypedColumn.In["one",   Int, Boolean],
      TypedColumn.In["two",   Int, Int],
    )

    assert(constValue[AllRequired.AllMapped[Columns, Ins]])
  }

  test("AllMapped returns false if one column is missing") {
    type Columns = (
      TypedColumn["one",   Boolean, "foo", EmptyTuple],
      TypedColumn["two",   Int,     "foo", EmptyTuple],
      TypedColumn["three", String,  "foo", (TypedColumn.Constraint.Unique.type, TypedColumn.Constraint.Default.type)],
    )

    type Ins = (
      TypedColumn.In["one",   Int, Boolean],
      // Missing required "two"
      TypedColumn.In["three", Int, Int],
    )

    type False = AllRequired.AllMapped[Columns, Ins]

    assert(!constValue[False])
  }

  test("AllMapped should not be synthesized if some columns are missing") {
    type Columns = (
      TypedColumn["one",   Boolean, "foo", EmptyTuple],
      TypedColumn["two",   Int,     "foo", EmptyTuple],
      TypedColumn["three", String,  "foo", (TypedColumn.Constraint.Unique.type, TypedColumn.Constraint.Default.type)],
    )

    type Ins = (
      TypedColumn.In["one",   Int, Boolean],
      TypedColumn.In["two",   Int, Int],
    )

    summon[AllRequired[Columns, Ins]]
  }
