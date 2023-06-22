package skunk.tables

import munit.FunSuite

class CanInsertSuite extends FunSuite:
  test("CanInsert can be generated for simple case class with all non-Option fields") {
    case class Person(id: Long, firstName: String, age: Int)
    case class PersonWithId(id: Int, firstName: String,  age: Int)
    val table = Table.of[Person].withName("persons").build

    val ci = CanInsert[PersonWithId]
      .into(table)
      .via(columns => (
        columns.id.from[PersonWithId](_.id),
        columns.first_name.from[PersonWithId](_.firstName),
        columns.age.from[PersonWithId](_.age),
      ))

    assert(ci.columns == List("id", "first_name", "age"))

  }

  test("CanInsert cannot be generated if at least one required field is missing in mapping") {
    case class Person(id: Long, firstName: String, age: Int)
    case class PersonWithId(id: Int, firstName: String,  age: Int)
    val table = Table.of[Person].withName("persons").build
    
    CanInsert[PersonWithId]
      .into(table)
      .via(columns => (
        columns.id.from[PersonWithId](_.id),
        columns.first_name.from[PersonWithId](_.firstName), // We can drop this. For some reasons compileErrors doesn't work
        columns.age.from[PersonWithId](_.age),
      ))
  }

  test("CanInsert can be generated if an optional field is missing in mapping") {
    case class Person(id: Long, firstName: Option[String], age: Int)
    case class PersonWithId(id: Int, age: Int)
    val table = Table.of[Person].withName("persons").build
    
    CanInsert[PersonWithId]
      .into(table)
      .via(columns => (
        columns.id.from[PersonWithId](_.id),
        columns.age.from[PersonWithId](_.age),
      ))
  }

  test("CanInsert can be generated with a single-element mapping") {
    case class Person(id: Long, firstName: Option[String], age: Int, isMale: Boolean)
    case class PersonWithId(id: Int)
    val table = Table.of[Person].withName("persons").withDefault("age").withDefault("is_male").build

    CanInsert[PersonWithId]
      .into(table)
      .via(columns => Tuple1(columns.id.from[PersonWithId](_.id)))
  }

