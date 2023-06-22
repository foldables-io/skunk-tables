package skunk.tables

import cats.implicits.*
import cats.effect.IO

import skunk.*

import munit.CatsEffectSuite
import java.time.LocalDateTime


class TableSuite extends CatsEffectSuite:
  test("table.all empty") {
    val expected = List()

    val result = Reset.getClean.use { session =>
      TableSuite.table.all[IO].run(session).compile.toList
    }

    assertIO(result, expected)
  }

  test("table.insert returning") {
    val expected = (1L, 35)

    val columns: ("id", "age") = ("id", "age")
    val result: IO[(Long, Int)] = Reset.getClean.use { session =>
      TableSuite.table.insert[IO, TableSuite.PersonNew](TableSuite.PersonNew(1, "Anton", 35)).returning(columns).run(session)
    }

    assertIO(result, expected)
  }

  test("table.insert returning single field") {
    val expected = 1L

    val columns: "id" *: EmptyTuple = "id" *: EmptyTuple
    val result: IO[Long] = Reset.getClean.use { session =>
      TableSuite.table.insert[IO, TableSuite.PersonNew](TableSuite.PersonNew(1, "Anton", 35)).returning(columns).run(session)
    }

    assertIO(result, expected)
  }


  test("table.insert returning and nested classes") {
    val expected = (1, 35)

    val columns: ("id", "age") = ("id", "age")
    val result: IO[(Int, Int)] = Reset.getClean.use { session =>
      TableSuite.tableWithMeta.insert[IO, TableSuite.PersonNew](TableSuite.PersonNew(1, "Anton", 35)).returning(columns).run(session)
    }

    assertIO(result, expected)
  }

  test("table.all") {
    import TableSuite.*

    val staticTime = LocalDateTime.parse("2023-06-03T11:05:34.095949")

    val expected = List(PersonWithMeta(Meta(1, staticTime), PersonInfo("Anton", 35))) 

    val result: IO[List[TableSuite.PersonWithMeta]] = Reset.getClean.use { session =>
      TableSuite.tableWithMeta.insert[IO, TableSuite.PersonNew](TableSuite.PersonNew(1, "Anton", 35)).run(session) *>
       TableSuite.tableWithMeta.all.run(session).map(x => x.copy(meta = x.meta.copy(createdAt = staticTime))).compile.toList
    }

    assertIO(result, expected)
  }

object TableSuite:

  case class Person(id: Long, firstName: String, age: Int)
  val table = Table.of[Person].withName("persons").build

  case class PersonNew(id: Int, firstName: String,  age: Int)

  given CanInsert[PersonNew, Person] =
    CanInsert[PersonNew].into(table).via(columns => (
      columns.id.from[PersonNew](_.id),
      columns.first_name.from[PersonNew](_.firstName),
      columns.age.from[PersonNew](_.age),
    ))

  case class Meta(id: Int, createdAt: LocalDateTime)
  case class PersonInfo(firstName: String, age: Int)
  case class PersonWithMeta(meta: Meta, info: PersonInfo)
  val tableWithMeta = Table
    .of[PersonWithMeta]
    .withName("persons_with_meta")
    .withDefault("id")
    .withDefault("created_at")
    .build

  given CanInsert[PersonNew, PersonWithMeta] =
    CanInsert[PersonNew].into(tableWithMeta).via(columns => (
      columns.first_name.from[PersonNew](_.firstName),
      columns.age.from[PersonNew](_.age),
    ))

