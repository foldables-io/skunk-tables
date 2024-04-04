package skunk.tables

import cats.data.NonEmptyList
import skunk.tables.internal.MacroQuery
import skunk.tables.internal.MacroTable

object Sql:

  // table.select(columns => (columns.id, columns.name)).where(columns => columns.age > 21)
  // table.select(columns => (columns.id, columns.name)).where(columns => columns.age > 21).count
  // table.select(columns => (columns.id, columns.name)).where(columns => columns.age > 21).orderBy(desc).limit(10)

  // GROUPING!
  // table.select(columns => (columns.department, avg(columns.salary))).groupBy(columns => columns.department)
  // table
  //   .select(columns => (columns.department, avg(columns.salary)))
  //   .where(columns => columns.age > 30)
  //   .groupBy(columns => columns.department)

  // // OR

  // table.query(columns => columns.age > 21).map(columns => (columns.id, columns.name))

  // query (or select) should construct a compile-time object
  // that later translated to run-time object

  case class Tbl(name: String, columns: (String, String))
