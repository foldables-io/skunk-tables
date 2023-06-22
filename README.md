# Skunk Tables

`skunk-tables` is a high-level type-safe wrapper over [Skunk](https://github.com/tpolecat/skunk/) data access library.

## Why

It's a somewhat common sentiment in Scala community that ORM/FRM libraries do not add much value and nothing beats plain SQL queries at their simplicity.
[Skunk](https://github.com/tpolecat/skunk/) and [Doobie](https://github.com/tpolecat/doobie) libraries are champions in the plain-SQL competition and we love them very much and want to use everywhere.
However, sometimes we need to write a lot of boilerplate SQL with decoders and Scala's type system doesn't prevent anyone from writing non-sense in there,
especially on first stages of the app when we prototype and change things like tables, classes and columns very often.

`skunk-tables` is designed to solve the problem of many look-a-like queries and tables that have to match your cases classes.
It's not a replacement of Skunk, but an addition to it, allowing to reduce the boilerplate AND add some common source of truth for your SQL fragments.

## Overview

The library is...

* Type-safe. `skunk-tables` attempts to forbid many non-sense operations, such as `OFFSET` for `INSERT` statements or accessing non-existing columns. If you see string literals somewhere - they're likely type-checked.
* Standing on shoulders of giants and scratching the boilerplate. Skunk Tables is based on Skunk, but does not try to replace it. Instead, it uses
  as many Skunk features as possible and enables its users to combine low-level Skunk API (such as `Fragment`) with high-level Skunk Tables API.
  Yet, the paramount goal of Skunk Tables is to make it possible to write the most common statements in a most concise and correct way.
* Scala 3. Skunk Tables is macro-heavy and uses a lot of shiny new Scala 3 features hence will never be available for Scala 2

## Note about stablity and Roadmap

`skunk-tables` is at its 0.0.x stage. The API will change a lot and everyone is welcome to explore what we can do here.

Next steps:

* Joins
* Updates
* Table creation
* Better complie-time error messages (God bless you if you make a typo on 1+ `Table`s now)

## Limitations

### Scala 3

`skunk-tables` heavily uses Scala 3 macro system and hence will never be available on Scala 2.

### Type annotations

It's generally a bad idea to annotate anything coming from the library with its types, for example:

```scala
val table: Table[Person] = Table.of[Person].withName("persons").build
```

This would be a bad idea because table's type in fact is much more complicated and plain `Table[Person]` erases a lot of information.

## Using

Below sections assume you're working with Postgres table named `persons` and with following schema:

```sql
CREATE TABLE persons (
  first_name        VARCHAR(32)     NOT NULL,
)
```

Wich corresponds to the following case class:

```scala
final case class Person(firstName: String)

```

### Table

`Table` is the entrypoint. If you want to do anything with Skunk Tables - you need to start with defining your `Table` object for every class you'd like to work with.
`Table` is a Scala representation of your DB schema and has to match it.
Above table could be desribed as following:

```scala
import cats.effect.{ IO, Resource }
import skunk.Session
import skunk.tables.*

val session: Resource[IO, Session[IO]] = ???

object Person:
  val table =                   // Bear in mind, you typically don't want to annotate it with `Table[Person]` type as the object has many type memembers which would be erased
    Table
      .of[Person]               // We map every field of `Person` class to a Postgres table
      .withName("persons")      // It's better to specify the name manually
      .withPrimary("person_id") // All string-literals are obviously type-checked. It means you cannot misspell the column name
      .build                    // Transforming the `TableBuilder` into `Table`
```

Now you have the `table` object, which in turn has some self-describing methods, such as `count`, `all`, `insert` etc.
These methods represent what we believe are the most common operations one would like to perform over a Postgres table.

```scala
val count: Query[IO, "single", Long] = table.count
session.use(count.run)
```

### Low-level API

`Table` object is cool not only because it provides short-hand methods like `get` and `insert`, but also because it adds
a source of truth for the actual PostgreSQL table. You can pull out things like table name, column names, column operations
for custom complex SQL fragments:

```scala
val P = Person.table        // Just a short-hand

sql"""
SELECT  ${P.id}, ${P.firstName}, ${P.lastName}
FROM    ${P.table}
WHERE   ${P.age.eql(Codecs.int4)}
"""
```

Results into:

```sql
SELECT id, first_name, last_name
FROM persons
WHERE age = ${int4}
```

If you change your class (e.g. field name or type) you also must change your query or it won't compile.
