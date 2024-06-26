# Skunk Tables

[![license][license-image]](https://opensource.org/licenses/Apache-2.0)
[![Continuous Integration](https://github.com/foldables-io/skunk-tables/actions/workflows/ci.yml/badge.svg)](https://github.com/foldables-io/skunk-tables/actions/workflows/ci.yml)

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

* Type-safe. `skunk-tables` attempts to forbid many non-sense operations, such as `OFFSET` on `INSERT` statements or accessing non-existing columns. If you see string literals somewhere - they're likely type-checked.
* Standing on shoulders of giants and scratching the boilerplate. Skunk Tables is based on Skunk, but does not try to replace it. Instead, it uses
  as many Skunk features as possible and enables its users to combine low-level Skunk API (such as `Fragment`) with high-level Skunk Tables API.
  Yet, the paramount goal of Skunk Tables is to make it possible to write the most common statements in a most concise and correct way.

If you're interested to see how the code looks like - you can head over to a [small demo](https://github.com/foldables-io/skunk-tables/blob/main/core/src/test/scala/skunk/tables/Example.scala).

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
  id              SERIAL      PRIMARY KEY,
  first_name      VARCHAR     NOT NULL,
  last_name       VARCHAR     NULL,
  created_at      TIMESTAMP   DEFAULT current_timestamp
)
```

Which corresponds to the following case class:

```scala
final case class Person(id: Long, firstName: String, lastName: Option[String], createdAt: java.time.LocalDateTime)

```

### Table

In order to match above table with above case class you need a `Table` object.

`Table` is the entrypoint. If you want to do anything with `skunk-tables` - you need to start with defining your `Table` object for every class you'd like to work with.
Our `Table` could be defined as following:

```scala
import cats.effect.{ IO, Resource }
import skunk.Session
import skunk.tables.*

val session: Resource[IO, Session[IO]] = ???  // Will be used later

object Person:
  val table =                     // Bear in mind, you don't want to annotate it with `Table[Person]` type as the object has many type memembers which would be erased
    Table
      .of[Person]                 // We map every field of `Person` class to a Postgres table
      .withName("persons")        // You have to specify the name manually
      .withPrimary("id")          // All string-literals are obviously type-checked. It means you cannot misspell the column name
      .withDefault("id")          // `default` means you won't have to provide it when inserting
      .withDefault("created_at")  // All column names are spelled as they are in DB, i.e. snake case
      .build                      // Transforming the `TableBuilder` into `Table`
```

At the moment you'll be mostly describing the constraints of the table. At the time of 0.0.1 release we have available: `UNIQUE`, `DEFAULT`, `PRIMARY`. All `Option` types also become "`NULLABLE`" columns.

Now you have the `table` object, which in turn has some useful methods, such as `count`, `all`, `insert` etc.
These methods represent what we believe are the most common operations one would like to perform over a Postgres table.

#### Structure of Table classess

There are few requirements that the class has to conform in order to be used for a table description.

1. It has to be a `Product` (all cases classes are)
2. It has to be non-empty
3. All members have to have `IsColumn` given instance

Let's talk about last one. Imagine the following structure:

```scala
enum Status:
  case Delivered
  case InProgress
case class Dimensions(x: Int, y: Int, z: Int)
case class Address(country: String, state: String, zipCode: String)
case class Delivery(dimensions: Dimensions, address: Address, status: Status)
```

What if we want our final `deliveries` table to have `Dimensions` as three distinct columns, but `Address` as a single `VARCHAR`?
For `Dimensions` we don't even have to do anything - all nested classess are unfolded until a "non-flattenning" type is encountered.
That's where `IsColumn` trait comes in. An `IsColumn` given states that a type should be handled as a single column and when the unfolding algorithm flattens a nested type classes this type will serve as a base case, even it's defined for a case class - it will be treated as a primitive.

```scala
val status: Codec[Status] = ???
val address: Codec[Address] = ???

inline given IsColumn[Status] = IsColumn.ofCodec(status)
inline given IsColumn[Address] = IsColumn.ofCodec(address)
```

In a nutshell, `IsColumn` brings `skunk.Codec` into implicit scope and states that the type must be treated as a single column.

### Iron

Apart from `skunk` itself and some Typelevel libraries, `skunk-tables` brings another dependency into the scope - [`iron`](https://github.com/Iltotore/iron) - an awesome Scala 3 library for refined types.

The `iron` helps `skunk-tables` to derive a proper cardinality for `VARCHAR` columns. For example, the following type:

```scala
type Title = String :| Title.Constraint
object Title:
  val Length: 256 = 256
  type Constraint = MaxLength[Length.type]
```

will result in `VARCHAR(256)`.

### Query

Most of the methods on `Table` return an object of type `Query[F, S, A]`, which is just a descrption of what should be performend and the second most common type in `skunk-tables`.
1. `F` here stands for your effect, typically `IO`
2. `A` is an unconstrained output type (e.g. `Person` if we querying the items or `Long` if we call `count`)
3. `S` (size) is one of `"many"`, `"single"` or `"optional"` (yes, they're string literals in the type).

Both `S` and `A` are dictated by a method being called on a `Table` object.
If you call `count` - you know you'll get a value, exactly one value and it's going to be `Long`, so you'll get `Query[F, "single", Long]`.
If you call `get` (get an item by a primary key) - you don't know if the key is present or not in the table, so you're ending up with `Query[F, "optional", A]`.
And finally if you call `all` - you can have zero or more, potentially even tens or thousands of objects, so it will be `Query[F, "many", A]`.

```scala
val count: Query[IO, "single", Long] = Person.table.count
session.use(count.run)
```

### Inserting

In order to insert something into the table you have to state that this something is insertable into a particular table.
In other words, you need to create a given instance:

```scala
case class NewPerson(name: String)
given CanInsert[NewPerson, Person] =      // You read it as "I can insert NewPerson into table of Persons"
  CanInsert[NewPerson]
    .into(Person.table)
    .via(columns => Tuple1(columns.firstName.from(_.name)))
```

The insertable type doesn't have to contain a member for every column, but only `DEFAULT` and non-`NULLABLE` ones.

Once you have the evidence you can insert an item:

```scala
val insert: Insert[IO, Completion] = Person.table.insert(NewPerson("Bob"))
```

Now you can run the insertion as is:

```scala
session.use(insert.run)
```

Or ask for some (perhaps auto-generated in DB) values:

```scala
val toReturn: ("id", "created_at") = ("id", "created_at")   // Need a literal type
val result: IO[(Long, java.time.LocalDateTime)] = session.use(insert.returning(toReturn).run)
```

### API

Once you know all the concepts, here are the functions available on our `Table[Person]`:

- `count: Query[F, "single", Long]` - count all rows
- `all: Query[F, "many", Person]` - stream all rows from the table
- `query(select: Select => Tuple): Query[F, "many", Person]` - select some specific rows and stream them
- `get(select: Select => Tuple): Query[F, "optional", Person]` - select a row (potentially) missing based on its primary and/or unique columns (others aren't available in `Select`)
- `insert(item: NewPerson): Insert[F, Completion]` - insert a new row

In above signatures `Select` is a special [Selectable](https://scala-lang.org/api/3.x/scala/Selectable.html) object created at compile-time and having columns as class members.
But only those columns which you really can access, e.g. in `get` you'd get only primary and unique columns.

### Low-level API

`Table` object is cool not only because it provides short-hand methods like `get` and `insert`, but also because it adds
a source of truth for the actual PostgreSQL table. You can pull out things like table name, column names, column operations
for custom complex SQL fragments:

All string literals in `skunk-tables` are compile-time checked for existence, so if you make a typo for example in a column name
it will be a compile-time error.

In order to construct a custom `Fragment` you typically want to get other entities as `Fragment`s too.
`Table` and `TypedColumn` classes have a special `low` namespace, all methods from there return only `Fragment`s (typically, `Fragment[Void]`).

```scala
val P = Person.table.low        // Just a short-hand
val age = Person.table.select.age.low

sql"""
SELECT ${P.pick("first_name", "last_name")}
FROM   ${P.name}
WHERE  ${age.eql}
"""
```

Results into:

```sql
SELECT first_name, last_name
FROM   persons
WHERE  age = ${int4}
```

It might look like not a big win at saving keystrokes, but there are few things to consider:

- Everything comes from a single source of truth, e.g. if you changed a column name in Postgres - you don't have to go over all your queries to find where it was used
- It adds compile-time safety at operations as well, e.g. only columns of type `Boolean` have `isTrue` or `isFalse` operations and only `LocalDateTime` have `currentTimestamp` operation
- In future we should be able to combine `Fragment`s coming from different sources:

```scala
Person.table.count.fragment ~ sql" WHERE ${age.eql}"
```

## Note about stablity and Roadmap

`skunk-tables` is at its 0.0.x stage. The API will change a lot and in a very breaking ways until at least 0.1.0. Everyone is welcome to explore what we can do here.

Next steps:

* Joins
* Updates
* Table creation
* Better complie-time error messages (God bless you if you make a typo on 1+ `Table` now)
* Make `Query` monadic
* Migrate to better designed ADTs for queries instead of `Fragment` mungling

## Alternatives

There are quite a bit of libraries with overlapping functionality:

- [zio-protoquill](https://github.com/zio/zio-protoquill/tree/master) is the closest in spirit. Way more reliable and featureful, written by way smarter people. In fact, in most of the cases you should use `zio-protoquill`. Perhaps, unless you don't want to bring ZIO dependency.
- [blaireau](https://github.com/valentinHenry/blaireau) is another very close analog, but written for Scala 2 and using Shapeless. Also not actively maintained.

## Copyright and license

The Skunk Tables is copyright 2023-2024 Foldables Ltd.

Licensed under the **[Apache License, Version 2.0][license]** (the "License");
you may not use this software except in compliance with the License.

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

[license-image]: http://img.shields.io/badge/license-Apache--2-blue.svg?style=flat
[license]: http://www.apache.org/licenses/LICENSE-2.0
