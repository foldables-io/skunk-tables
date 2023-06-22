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

import java.util.UUID
import java.time.LocalDateTime

import cats.implicits.*
import cats.effect.{IO, IOApp, Resource}

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.collection.*

import skunk.{Session, Codec}
import skunk.codec.`enum`.`enum`
import skunk.data.Type

import natchez.Trace.Implicits.noop

import skunk.tables.*

final case class Task(meta: Task.Meta, title: Task.Title, status: Task.Status, assignee: Option[String])

object Task:

  final case class Meta(id: UUID, createdAt: LocalDateTime)

  type Title = String :| Title.Constraint
  object Title:
    val Length: 256 = 256
    type Constraint = MaxLength[Length.type]

  enum Status:
    case Done
    case Undone

    def asString: String = this.toString.toLowerCase

  object Status:
    def fromString(s: String): Either[String, Status] =
      Status.values
        .find(r => s.toLowerCase == r.asString)
        .toRight(
          s"$s is not a valid Status. Available options: ${Status.values.map(_.toString.toLowerCase).mkString(", ")}"
        )

    def codec: Codec[Status] = `enum`[Status](_.asString, s => fromString(s).toOption, Type("task_status"))

    inline given IsColumn[Status] = IsColumn.IsColumn(Status.codec)

  val table = Table
    .of[Task]
    .withName("tasks")
    .withPrimary("id")
    .withDefault("id")
    .withDefault("created_at")
    .withUnique("title")
    .build

  final case class New(title: Task.Title, assignTo: Option[String])

  object New:
    given CanInsert[Task.New, Task] =
      CanInsert[Task.New]
        .into(Task.table)
        .via(columns =>
          (columns.title.from(_.title), columns.assignee.from(_.assignTo), columns.status.default(Status.Undone))
        )

object Main extends IOApp.Simple:
  def run =
    val tasks = List(Task.New("Write docs", Some("chuwy")), Task.New("Refactor", None), Task.New("Release 0.1.0", None))
    val last  = Task.New("Improve docs", None)

    val toReturn: ("id", "created_at") = ("id", "created_at")

    Session
      .single[IO](host = "localhost",
                  port = 5432,
                  user = "postgres",
                  database = "skunk",
                  password = Some("Supersecret1")
      )
      .use { session =>
        for
          _   <- tasks.traverse_(task => Task.table.insert[IO, Task.New](task).run(session))
          one <- Task.table.get(columns => columns.title == "Write docs").run(session)
          _   <- one.fold(IO.unit)(task => IO.println(s"${task.assignee} has to write docs"))

          idCreatedAt <- Task.table.insert[IO, Task.New](last).returning(toReturn).run(session)
          _           <- IO.println(s"A task with id ${idCreatedAt._1} was created at ${idCreatedAt._2}")

          total <- Task.table.count.run(session)
          _     <- IO.println(s"We have $total tasks now")

          _ <- IO.println("And here they are")
          _ <- Task.table.all.run(session).evalMap(task => IO.println(task)).compile.drain
        yield ()
      }
