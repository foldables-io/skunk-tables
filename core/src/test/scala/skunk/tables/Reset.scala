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

import cats.implicits.*
import cats.effect.{ IO, Resource }

import fs2.Stream
import fs2.io.file.{ Path, Files }

import natchez.Trace.Implicits.noop

import skunk.Session
import skunk.implicits.*
import skunk.util.Typer

object Reset:
  val resetFile = getClass.getResource("/reset.sql").getFile
  val initial = getClass.getResource("/prepare.sql").getFile

  /** Prepare a database in one session, then create another for query execution */
  def getClean =
    for {
      r <- Reset.build.flatten
      _ <- Resource.make(Reset.runReset(r) *> Reset.runPrepare(r))(_ => Reset.runReset(r))
    } yield r

  def runReset(pg: Session[IO]) =
    Files[IO]
      .readUtf8Lines(Path(resetFile))
      .filter(s => !s.isBlank)
      .evalTap { line => pg.execute(sql"""#$line""".command) }
      .compile
      .drain

  def runPrepare(pg: Session[IO]) =
    Files[IO]
      .readUtf8Lines(Path(initial))
      .filter(s => !s.startsWith("--"))
      .fold(("", List.empty[String])) {
        case ((_, sqls), string) if string.startsWith("CREATE") && string.endsWith(";") =>
          ("", dropComment(string) :: sqls)
        case ((acc, sqls), string) if string.startsWith("CREATE") =>
          (acc + "\n" + string, sqls)
        case ((acc, sqls), string) if string.endsWith(";") =>
          ("", acc + "\n" + dropComment(string) :: sqls)
        case ((acc, sqls), string) =>
          (acc + "\n" + string, sqls)
      }
      .flatMap { (_, sqls) => Stream.emits(sqls.reverse) }
      .evalTap { sql => pg.execute(sql"""#$sql""".command) }
      .compile
      .drain
  
  def dropComment(s: String): String =
    val idx = s.indexOfSlice("-- ")
    if (idx == -1) s
    else s.take(idx)

  def build =
    Session.pooled[IO](
      host     = "localhost",
      port     = 5432,
      user     = "postgres",
      database = "ratio",
      password = Some("Supersecret1"),
      strategy = Typer.Strategy.SearchPath,
      max      = 2
    )
