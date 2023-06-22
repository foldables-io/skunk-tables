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

import cats.data.NonEmptyList

// TODO: not used yet, all column names are snake case
enum NameStrategy:
  case Snake
  case FullSnake
  case Camel
  case FullCamel

  def transform(path: NonEmptyList[String]): String =
    this match
      case Snake =>
        NameStrategy.snakeCase(path.last)
      case FullSnake =>
        path.toList.map(NameStrategy.snakeCase).mkString("_")
      case Camel =>
        path.last
      case FullCamel =>
        path match
          case NonEmptyList(head, tail) =>
            (head :: tail.map(_.capitalize)).mkString("")

object NameStrategy:
  def snakeCase(str: String): String =
    str
      .replaceAll("([A-Z]+)([A-Z][a-z])", "$1_$2")
      .replaceAll("([a-z\\d])([A-Z])", "$1_$2")
      .toLowerCase
