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
