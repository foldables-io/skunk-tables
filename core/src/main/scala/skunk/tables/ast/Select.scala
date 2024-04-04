package skunk.tables.ast

import cats.data.NonEmptyList
import skunk.tables.ast.Select.*

final case class Select(toSelect: NonEmptyList[ToSelect],
                        distinct: Boolean,
                        from: Option[String],
                        where: Option[ConditionTree],
                        limit: Option[Int],
                        orderBy: Option[OrderBy])

object Select:
  trait SelectH:
    type ToSelect <: NonEmptyTuple
    type Distinct
    type From
    type Where
    type Limit
    type OrderBy

  type ColumnName = String
  type Value = String

  enum ToSelect:
    case Constant(value: Value)
    case Column(name: ColumnName)
    case Function(name: String, toColumn: Option[ColumnName])

  trait ConstantH:
    type Value
  trait ColumnH:
    type Name
  trait FunctionH:
    type Name
    type ToColumn


  enum ConditionTree:
    case Leaf(a: Condition)
    case Not(a: ConditionTree)
    case And(a: ConditionTree, b: ConditionTree)
    case Or(a: ConditionTree, b: ConditionTree)

  final case class OrderBy(columns: NonEmptyList[ColumnName], desc: Boolean)

  enum Operator:
    case LessThan
    case GreaterThan
    case LessThanOrEqualTo
    case GreaterThanOrEqualTo
    case Equal
    case NotEqual

  enum Condition:
    case IsNull(column: ColumnName)
    case IsNotNull(column: ColumnName)
    case Op(column: ColumnName, op: Operator, value: Value)


