import cats.data.NonEmptyList

type TODO = Any

/** [ FROM from_item [, ...] ] */
final case class FromItem(items: NonEmptyList[String])

enum Disctinct:
  /** Default, not disctinct */
  case All
  /** `DISTINCT` */
  case Distinct
  /** `DISTINCT ON` */
  case DistinctOn(expression: TODO)

enum What:
  case Asterisk
  case Expression(exp: TODO)

final case class Condition(item: TODO)

final case class Select(distinct: Disctinct, what: What, from: Option[FromItem], where: Option[Condition], having: Option[Condition])
