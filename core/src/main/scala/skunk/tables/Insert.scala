package skunk.tables

import skunk.{ Query => _, * }
import skunk.data.Completion
import skunk.implicits.*

/**
 * `Insert` is an `Action` with `INSERT INTO` statement underneath
 * It returns no values as is (just `Completion`), but can be transformed
 * into a `Query` if `RETURNING` clause is added
 */
sealed trait Insert[F[_], A] extends Action[F, "none", Completion]:
  self =>

  def run(session: Session[F]): F[Completion] =
    session
      .execute[Input](fragment.command)(input)

  /** All original table columns */
  type Columns <: NonEmptyTuple

  /**
   * Add `RETURNING` clause to transform this `Insert` into `Query`
   *
   * @param columns a tuple of literals, matching the column names
   * @param ft given evidence that `columns` are present in the original table
   * @return plain `Query` with `single` cardinality
   */
  transparent inline def returning[RL <: NonEmptyTuple](columns: RL)(using ft: FromTable[Columns, RL]) =
    new Query[F, "single", ft.Out]:
      type Input = self.Input
      val input = self.input
      val decoder = ft.decoder
      // I don't know why `Fragment` interpolation needs casting when the method is transparent
      val fragment: Fragment[Input] =
        self.fragment <~ sql" RETURNING ${ft.columnsFragment}".asInstanceOf[Fragment[Void]]


object Insert:

  def insert[F[_], T, A, C <: NonEmptyTuple](table: Table.Name, a: A, ci: CanInsert[A, T]) =
    new Insert[F, A]:
      type Columns = C
      type Input = ci.Twiddled

      def input: Input =
        if (table.toString == "inventory_lists") println(s"Transforming ${ci} with $a")
        ci.transform(a)
      def fragment: Fragment[Input] =
        sql"INSERT INTO ${table.toFragment} (${ci.columnsFragment}) VALUES (${ci.encoder})"

