package skunk.tables

import skunk.Fragment

/**
  * Descendants of this trait represnet the last stage of a query,
  * ready to be executed in Skunk session. All these actions need to have
  * at least some input (a type, its value and a `Fragment`).
  *
  * The descendants however can be seen as a combination of all necessary query components:
  * - `Fragment` (to prepare SQL, coming from `Action`)
  * - user-provided values (to fulfill prepared SQL, also coming from `Action`)
  * - `Query` and `Decoder` (to decode output, coming from `Query` descendant and present only there)
  * - statically known info about cardinality of the query
  *
  * Typically, `Action` contains some kind of a `Query` (like `SELECT`)
  * underneath, but also can be an `Command` (like `INSERT`).
  *
  * @tparam F - an effect, such as `IO`
  * @tparam S - special `Size` label, telling user how many items
  *             the query would return, as we know this information statically
  * @tparam O - returned output value of the action
  */
private[tables] trait Action[F[_], S <: Action.Size, O]:
  /**
   * Input is whatever provided via `Fragment`, i.e. a Twiddle list
   * For `SELECT` it will be whatever values provided for `WHERE`
   * for `INSERT` it will be whatever provided for `VALUES`
   * for static tables (`COUNT`, `SELECT *`) it will be `Void`
   *
   * It's hidden as type member because user-generally doesn't care
   * what was passed in to construct the query, unlike `O` which is
   * going to be an output type
   */
  type Input

  /** The actual user-provided values as Twiddle list */
  def input: Input

  /** The original fragment. All descendants only prepend/append SQL to it */
  def fragment: Fragment[Input]

object Action:

  type Size = Query.Size | "none"

