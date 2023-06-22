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

import scala.compiletime.erasedValue

import cats.Monad
import cats.implicits.*

import fs2.Stream

import skunk.{ Query => SkunkQuery, * }
import skunk.implicits.*
import skunk.codec.numeric.int8


trait Query[F[_], S <: Query.Size, O] extends Action[F, S, O]:
  self =>

  /** Decoder for the output */
  def decoder: Decoder[O]

  inline def run(session: Session[F])(using Monad[F]): Query.Out[F, S, O] =
    inline erasedValue[S] match
      case _: "single"   =>
        session
          .prepare(fragment.query(decoder))
          .flatMap(prepared => prepared.unique(input))
      case _: "optional" => 
        session
          .prepare(statement)
          .flatMap(prepared => prepared.option(input))
      case _: "many"     =>
        Stream
          .eval(session.prepare(statement))
          .flatMap(prepared => prepared.stream(input, 4096))

  def statement: SkunkQuery[Input, O] =
    buildFragment.query(decoder)

  def buildFragment: Fragment[Input] =
    val lim = getLimit.fold(Fragment.empty)(l => sql" LIMIT #${l.toString}")
    val off = getOffset.fold(Fragment.empty)(l => sql" OFFSET #${l.toString}")
    fragment <~ lim <~ off

  def getLimit: Option[Long] = None

  def limit(using S =:= "many")(l: Long): Query[F, "many", O] =
    new Query[F, "many", O]:
      type Input = self.Input
      def input = self.input
      def decoder = self.decoder
      def fragment: Fragment[Input] = self.fragment
      override def getLimit: Option[Long] = Some(l)

  def getOffset: Option[Long] = None

  def offset(using S =:= "many")(o: Long): Query[F, "many", O] =
    new Query[F, "many", O]:
      type Input = self.Input
      def input = self.input
      def decoder = self.decoder
      def fragment: Fragment[Input] = self.fragment
      override def getOffset: Option[Long] = Some(o)
    
object Query:
  /**
   * Every query is statically known to return exactly-one, at-most-one or zero-or-more elements
   * `Query.Size` is used to parametrize all `Query` objects
   */
  type Size = "single" | "optional" | "many"

  type Out[F[_], S <: Size, O] = S match
    case "single"   => F[O]
    case "optional" => F[Option[O]]
    case "many"     => Stream[F, O]

  def count[F[_]](table: Table.Name): Query[F, "single", Long] = 
    new Query[F, "single", Long]:
      type Input = Void
      val input = Void
      val decoder = int8
      def fragment: Fragment[Void] = sql"SELECT COUNT(*) FROM ${table.toFragment}"

  def select[F[_], A, T](table: Table.Name, names: List[String], ops: TypedColumn.Op[A], aDecoder: Decoder[T]): Query[F, "many", T] = 
    val selectFragment = sql"#${names.mkString(", ")}"
    new Query[F, "many", T]:
      type Input = A
      val input = ops.a
      val decoder = aDecoder
      def fragment: Fragment[A] = sql"SELECT ${selectFragment} FROM ${table.toFragment} WHERE ${ops.fragment}"

  def get[F[_], A, T](table: Table.Name, names: List[String], ops: TypedColumn.Op[A], aDecoder: Decoder[T]): Query[F, "optional", T] = 
    val selectFragment = sql"#${names.mkString(", ")}"
    new Query[F, "optional", T]:
      type Input = A
      val input = ops.a
      val decoder = aDecoder
      def fragment: Fragment[A] = sql"SELECT ${selectFragment} FROM ${table.toFragment} WHERE ${ops.fragment}"

  def all[F[_], A](table: Table.Name, names: List[String], aDecoder: Decoder[A]): Query[F, "many", A] { type Input = Void } =
    val selectFragment = sql"#${names.mkString(", ")}"
    new Query[F, "many", A]:
      type Input = Void
      val input = Void
      val decoder = aDecoder
      def fragment: Fragment[Void] = sql"SELECT ${selectFragment} FROM ${table.toFragment}"

