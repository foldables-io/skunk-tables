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

package skunk.tables.internal

import scala.runtime.Tuples.fromProduct
import scala.quoted.*

import quotidian.MacroMirror

import skunk.tables.IsColumn

abstract sealed class MacroDissect[Q <: Quotes & Singleton](val quotes: Q):
  /** `TypeRepr` of input type */
  def in: quotes.reflect.TypeRepr

  /** `TypeRepr` of output types, recursively destructured */
  def out: List[quotes.reflect.TypeRepr]

  /** `TypeRepr` of output types, NOT recursively destructured */
  def outTupled: List[quotes.reflect.TypeRepr]

  /** A function that can transform `in` to `out` */
  def destruct: quotes.reflect.Term

  /** A function that can transform `out` to `in` */
  def construct: quotes.reflect.Term

  /** A "twiddled" version of `out`, where items grouped in paris, e.g.
    * (((1,2),3),4)
    */
  def twiddled: quotes.reflect.TypeRepr

  /** A function to transform `out` into `twiddled` */
  def twiddle: quotes.reflect.Term

  /** A function to transform `twiddled` into `out` */
  def untwiddle: quotes.reflect.Term

  def arity: Int = outTupled.length

  /** `out` as `Type` instead of `TypeRepr` */
  def outType = quotes.reflect.defn
    .TupleClass(out.length)
    .typeRef
    .dealias
    .appliedTo(out)
    .asType

object MacroDissect:

  /** A `Product` type that only has primtivie types in it, i.e. no nesting */
  abstract class Leaf[Q <: Quotes & Singleton](override val quotes: Q) extends MacroDissect[Q](quotes):
    def outTupled: List[quotes.reflect.TypeRepr] = out
    override def toString: String =
      s"Leaf(${in.show}, (${out.map(_.show).mkString(", ")}))"

  /** A `Product` type that contains primtive types, leafs and other branches */
  abstract class Branch[Q <: Quotes & Singleton](override val quotes: Q) extends MacroDissect[Q](quotes):
    /** Unlike `construct` this `Term` represents real constructor, not
      * destructured, i.e. transforms `outTupled` to `in`
      */
    def constructTupled: quotes.reflect.Term

    /** Tree-structured metadata for every element of `out` */
    def nested: List[Option[MacroDissect[quotes.type]]]
    override def toString: String =
      s"Branch(${in.show}, (${out.map(_.show).mkString(", ")}), $nested)"

  private final case class Accumulator(position: Int, transforms: List[Expr[Tuple => Tuple]]):
    def next(t: Expr[Tuple => Tuple]): Accumulator =
      Accumulator(position + 1, t :: transforms)
    def next(ts: List[Expr[Tuple => Tuple]]): Accumulator =
      Accumulator(position + 1, ts ++ transforms)

  def build[T: Type](using quotes: Quotes): MacroDissect[quotes.type] =
    import quotes.reflect.*

    MacroMirror.summon[T] match
      case Some(pm: MacroMirror.ProductMacroMirror[quotes.type, T]) =>
        val nested = pm.elemTypes.map { case '[tpe] =>
          Expr.summon[IsColumn[tpe]] match
            case Some(_) => None
            case None    => Some(build[tpe])
        }

        if nested.exists(_.isDefined) then
          buildBranch[T](pm)(nested.asInstanceOf[List[Option[MacroDissect[quotes.type]]]])
        else buildLeaf[T](pm)

      case _ =>
        report.errorAndAbort(
          s"Type ${TypeRepr.of[T].widen.show} is not a Product. Dissect can be built only for Products"
        )

  def buildLeaf[T: Type](using quote: Quotes)(pm: MacroMirror[quotes.type, T]) =
    import quotes.reflect.*

    new MacroDissect.Leaf[quotes.type](quotes):
      val in  = pm.monoType
      val out = pm.elemTypeReprs
      val destruct =
        '{ (t: T) => fromProduct(t.asInstanceOf[Product]) }.asTerm
      val construct =
        // This is a hacky way to say that `Tuple1[A]` constructor is not `A => Tuple[A]`, but `identity`
        // because we always work with tuples
        if in.typeSymbol == Symbol.classSymbol("scala.Tuple1") then
          Ref(Symbol.requiredMethod("scala.Predef.identity"))
            .appliedToType(TypeRepr.of[Tuple1].appliedTo(out.head))
            .etaExpand(Symbol.spliceOwner)
        else
          val ctor = Constructor.apply(in).etaExpand(Symbol.spliceOwner)
          if out.length > 1 then Select.unique(ctor, "tupled") else ctor

      def twiddled =
        out match
          case Nil =>
            report.errorAndAbort("Invalid state. Dissected type cannot be empty Product")
          case a :: Nil =>
            TypeRepr.of[Tuple1].appliedTo(a)
          case a :: b :: Nil =>
            TypeRepr.of[Tuple2].appliedTo(List(a, b))
          case a :: b :: tail =>
            val init = TypeRepr.of[Tuple2].appliedTo(List(a, b))
            tail.foldLeft(init) { (acc, tpe) =>
              TypeRepr.of[Tuple2].appliedTo(List(acc, tpe))
            }

      def twiddle: quotes.reflect.Term =
        val f = '{ (input: Tuple) =>
          input.toArray.toList match
            case a :: Nil =>
              a *: EmptyTuple
            case a :: b :: Nil =>
              (a, b)
            case a :: b :: tail =>
              val init = (a, b)
              tail.foldLeft(init)((acc, tpe) => (acc, tpe))
            case Nil =>
              throw new IllegalStateException("Dissected product cannot be empty")
        }

        f.asTerm

      def untwiddle: quotes.reflect.Term =
        val f = '{ (input: Tuple) =>
          def go(i: List[Object]): List[Any] =
            i match
              case List(a, c) =>
                (if a.isInstanceOf[Tuple] then go(a.asInstanceOf[Tuple].toList.asInstanceOf[List[Object]])
                 else List(a)) ++ List(c)
              case a :: Nil =>
                List(a)
              case Nil =>
                Nil
              case _ =>
                throw new IllegalStateException("Twiddled list is a nested pair of pairs")

          Tuple.fromArray(go(input.toList.asInstanceOf[List[Object]]).toArray)
        }

        f.asTerm

  /** Every branch has only one level of nested dissects
    */
  def buildBranch[T: Type]
    (using quote: Quotes)
    (pm: MacroMirror[quotes.type, T])
    (nestedDissects: List[Option[MacroDissect[quotes.type]]]) =
    import quotes.reflect.*

    new MacroDissect.Branch[quotes.type](quote):
      val nested = nestedDissects

      val in = pm.monoType
      val out = pm.elemTypeReprs.zip(nested).flatMap {
        case (tpr, None)       => List(tpr)
        case (_, Some(unwrap)) => unwrap.out
      }

      val outTupled: List[quotes.reflect.TypeRepr] = pm.elemTypeReprs

      val destruct =
        // A list of functions that either destruct the nested Product if it
        // has matching `Unwrap` OR return the value as is if `Unwrap` is missing
        val functions = nested.map {
          case Some(dissect) => dissect.destruct.asExprOf[Nothing => Tuple]
          case None          => '{ (input: Any) => input }
        }

        val maps = Expr.ofTupleFromSeq(functions)

        val destructExpr: Expr[T => Tuple] =
          '{ (t: T) =>
            Tuple.fromArray(
              fromProduct(t.asInstanceOf[Product])
                .zip($maps)
                .toArray
                .flatMap(applyDestructure)
            )
          }

        destructExpr.asTerm

      val constructTupled =
        val ctor = Constructor.apply(in).etaExpand(Symbol.spliceOwner)
        if out.length > 1 then Select.unique(ctor, "tupled") else ctor

      def construct: quotes.reflect.Term =
        val functionsExpr = Expr.ofList(flatten(0, nested).transforms.reverse)

        '{ (tuple: Tuple) =>
          val unwrapped = $functionsExpr.foldLeft(tuple) { (acc, f) =>
            f.asInstanceOf[Any => Tuple](acc)
          }
          val fromTuple = ${
            constructTupled.etaExpand(Symbol.spliceOwner).asExpr
          }
          fromTuple.asInstanceOf[Tuple => Any].apply(unwrapped)
        }.asTerm

      def twiddled =
        out match
          case Nil =>
            report.errorAndAbort("Invalid state. Dissected type cannot be empty Product")
          case a :: Nil =>
            TypeRepr.of[Tuple1].appliedTo(a)
          case a :: b :: Nil =>
            TypeRepr.of[Tuple2].appliedTo(List(a, b))
          case a :: b :: tail =>
            val init = TypeRepr.of[Tuple2].appliedTo(List(a, b))
            tail.foldLeft(init) { (acc, tpe) =>
              TypeRepr.of[Tuple2].appliedTo(List(acc, tpe))
            }

      def twiddle: quotes.reflect.Term =
        val f = '{ (input: Tuple) =>
          input.toArray.toList match
            case a :: Nil =>
              a *: EmptyTuple
            case a :: b :: Nil =>
              (a, b)
            case a :: b :: tail =>
              val init = (a, b)
              tail.foldLeft(init)((acc, tpe) => (acc, tpe))
            case Nil =>
              throw new IllegalStateException("Dissected product cannot be empty")
        }

        f.asTerm

      def untwiddle: quotes.reflect.Term =
        val f = '{ (input: Tuple) =>
          def go(i: List[Object]): List[Any] =
            i match
              case List(a, c) =>
                (if a.isInstanceOf[Tuple] then go(a.asInstanceOf[Tuple].toList.asInstanceOf[List[Object]])
                 else List(a)) ++ List(c)
              case a :: Nil =>
                List(a)
              case Nil =>
                Nil
              case _ =>
                throw new IllegalStateException("Twiddled list is a nested pair of pairs")

          Tuple.fromArray(go(input.toList.asInstanceOf[List[Object]]).toArray)
        }

        f.asTerm

  /** A function producing a list of functions that if applied sequentially, can
    * transform a flat tuple of `(1,2,3,4,5,6)` into a nested/tree structure of
    * original hierarchy, `A(1, B(2,3), C(4, D(5,6)))`.
    *
    * Its flow depends on [[nested]] to decide what transformation to apply to
    * every element. It has three cases:
    *   - If element is primtive (no `Dissect`) - apply no transformation, so
    *     `(1,2,3)` remains `(1,2,3)`
    *   - If element is a `Leaf` (a `Product` that itself contains only
    *     primitives) - apply a constructor to `n` element (`n` = arity) and
    *     drop `n` elements, so `(1,2,3)` turns into `(1,A(2,3))`
    *   - If element if a `Branch` (a `Product` with `Leaf`s/primtives) - first
    *     recurse into its `nested` structure and flatten everything there, then
    *     once it's ready - apply `Branch`'s constructor
    */
  private def flatten(using quote: Quotes)(drop: Int, nested: List[Option[MacroDissect[?]]]): Accumulator =
    nested.foldLeft(Accumulator(drop, Nil)) {
      case (accumulator, None) =>
        val step = '{ (input: Tuple) => input }

        accumulator.next(step)

      case (accumulator, Some(leaf: MacroDissect.Leaf[?])) =>
        val step =
          mkStep(accumulator.position, leaf.arity, leaf.construct.asExpr)

        accumulator.next(step)

      case (accumulator, Some(branch: MacroDissect.Branch[?])) =>
        val inner = flatten(accumulator.position, branch.nested)
        val step  = mkStep(accumulator.position, branch.arity, branch.constructTupled.asExpr)

        accumulator.next(step :: inner.transforms)
    }

  def mkStep(using quote: Quotes)(position: Int, arity: Int, constructor: Expr[Any]): Expr[Tuple => Tuple] =
    val arityExpr    = Expr(arity)
    val positionExpr = Expr(position)

    '{ (input: Tuple) =>
      val beginning = input.drop($positionExpr).asInstanceOf[Tuple]
      val values =
        (if $arityExpr == 1 then beginning.asInstanceOf[NonEmptyTuple].head
         else beginning.take($arityExpr)).asInstanceOf[Any]
      val result = $constructor.asInstanceOf[Any => Any].apply(values)
      (input.take($positionExpr).asInstanceOf[Tuple] :* result) ++ input
        .drop($positionExpr + $arityExpr)
        .asInstanceOf[Tuple]
    }

  private def applyDestructure(any: Object) =
    val (t, f) = any.asInstanceOf[(Any, Any => Any)]
    f(t) match
      case tuple: Tuple => tuple.toArray
      case other        => Array(other)
