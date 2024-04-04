package skunk.tables.internal

import scala.quoted.*

import quotidian.MacroMirror

import skunk.tables.Column
import skunk.tables.Column.IsColumn


object MacroTable:

  inline transparent def builder[T] = ${ builderImpl[T] }

  private def builderImpl[T: Type](using Quotes) =
    import quotes.reflect.*

    MacroMirror.summonProduct[T] match
      case p: MacroMirror.Product[quotes.type, T] =>
        val pairs = p.elemLabels.map(label => ConstantType(StringConstant(label)).asType).zip(p.elemTypes).map:
          case ('[label], '[a]) =>
            Expr.summon[IsColumn[a]] match
              case Some(ev) =>
                '{ Column(${ev.asExprOf[IsColumn[a]]}).asInstanceOf[Column[label & String & scala.Singleton, a, EmptyTuple]] }
              case None =>
                report.errorAndAbort(s"Oh no! There's no IsColumn[${TypeRepr.of[a].show}]")

        Expr.ofTupleFromSeq(pairs)
