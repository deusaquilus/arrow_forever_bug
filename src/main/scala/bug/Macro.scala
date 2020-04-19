package bug

import scala.quoted._

object Macro {
  inline def mac(tree: Any): String = ${ macImpl('tree) }
  def macImpl(tree: Expr[Any])(using qctx: QuoteContext): Expr[String] = {
    import qctx.tasty.{given _, _}

    tree.unseal.underlyingArgument.seal match {
        case '{ ($in: $tpe1) => ($out: $tpe2) } => Expr(out.toString)
        case _ => Expr("not matched")
    }
  }
}
