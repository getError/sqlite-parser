package cn.geterror.sqlite.parser

import cn.geterror.sqlite.expr._

object ExprTest extends App{
  val e1 = BinaryExpr("+",LiteralExpr("a"),LiteralExpr("b"))
  val e2 = BinaryExpr("+",LiteralExpr("a"),LiteralExpr("b"))
  private val bool: Boolean = e1.equals(e2)
  bool
}
