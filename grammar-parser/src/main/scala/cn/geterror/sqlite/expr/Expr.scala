package cn.geterror.sqlite.expr

import cn.geterror.sqlite.result.ParserResult
import cn.geterror.sqlite.tree.TreeNode

abstract class Expr extends TreeNode[Expr]{
  override def children:Seq[Expr]
  def toDsl:String
}

sealed abstract class ColumnResultExpr extends Expr {
  override def children: Seq[Expr] = Seq()
  override def toDsl: String = s"${schema.map(s => s"$s.").getOrElse("")}${table.map(t => s"$t.").getOrElse("")}$column"
  val schema:Option[String]
  val table: Option[String]
  val column: String
}

trait BooleanExpr extends Expr

case class LiteralExpr(literal: String) extends Expr{
  def children: Seq[Expr] = Seq.empty
  def toDsl: String = literal

  override def equals(obj: Any): Boolean = {
    obj match {
      case e:LiteralExpr => e.literal == literal
      case _ => false
    }
  }
}

case class BindParamExpr(bindParam: String) extends Expr{
  def children: Seq[Expr] = Seq.empty
  def toDsl: String = bindParam
  override def equals(obj: Any): Boolean = {
    obj match {
      case e:BindParamExpr => e.bindParam == bindParam
      case _ => false
    }
  }
}

case class ExtractExpr(schema: Option[String], table:Option[String], column:String) extends Expr{
  def children: Seq[Expr] = Seq.empty
  def toDsl: String = {
    val schemaStr = schema.map(s => s"$s.").getOrElse("")
    val tableStr = table.map(t => s"$t.").getOrElse("")
    s"${schemaStr}${tableStr}$column"
  }
  override def equals(obj: Any): Boolean = {
    obj match {
      case e:ExtractExpr => e.schema == schema && e.table == table && e.column == column
      case _ => false
    }
  }
}

case class StarExpr() extends ColumnResultExpr{
  override val column: String = "*"
  override val schema: Option[String] = None
  override val table: Option[String] = None
  override def equals(obj: Any): Boolean = {
    obj match {
      case e:StarExpr => true
      case _ => false
    }
  }
}

case class TableStarExpr(override val table: Option[String]) extends ColumnResultExpr{
  override val column: String = "*"
  override val schema: Option[String] = None
  override def equals(obj: Any): Boolean = {
    obj match {
      case e:TableStarExpr => e.table == table
      case _ => false
    }
  }
}

case class SchemaTableStarExpr(override val schema:Option[String],
                               override val table: Option[String]) extends ColumnResultExpr{
  override val column: String = "*"
  override def equals(obj: Any): Boolean = {
    obj match {
      case e:SchemaTableStarExpr => e.schema == schema && e.table == table
      case _ => false
    }
  }
}

case class ColumnExpr(schema:Option[String], table: Option[String],column: String) extends ColumnResultExpr{
  override def equals(obj: Any): Boolean = {
    obj match {
      case e:ColumnExpr => e.schema == schema && e.table == table && e.column == column
      case _ => false
    }
  }
}

case class UnaryExpr(op: String, expr: Expr) extends Expr{
  def children: Seq[Expr] = Seq(expr)
  def toDsl: String = s"$op(${expr.toDsl}"
  override def equals(obj: Any): Boolean = {
    obj match {
      case e:UnaryExpr => e.op == op && expr.equals(e.expr)
      case _ => false
    }
  }
}

case class BinaryExpr(op: String, left: Expr, right: Expr) extends Expr{
  def children: Seq[Expr] = Seq(left,right)
  def toDsl: String = s"${left.toDsl} $op ${right.toDsl}"
  override def equals(obj: Any): Boolean = {
    obj match {
      case e:BinaryExpr => e.op == op && left.equals(e.left) && right.equals(e.right)
      case _ => false
    }
  }
}

case class TernaryExpr(op1: String,op2:String, left: Expr, middle: Expr, right: Expr) extends Expr{
  def children: Seq[Expr] = Seq(left,middle,right)
  def toDsl: String = s"${left.toDsl} $op1 ${middle.toDsl} $op2 ${right.toDsl}"
  override def equals(obj: Any): Boolean = {
    obj match {
      case e:TernaryExpr => e.op1 == op1 && e.op2 == op2 && left.equals(e.left) && middle.equals(e.middle) && right.equals(e.right)
      case _ => false
    }
  }
}

case class FunctionExpr(function: String,distinct: Boolean, args: Seq[Expr]) extends Expr{
  def children: Seq[Expr] = args
  def toDsl: String = {
    val argStr = args.map(a => a.toDsl).mkString(",")
    val distinctStr = if(distinct) "distinct " else ""
    s"$function($distinctStr$argStr)"
  }
  override def equals(obj: Any): Boolean = {
    obj match {
      case e:FunctionExpr => e.function == function && e.distinct == distinct && args.equals(e.args)
      case _ => false
    }
  }
}

case class TupleExpr(args: Seq[Expr]) extends Expr{
  def children: Seq[Expr] = args
  def toDsl: String = {
    val argStr = args.map(a => a.toDsl).mkString(",")
    s"($argStr)"
  }
  override def equals(obj: Any): Boolean = {
    obj match {
      case e:TupleExpr => ExprDriver.seqEquals(args,e.args)

      case _ => false
    }
  }
}

case class CastExpr(expr: Expr, typeName: String) extends Expr{
  def children: Seq[Expr] = Seq(expr)
  def toDsl: String = s"cast(${expr.toDsl} as $typeName)"
  override def equals(obj: Any): Boolean = {
    obj match {
      case e:CastExpr => expr.equals(e.expr) && typeName == e.typeName
      case _ => false
    }
  }
}
case class WhenExpr(condition:Expr,thenExpr:Expr) extends Expr{
  override def children: Seq[Expr] = Seq(condition,thenExpr)

  override def toDsl: String = s"when ${condition.toDsl} then ${thenExpr.toDsl}"

  override def equals(obj: Any): Boolean = {
    obj match {
      case e:WhenExpr =>
        e.condition.equals(condition)&&e.thenExpr.equals(thenExpr)
      case _ => false
    }
  }
}

case class CaseExpr(caseExpr:Option[Expr], whens: Seq[WhenExpr], elseExpr: Option[Expr]) extends Expr{
  def children: Seq[Expr] = whens ++ caseExpr ++ elseExpr
  def toDsl: String = {
    val whenStr = whens.map(_.toDsl).mkString(" ")
    val elseStr = elseExpr.map(e => s"else ${e.toDsl}").getOrElse("")
    val caseStr = caseExpr.map(e => s"${e.toDsl}").getOrElse("")
    s"case $caseStr $whenStr $elseStr"
  }
  override def equals(obj: Any): Boolean = {
    obj match {
      case e:CaseExpr =>
        e.caseExpr == caseExpr &&
        e.elseExpr == elseExpr &&ExprDriver.seqEquals(e.whens,whens)
      case _ => false
    }
  }
}

case class SubQueryExpr(query: ParserResult) extends Expr{
  def children: Seq[Expr] = Seq.empty
  def toDsl: String = "子查询暂不支持"
  override def equals(obj: Any): Boolean = {
    obj match {
      case e:SubQueryExpr => query.equals(e.query)
      case _ => false
    }
  }
}


