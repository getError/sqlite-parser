package cn.geterror.sqlite.result

import cn.geterror.sqlite.expr._

/**
 * 字段投影，展示源表某字段到目标表的过程
 */
sealed abstract class Projection
{
  val expr:Option[Expr] = None
  val alias:Option[String] = None
  val resolved:Boolean
}

abstract class UnresolvedProjection extends Projection{
  override val resolved = false
}


abstract class ResolvedProjection extends Projection{
  val factTables: Seq[String]
  override val resolved = true
}

case class UnresolvedStarProjection() extends UnresolvedProjection{
  override val expr: Option[Expr] = Option(StarExpr())
}

case class UnresolvedTableStarProjection(tableName:String) extends UnresolvedProjection{
  override val expr: Option[Expr] = Option(TableStarExpr(Option(tableName)))
}

case class UnresolvedFieldProjection(override val expr:Option[Expr],
                                     override val alias:Option[String]) extends UnresolvedProjection{
}

case class ResolvedFieldProjection(override val expr:Option[Expr],
                                    override val alias:Option[String],
                                   override val factTables: Seq[String]) extends ResolvedProjection{
}

case class FactFieldProjection(factTable: String, fieldName:String, fieldType: String) extends ResolvedProjection{
  override val factTables: Seq[String] = Seq(factTable)
  override val expr: Option[Expr] = Some(ColumnExpr(None,Some(factTable),fieldName))
  override val alias: Option[String] = Some(fieldName)
}



case class JoinedFieldProjection(fromTable: Table,
                                 override val alias:Option[String],
                                 factTables: Seq[String],
                                 override val expr: Option[Expr]) extends ResolvedProjection{

  def factTable: Option[String] = factTables.find(_=>true)
}

case class StarFieldProjection(fromTable: Table,
                               override val alias:Option[String],
                               factTables: Seq[String],
                               override val expr: Option[Expr]) extends ResolvedProjection{

  def factTable: Option[String] = factTables.find(_=>true)
}