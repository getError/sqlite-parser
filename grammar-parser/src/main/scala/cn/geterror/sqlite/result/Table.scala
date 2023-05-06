package cn.geterror.sqlite.result

import cn.geterror.sqlite.tree.TreeNode

object JoinType extends Enumeration {
  val Inner = Value("INNER")
  val Left = Value("LEFT")
  val Right = Value("RIGHT")
  val Full = Value("FULL")
  val Unknown = Value("UNKNOWN")

}

sealed abstract class Table extends TreeNode[Table]{
  val alias:Option[String]
  val children:Seq[Table]
  val isResolved:Boolean
}

sealed abstract class UnresolvedTable extends Table{
  override val isResolved: Boolean = false
}
sealed abstract class ResolvedTable extends Table{
  override val isResolved: Boolean = true
  val fields: Seq[ResolvedProjection]
}


case class SimpleTable(override val alias: Option[String],
                       tableName:String) extends UnresolvedTable{
  override val children: Seq[Table] = Seq()// SimpleTable没有来源表，是根源表的一种

}

case class ResolvedSimpleTable(aliasName:Option[String],
                       tableName:String, override val fields: Seq[FactFieldProjection]) extends ResolvedTable{
  override val children: Seq[Table] = Seq()// SimpleTable没有来源表，是根源表的一种
  override val alias: Option[String] = Option(aliasName.getOrElse(tableName))
}


case class FactTable(schema:Option[String],
                     tableName:String,
                     override val fields: Seq[FactFieldProjection]
                    ) extends ResolvedTable {
  override val children: Seq[Table] = Seq()// FactTable从解析建表语句得来，没有来源表
  override val alias: Option[String] = Some(tableName)
}

case class SubQuery(override val alias: Option[String],
                    override val children: Seq[Table],
                    parserResult: ParserResult) extends UnresolvedTable{
  def table:Table = children.head
  def projection:Seq[Projection] = parserResult.projections
}

case class ResolvedSubQuery(override val alias: Option[String],
                            override val children: Seq[Table],
                            result: ParserResult)extends ResolvedTable{
  override val fields:Seq[ResolvedProjection] = result.projections.map(_.asInstanceOf[ResolvedProjection])
}

case class JoinTable(override val alias: Option[String],
                     override val children: Seq[Table],
                     joinType:JoinType.Value = JoinType.Unknown) extends UnresolvedTable{}

case class ResolvedJoinTable(override val alias: Option[String],
                             override val children: Seq[ResolvedTable],
                             joinType:JoinType.Value = JoinType.Unknown,
                             override val fields: Seq[ResolvedProjection])extends ResolvedTable{}



