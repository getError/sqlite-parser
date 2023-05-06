package cn.geterror.sqlite.analyzer

import cn.geterror.sqlite.exception.SqlResolveException
import cn.geterror.sqlite.expr.ColumnExpr
import cn.geterror.sqlite.result._

class ProjectionAnalyzer(currentTables:Seq[ResolvedTable], factTable: Seq[FactTable]) {
  val exprAnalyzer:ExprAnalyzer = new ExprAnalyzer(currentTables, factTable)



  // 拼接字段加工逻辑 对字段依次有y=f(x) y=g(x) 则处理为 y=f(g(x))
  def dealFieldProjection(f:UnresolvedFieldProjection):ResolvedProjection = {
    val expr = f.expr.get
    val resolvedExpr = exprAnalyzer.resolvedExpr(expr)
    val usedFactTable = expr.flatMap({
      case c: ColumnExpr if c.table.isDefined => factTable.filter(_.tableName.equals(c.table.get)).map(_.tableName)
      case _ => Seq()
    }).toIndexedSeq
    ResolvedFieldProjection(Option(resolvedExpr),Option(f.alias.getOrElse(f.expr.map(_.toDsl).get)),usedFactTable)
  }
}

object ProjectionAnalyzerUtil{
  // 处理*
  def dealStar(t: ResolvedTable)(implicit tableAlias:Option[String]=t.alias):Seq[ResolvedProjection]=t.fields.map({
    case f: FactFieldProjection => StarFieldProjection(t, f.alias, Seq(f.factTable), f.expr)
    case f: JoinedFieldProjection => StarFieldProjection(t, f.alias, f.factTables, f.expr)
    case f: StarFieldProjection => StarFieldProjection(t, f.alias, f.factTables, f.expr)
    case f:ResolvedProjection => StarFieldProjection(t, f.alias, f.factTables, f.expr)
  })
}
