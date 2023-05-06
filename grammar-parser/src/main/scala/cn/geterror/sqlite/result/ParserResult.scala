package cn.geterror.sqlite.result

import cn.geterror.sqlite.expr.Expr

case class ParserResult
(
  /**
   * 投影部分，对应于select操作
   */
  projections:Seq[Projection],

  /**
   * 来源部分，对应于from操作
   */
  table:Table,

  /**
   * where部分，对应于where操作
   */
  where:Option[Expr],

  /**
   * group by部分，对应于group by操作
   */
  groupBy:Seq[Expr],

  /**
   * having部分，对应于having操作
   */
  having:Option[Expr]
)
