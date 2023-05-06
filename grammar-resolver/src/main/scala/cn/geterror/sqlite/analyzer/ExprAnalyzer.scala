package cn.geterror.sqlite.analyzer

import cn.geterror.sqlite.exception.SqlResolveException
import cn.geterror.sqlite.expr.{ColumnExpr, Expr}
import cn.geterror.sqlite.result.{FactTable, ResolvedTable}

class ExprAnalyzer (currentTables:Seq[ResolvedTable], factTable: Seq[FactTable]){

  def resolvedExpr(expr:Expr):Expr={
    expr.transform{
      // select table.field from table
      case col: ColumnExpr if col.table.isDefined =>
        // currentTables存在别名与字段table声明一致的
        val currentExpr = currentTables.find(_.alias.exists(_.equals(col.table.get))) //找到对应表
          .flatMap(_.fields.find(_.alias.exists(col.column.equals(_))) //找到对应字段
            .flatMap(_.expr)) //该字段的表达式
        if (currentExpr.isEmpty) {
          val factExpr = factTable.find(_.tableName.equals(col.table.get)) //找到对应表
            .flatMap(_.fields.find(_.alias.exists(col.column.equals(_))) //找到对应字段
              .flatMap(_.expr)) //该字段的表达式
          if (factExpr.isEmpty) {
            throw new SqlResolveException(s"字段${col.column}找不到依赖")
          }
          factExpr.get
        } else {
          currentExpr.get
        }
      case col: ColumnExpr if col.table.isEmpty =>
        val currentExpr = currentTables
          .flatMap(_.fields.find(_.alias.exists(col.column.equals(_))) //找到对应字段
            .flatMap(_.expr)) //该字段的表达式
        if (currentExpr.isEmpty) {
          throw new SqlResolveException(s"字段${col.column}找不到依赖")
        } else if (currentExpr.size != 1) {
          throw new SqlResolveException(s"字段${col.column}定义模糊")
        }
        currentExpr.head
      case e:Expr=>e
    }
  }
}
