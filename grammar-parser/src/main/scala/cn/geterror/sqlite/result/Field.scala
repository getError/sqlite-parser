package cn.geterror.sqlite.result

import cn.geterror.sqlite.expr.Expr


/**
 * 并非指SQL中的投影，而是指某个表的某个字段,或者计算出的衍生字段如count(*)等
 */
case class Field(name: String, fieldType: String) {
}
