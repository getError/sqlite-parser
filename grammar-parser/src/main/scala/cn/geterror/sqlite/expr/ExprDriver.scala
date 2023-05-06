package cn.geterror.sqlite.expr

import scala.reflect.ClassTag

object ExprDriver {


  /**
   * 找到表达式数满足target条件的表达式子树，且不会在满足条件的子树A中寻找A的子树满足target条件
   * @param expr
   * @return
   */
  def findInExpr(expr: Expr, target:Function[Expr,Boolean]): Seq[Expr]={
    var result:Seq[Expr] = Seq()
    if(target(expr)){
      Seq(expr)
    }else{
      expr.children.foreach(child=>{
        result = result++findInExpr(child,target)
      })
      result
    }
  }

  def findTypeInExpr[T<:Expr:ClassTag](expr: Expr, target:Class[T]): Seq[T]= {
    findInExpr(expr, target.isInstance).asInstanceOf[Seq[T]]
  }

  def seqEquals(seq1:Seq[Expr],seq2:Seq[Expr]):Boolean={
    if(seq1.size != seq2.size){
      false
    }else{
      seq1.zip(seq2).forall(tuple=>tuple._1.equals(tuple._2))
    }
  }
}
