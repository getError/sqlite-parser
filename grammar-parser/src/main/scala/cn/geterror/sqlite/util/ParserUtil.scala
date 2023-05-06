package cn.geterror.sqlite.util

import org.antlr.v4.runtime.ParserRuleContext

object ParserUtil {
  def getText(ctx: ParserRuleContext): String = {
    if(ctx == null) {
      return null
    }
    ctx.getText
  }
}
