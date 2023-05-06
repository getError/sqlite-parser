package cn.geterror.sqlite.tree

import com.fasterxml.jackson.annotation.JsonIgnore

/**
  * @author hanyecong02
  */
trait OriginAware {

  @JsonIgnore
  val origin: Origin = CurrentOrigin.get

}
