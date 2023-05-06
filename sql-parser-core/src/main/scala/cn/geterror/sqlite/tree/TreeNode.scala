package cn.geterror.sqlite.tree

import org.apache.commons.lang3.ClassUtils

import java.util.concurrent.atomic.AtomicBoolean
import scala.collection.Map


abstract class TreeNode[BaseType <: TreeNode[BaseType]] extends Product with OriginAware with Serializable {
  self: BaseType =>

  def children: Seq[BaseType]

  private lazy val _childrenSet: Set[TreeNode[_]] = children.toSet

  final def containsChild(child: TreeNode[_]): Boolean = {
    _childrenSet.contains(child)
  }

  /**
   * Faster version of equality which short-circuits when two treeNodes are the same instance.
   * We don't just override Object.equals, as doing so prevents the scala compiler from
   * generating case class `equals` methods
   */
  def fastEquals(other: TreeNode[_]): Boolean = {
    this.eq(other) || this == other
  }

  def exists(f: BaseType => Boolean): Boolean = {
    if (f(this)) {
      true
    } else {
      children.exists(_.exists(f))
    }
  }

  /**
   * Find the first [[TreeNode]] that satisfies the condition specified by `f`.
   * The condition is recursively applied to this node and all of its children (pre-order).
   */
  def find(f: BaseType => Boolean): Option[BaseType] = if (f(this)) {
    Some(this)
  } else {
    children.foldLeft(Option.empty[BaseType]) { (l, r) => l.orElse(r.find(f)) }
  }

  /**
   * Runs the given function on this node and then recursively on [[children]].
   * @param f the function to be applied to each node in the tree.
   */
  def foreach(f: BaseType => Unit): Unit = {
    f(this)
    children.foreach(_.foreach(f))
  }

  /**
   * Runs the given function recursively on [[children]] then on this node.
   * @param f the function to be applied to each node in the tree.
   */
  def foreachUp(f: BaseType => Unit): Unit = {
    children.foreach(_.foreachUp(f))
    f(this)
  }

  /**
   * Returns a Seq containing the result of applying the given function to each
   * node in this tree in a preorder traversal.
   * @param f the function to be applied.
   */
  def map[A](f: BaseType => A): Seq[A] = {
    val ret = new collection.mutable.ArrayBuffer[A]()
    foreach(ret += f(_))
    ret
  }

  /**
   * Returns a Seq by applying a function to all nodes in this tree and using the elements of the
   * resulting collections.
   */
  def flatMap[A](f: BaseType => TraversableOnce[A]): Seq[A] = {
    val ret = new collection.mutable.ArrayBuffer[A]()
    foreach(ret ++= f(_))
    ret
  }

  /**
   * Returns a Seq containing the result of applying a partial function to all elements in this
   * tree on which the function is defined.
   */
  def collect[B](pf: PartialFunction[BaseType, B]): Seq[B] = {
    val ret = new collection.mutable.ArrayBuffer[B]()
    val lifted = pf.lift
    foreach(node => lifted(node).foreach(ret.+=))
    ret
  }

  /**
   * Returns a Seq containing the leaves in this tree.
   */
  def collectLeaves(): Seq[BaseType] = {
    this.collect { case p if p.children.isEmpty => p }
  }

  /**
   * Finds and returns the first [[TreeNode]] of the tree for which the given partial function
   * is defined (pre-order), and applies the partial function to it.
   */
  def collectFirst[B](pf: PartialFunction[BaseType, B]): Option[B] = {
    val lifted = pf.lift
    lifted(this).orElse {
      children.foldLeft(Option.empty[B]) { (l, r) => l.orElse(r.collectFirst(pf)) }
    }
  }

  /**
   * Returns a copy of this node where `rule` has been recursively applied to the tree.
   * When `rule` does not apply to a given node it is left unchanged.
   * Users should not expect a specific directionality. If a specific directionality is needed,
   * transformDown or transformUp should be used.
   *
   * @param rule the function use to transform this nodes children
   */
  def transform(rule: PartialFunction[BaseType, BaseType]): BaseType = {
    transformDown(rule)
  }

  /**
   * Returns a copy of this node where `rule` has been recursively applied to it and all of its
   * children (pre-order). When `rule` does not apply to a given node it is left unchanged.
   *
   * @param rule the function used to transform this nodes children
   */
  def transformDown(rule: PartialFunction[BaseType, BaseType]): BaseType = {
    val afterRule = CurrentOrigin.withOrigin(origin) {
      rule.applyOrElse(this, identity[BaseType])
    }

    // Check if unchanged and then possibly return old copy to avoid gc churn.
    if (this fastEquals afterRule) {
      mapChildren(_.transformDown(rule))
    } else {
      afterRule.mapChildren(_.transformDown(rule))
    }
  }

  /**
   * Returns a copy of this node where `rule` has been recursively applied first to all of its
   * children and then itself (post-order). When `rule` does not apply to a given node, it is left
   * unchanged.
   *
   * @param rule the function use to transform this nodes children
   */
  def transformUp(rule: PartialFunction[BaseType, BaseType]): BaseType = {
    val afterRuleOnChildren = mapChildren(_.transformUp(rule))
    if (this fastEquals afterRuleOnChildren) {
      CurrentOrigin.withOrigin(origin) {
        rule.applyOrElse(this, identity[BaseType])
      }
    } else {
      CurrentOrigin.withOrigin(origin) {
        rule.applyOrElse(afterRuleOnChildren, identity[BaseType])
      }
    }
  }

  /**
   * Returns a copy of this node where `f` has been applied to all the nodes children.
   */
  def mapChildren(f: BaseType => BaseType): BaseType = {

    if (children.isEmpty) {
      return this
    }

    def mapChild[Out](child: TreeNode[_], changed: AtomicBoolean)(ft: BaseType => Out): Out = {
      val newChild = f(child.asInstanceOf[BaseType])
      if (!(newChild fastEquals child)) {
        changed.set(true)
        ft(newChild)
      } else {
        ft(child.asInstanceOf[BaseType])
      }
    }

    def mapAny(in: Any, changed: AtomicBoolean): Any = in match {
      case arg: TreeNode[_] if containsChild(arg) =>
        mapChild(arg, changed)((x: TreeNode[_]) => x)
      case Some(arg: TreeNode[_]) if containsChild(arg) =>
        mapChild(arg, changed)(Option.apply)
      case m: Map[_, _] =>
        val mapChanged = new AtomicBoolean(false)
        val newMap = m.mapValues(mapAny(_, mapChanged)).view.force
        if (mapChanged.get()) {
          changed.set(true)
          newMap
        } else m
      case args: Traversable[_] =>
        val seqChanged = new AtomicBoolean(false)
        val newSeq = args.map(mapAny(_, seqChanged))
        if (seqChanged.get()) {
          changed.set(true)
          newSeq
        } else args
      case nonChild => nonChild
    }

    val argsChanged = new AtomicBoolean(false)
    val newArgs = this.productIterator.map(mapAny(_, argsChanged)).toArray

    if (argsChanged.get()) {
      makeCopy(newArgs)
    } else this
  }

  protected val otherCopyArgs: Seq[AnyRef] = Nil

  def makeCopy(arguments: Array[Any]): BaseType = {

    val constructors = this.getClass.getConstructors

    val allArgs = arguments ++ otherCopyArgs

    val ctor = constructors.find { ctor =>

      def isAssignable(skipNull: Boolean): Boolean = {

        val classPairs = allArgs.zip(ctor.getParameterTypes).map {
          case (argObj, paramCls) =>
            val argCls = Option(argObj).map(_.getClass).orNull[Class[_]]
            (argCls, paramCls)
        }

        val classesToCompare = if (skipNull) classPairs.filter(_._1 != null) else classPairs

        val (argClasses, paramClasses) = classesToCompare.unzip[Class[_], Class[_]]

        ClassUtils.isAssignable(argClasses, paramClasses, true)
      }

      if (allArgs.length != ctor.getParameterCount) {
        false
      } else if (isAssignable(false)) {
        true
      } else if (isAssignable(true)) {
        true
      } else {
        false
      }
    }.orNull

    try {
      ctor.newInstance(allArgs.asInstanceOf[Array[_ <: AnyRef]]: _*).asInstanceOf[BaseType]
    } catch {
      case e: IllegalArgumentException =>
        throw e
    }
  }



}
