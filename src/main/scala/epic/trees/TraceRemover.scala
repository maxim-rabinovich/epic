package epic.trees

/**
 * Removes all traces from the word sequence, deleting all empty categories while it's at it.
 *
 * @author dlwh
 **/
class TraceRemover[T, W](emptyCategory: T=>Boolean) extends (Tree[T] =>Tree[T]) {
  def apply(tree: Tree[T]):Tree[T] = {
    def rec(tree: Tree[T]):Option[Tree[T]] = {
      println(tree.toString)
      println()
      System.out.flush()

      if (emptyCategory(tree.label) || tree.span.begin == tree.span.end) {
        None
      } else if (tree.children.length == 0) {
        Some(tree)
      } else {
        val newChildren = tree.children.map(rec).collect{ case Some(t) => t }
        if (newChildren.length == 0 && !tree.isLeaf) {
          None
        } else {
          Some(Tree(tree.label,newChildren, tree.span))
        }
      }
    }

    rec(tree).get
  }
}
