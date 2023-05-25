import gapt.expr.formula.Formula
import gapt.expr.formula.fol.FOLTerm
import Conflicts.tpf

/**
 * Recursive definition of a hitting tree.
 * @param contents the conflict terms at this level in the tree
 * @param children the children (tree) for every respective conflict term
 */
case class Tree(contents: List[FOLTerm], var children: List[Option[Tree]]) {}

object Hit {
  /**
   * Diagnose the problem by finding all the minimal hitting sets.
   * @param problem
   * @return the minimal hitting sets
   */
  def diagnose(problem : () => (List[Formula], List[FOLTerm], List[Formula])) : Option[List[List[FOLTerm]]] = {

    var hits: List[List[FOLTerm]] = List()

    def recurs(tree: Tree, path: List[FOLTerm]): Unit = {
      // Loop over all the components in the conflict set and run the theorem prover.
      for (comp <- tree.contents) {
        tpf(problem, comp :: path) match {
          // If we find a new conflict set we recurse down the tree on the new conflict set.
          case Some(conflictSet) =>
            val subtree = Tree(conflictSet.toList, List())
            tree.children = tree.children :+ Some(subtree)
            recurs(subtree, comp :: path)
          // Else we have reached a leaf node meaning we found a hitting set.
          case None =>
            tree.children = tree.children :+ None
            hits = hits :+ (comp :: path)
        }
      }
    }

    tpf(problem, List()) match {
      case Some(conflictSet) =>
        // Recurse over the hitting tree to find all the hitting sets.
        val tree = Tree(conflictSet.toList, List())
        recurs(tree, List())
        // Remove all the non-minimal hitting sets and return the final hitting sets.
        Some(hits.filter(hit => !hits.exists(otherhit => otherhit != hit && otherhit.forall(hit.contains))))
      case None => None
    }
  }
}