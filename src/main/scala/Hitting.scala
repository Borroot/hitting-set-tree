import gapt.expr.formula.Formula
import gapt.expr.formula.fol.FOLTerm
import gapt.expr.stringInterpolationForExpressions
import Conflicts.tpf

object Hitting {
  def makeHittingTree(problem : () => (List[Formula], List[FOLTerm], List[Formula])) : Option[Tree] = {

    def recurs(tree: Tree, path: List[FOLTerm]): Unit = {
      for (comp <- tree.contents) {
        tpf(problem, comp :: path) match {
          case Some(conflictSet) =>
            val conflictList = conflictSet.toList
            val subtree = Tree(conflictList, List())
            tree.children = tree.children :+ Some(subtree)
            recurs(subtree, comp :: path)
          case None =>
            tree.children = tree.children :+ None
        }
      }
    }

    val path = List()

    tpf(problem, path) match {
      case Some(conflictSet) =>
        val conflictList = conflictSet.toList
        val tree = Tree(conflictList, List())
        recurs(tree, path)
        Some(tree)
      case None => None
    }
  }

  def gatherHittingSets(tree: Tree): Set[Set[FOLTerm]] = {
    Set(Set(fot""))
  }

  def getDiagnoses(hittingSets: Set[Set[FOLTerm]]): Set[Set[FOLTerm]] = {
    hittingSets
  }

  def diagnose(problem : () => (List[Formula], List[FOLTerm], List[Formula])) : Option[Set[Set[FOLTerm]]] = {
    makeHittingTree(problem) match {
      case Some(tree) =>
        val hittingSets = gatherHittingSets(tree)
        Some(getDiagnoses(hittingSets))
      case None => None
    }
  }
}