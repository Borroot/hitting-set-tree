import gapt.expr.formula.fol.FOLTerm

case class Tree(contents: List[FOLTerm], var children: List[Option[Tree]]) {}