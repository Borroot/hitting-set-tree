import Diagnosis._
import Conflicts.tpf
import Hitting.makeHittingTree
import gapt.expr.stringInterpolationForExpressions



object Main extends App {
  println("Running diagnostics on problem 1 with an empty list of broken components..")

  val tree = makeHittingTree(problem1)
  println(tree)

//  val hs = List()
//  val Some(result) = tpf(problem1, hs)
//
//  println(result)
}
