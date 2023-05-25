import Diagnosis._
import Hit.diagnose
import gapt.expr.formula.Formula
import gapt.expr.formula.fol.FOLTerm



object Main extends App {
  val problems : List[() => (List[Formula], List[FOLTerm], List[Formula])] = List(problem1, problem2, problem3, problem_fa)
  val problem_names = List("problem1", "problem2", "problem3", "problem_fa")

  for ((problem, name) <- problems zip problem_names) {
    println(s"Minimal hitting sets for ${name}.")
    diagnose(problem) match {
      case Some(hits) => println(s"${hits.map(_.mkString(" ")).mkString(" | ")}")
      case None => println("None")
    }
  }
}
