package Lab3

import com.cra.figaro.language._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.library.compound._
import com.cra.figaro.algorithm.factored.VariableElimination

object Ex3 {
    def main(args: Array[String]) {
        // val x = Flip(0.4)
        // val y = Flip(0.4)
        // val z = x
        // val w = x === z
        // println(VariableElimination.probability(w, true))

        val x = Flip(0.4)
        val y = Flip(0.4)
        val z = y
        val w = x === z
        println(VariableElimination.probability(w, true))
	}
}