package Lab2

import com.cra.figaro.language._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.library.compound._
import com.cra.figaro.algorithm.factored.VariableElimination

object Ex1 {
    def main(args: Array[String]) {
		val test = Constant("Test")
        val reduced = Flip(0.5)
        val needed = Flip(0.5)
        val result = CPD(reduced,needed,
            (true,true) -> Constant("100%"),
            (true,false) -> Constant("50%"),
            (false,true) -> Constant("80%"),
            (false,false) -> Constant("20%"),
        )

        result.observe("100%")
		val algorithm = Importance(1000, needed)
		algorithm.start()
		
		println(algorithm.probability(needed, true))
	}
}