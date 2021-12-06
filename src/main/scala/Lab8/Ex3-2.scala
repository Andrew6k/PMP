package Lab8

import com.cra.figaro.language._
import com.cra.figaro.library.atomic.discrete._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.algorithm.factored.VariableElimination

object Ex32 {
	def main(args: Array[String]) {
		
		val length = 10
		val study: Array[Element[String]] = Array.fill(length)(Constant(""))
		val pass: Array[Element[Boolean]] = Array.fill(length)(Constant(false))
        val score: Array[Element[Int]] = Array.fill(length)(Constant(0))
		
        study(0)= Constant("mediu")
		for{course <-1 until length} {
			study(course) = Apply(study(course-1),(s: String)=>
				if(s=="mediu")
					if(Flip(0.7)==true)
						"mult"
					else
						"putin"
				else if(s=="mult")
					if(Flip(0.6)==true)
						"mult"
					else if(Flip(0.5)==true)
						"putin"
					else
						"mediu"
				else
					"mediu"
			)
		}//
        for{course <- 0 until length} {
            score(course) = Apply(study(course-1),(s: String)=>
				if(s=="mediu")
                    FromRange(4, 8)
				else if(s=="mult")
					FromRange(7,10)
				else
					FromRange(1,5)
			)
        }
		for{course <- 0 until length} {
			pass(course) = Apply(score(course),(s: Int)=>
				if(s>=5)
					true
				else
					false
			)
		}//
		pass(0).observe(true)
		pass(1).observe(true)
		pass(2).observe(true)
		VariableElimination.probability(pass(9),true)		
	}
}