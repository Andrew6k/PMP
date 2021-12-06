package Lab8

import com.cra.figaro.language._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.algorithm.factored.VariableElimination

object Ex3 {
	def main(args: Array[String]) {
		
		val length = 10
		val study: Array[Element[String]] = Array.fill(length)(Constant(""))
		val pass: Array[Element[Boolean]] = Array.fill(length)(Constant(false))

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
			pass(course) = Apply(study(course),(s: String)=>
				if(s=="mediu")
					Flip(0.5)==true
				else if(s=="mult")
					Flip(0.8)==true
				else
					Flip(0.2)==true
			)
		}//
		pass(0).observe(true)
		pass(1).observe(true)
		pass(2).observe(true)
		VariableElimination.probability(pass(9),true)		
	}
}