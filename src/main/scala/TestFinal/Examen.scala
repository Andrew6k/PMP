package Test2

import com.cra.figaro.language._
import com.cra.figaro.library.compound.{If, CPD, RichCPD, OneOf}
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.sampling._

object Ex1 {
	class Autor() {
		val popular = Flip(0.16) //de 5 ori mai probabil sa nu fie popular
	}
	class Album(val at: Autor){
		val calitate = Select(0.27 -> 'mica, 0.6 -> 'medie, 0.13 -> 'mare)
	}
	class Nominalizare(val alb: Album){
		def getProb() {
			val prob = RichCPD(alb.calitate, alb.at.popular,
           	 ('mica, false) -> Constant(0.003),
			 ('mica, true) -> Constant(0.014),
			 ('medie, false) -> Constant(0.016),
			 ('medie, true) -> Constant(0.043),
			 ('mare, false) -> Constant(0.047),
			 ('mare, true) -> Constant(0.18)
			)
			val nominalizat = Flip(prob)
		}
		
	}
	def main(args: Array[String]) {
		var autori:Array[Autor]=new Array[Autor](5)
	}
}