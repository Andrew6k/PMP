package Lab11

import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.language._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.library.compound.CPD
import com.cra.figaro.library.compound.{RichCPD, OneOf, *}


object Ex1 {
	def main(args: Array[String]) {
		
		val isPresident = Flip(0.000000025) //one in 40 mil
		val personIsLeftHanded = Flip(0.1)
  		val presidentIsLeftHanded = Flip(0.5)
  		personIsLeftHanded.observe(true)
  		val becomesPresident = RichCPD(personIsLeftHanded, presidentIsLeftHanded,
        	(true, true) -> Flip(0.005),
        	(*, *) -> Flip(0.000000025)
			)
		val alg1 = VariableElimination(becomesPresident)
    	alg1.start()
    	alg1.stop()
    	println("Left handed:" + alg1.probability(becomesPresident,true))

		val presidentHarvard = Flip(0.15)
	    val personHarvard = Flip(0.0005)
  		personHarvard.observe(true)
  		val becomesPresident2 =RichCPD(personHarvard, presidentHarvard,
        	(true, true) -> Flip(0.075),
        	(*, *) -> Flip(0.000000025)
			)
		val alg2 = VariableElimination(becomesPresident2)
    	alg2.start()
    	alg2.stop()
    	println("Went to Harvard:" + alg2.probability(becomesPresident2,true))

		presidentHarvard.observe(true)
		personHarvard.observe(true)
		personIsLeftHanded.observe(true)
		presidentIsLeftHanded.observe(true)

		val becomesPresident3 =RichCPD(personHarvard, presidentHarvard, personIsLeftHanded, presidentIsLeftHanded,
        	(true, true, true, true) -> Flip(0.2),
			(true, true, *, *) -> Flip(0.075),
			(*, *, true, true) -> Flip(0.005),
        	(*, *, *, *) -> Flip(0.000000025)
			)
		val alg3 = VariableElimination(becomesPresident3)
    	alg3.start()
    	alg3.stop()
    	println("Both:" + alg3.probability(becomesPresident3,true))

    )

	}
}