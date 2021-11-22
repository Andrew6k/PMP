package Lab7

import com.cra.figaro.language.{Element, Select, Flip, Apply, Chain}
import com.cra.figaro.library.compound.{^^}
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.discrete._
import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.library.compound.If

object Departments {

	class ResearchAndDevelopment {
		val state = FromRange(0,101)
	}

	class HumanResources {
		val state = FromRange(0,101) 
	}

	class Production(val rd: ResearchAndDevelopment, val hr: HumanResources) {
		
		val state = Apply(rd.state, hr.state,
								(state1: Int, state2: Int) =>
									if (state1 >= 50)
									{
										if (state2 >= 50) 80 else 60
									}
									else
									{
										20
									}
			)

	}

	class Sales(val p: Production) {
		val state = FromRange(0,101) 
	}

	class Finance(val hr: HumanResources, val s: Sales) {
		
		val state = Apply(hr.state, s.state,
								(state1: Int, state2: Int) =>
									if (state1 >= 50)
									{
										if (state2 >= 50) 90 else 50
									}
									else
									{
										10
									}
			) 
	}

	class Firm(val rd: ResearchAndDevelopment, val hr: HumanResources, val p: Production, val s: Sales, val f: Finance) {
	// 	val health = Apply(rd.state, hr.state, p.state, s.state, f.state,
	// 							(state1: Int, state2: Int, state3: Int, state4: Int, state5: Int) =>
	// 								if (state3 >= 50)
	// 								{
	// 									if (state5 >= 50) 99 else 80
	// 								}
	// 								else
	// 								{
	// 									45
	// 								}
	// 		)
		val health = Apply(rd.state, hr.state, p.state, s.state, f.state,
	 							(state1: Int, state2: Int, state3: Int, state4: Int, state5: Int) =>
								(state1+state2+state3+state4+state5)/5) 				 
	}

	def main(args: Array[String]) {
		val rd = new ResearchAndDevelopment()
		val hr = new HumanResources()
		val p = new Production(rd, hr)
		val s = new Sales(p)
		val f = new Finance(hr, s)
		val firm = new Firm(rd, hr, p, s, f)

		p.state.observe(55)
		f.state.observe(55)
		val alg = Importance(200, firm.health)
        alg.start()
        alg.stop()
        println("Expected health:" + alg.mean(firm.health))
		//VariableElimination.probability(firm.health,45)
		
	}
}