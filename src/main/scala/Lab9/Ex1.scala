package Lab9

import com.cra.figaro.language._
import com.cra.figaro.library.compound.{If, CPD, RichCPD, OneOf}
import com.cra.figaro.algorithm.factored.VariableElimination

object Ex4
{
	def main(args: Array[String])
	{
		val years = 10
		val investment: Array[Element[Int]] = Array.fill(years)(Constant(150))
		val profit: Array[Element[Int]] = Array.fill(years)(Constant(500))
		val capital: Array[Element[Int]] = Array.fill(years)(Constant(1000))

		capital(0) = Constant(1000)
		for {year <- 0 until years}
		{
			profit(year) = Select(0.3 -> 1000, 0.5->500, 0.4->0)
		}
		for { year <- 0 until years }
		{
			investment(year) = CPD(profit(year),
				1000 -> Select(0.1 -> 1000, 0.1 -> 2000, 0.3 -> 3000, 0.7 -> 4000, 0.5 -> 5000),
				500 -> Select(0.1 -> 100, 0.4 -> 200, 0.8 -> 300, 0.5 -> 400, 0.1 -> 500),
				0 -> Select(0.4 -> 100, 0.6 -> 200, 0.2 -> 300, 0.1 -> 400, 0.1 -> 500))
		}
		for { year <- 1 until years }
		{
			capital(year) = capital(year-1)+profit(year)-investment(year)
		}
		
		println(VariableElimination.probability(capital(9), 3000))

	}
}