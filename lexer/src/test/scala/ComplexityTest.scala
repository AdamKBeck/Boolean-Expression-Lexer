package lexer

import org.scalatest.FlatSpec

class ComplexityTest extends FlatSpec {

	// Helper method to sum up the complexities
	def complexitySum(lexer: Lexer): Int = {
		var sum = 0

		while (lexer.hasNext) {
			val token = lexer.next.token
			if (token.tokenType.isComplex) {
				sum += 1
			}
			else {
			}
		}
		sum
	}

	behavior of "A simple non-complex expression"
	it should "Have a complexity of zero" in {
		val lexer = Lexer("(a not b)")
		val sum = complexitySum(lexer)
		assert(sum == 0)
	}

	behavior of "An expression with all non-complex tokens"
	it should "Have a complexity of zero" in {
		val lexer = Lexer("(a not b  234 * 3)")
		val sum = complexitySum(lexer)
		assert(sum == 0)
	}

	behavior of "An expression with complex tokens"
	it should "Have a complexity of as many complex tokens there are" in {
		val lexer = Lexer("(a and b or c and 2323*34)")
		val sum = complexitySum(lexer)
		assert(sum == 3)
	}

}
