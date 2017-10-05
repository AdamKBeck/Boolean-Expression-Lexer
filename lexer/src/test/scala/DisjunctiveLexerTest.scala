package lexer

import org.scalatest.FlatSpec

class DisjunctiveLexerTest extends FlatSpec {

	behavior of "nextValid for all valid tokens"
	it should "return the correct token" in {
		val disjunctiveLexer = DisjunctiveLexer("(a and b and not c)")

		assert(disjunctiveLexer.nextValid.get.getType == Token.OPEN)
		assert(disjunctiveLexer.nextValid.get.getType == Token.ID)
		assert(disjunctiveLexer.nextValid.get.getType == Token.AND)
		assert(disjunctiveLexer.nextValid.get.getType == Token.ID)
		assert(disjunctiveLexer.nextValid.get.getType == Token.AND)
		assert(disjunctiveLexer.nextValid.get.getType == Token.NOT)
		assert(disjunctiveLexer.nextValid.get.getType == Token.ID)
		assert(disjunctiveLexer.nextValid.get.getType == Token.CLOSE)
	}

	behavior of "nextValid for all invalid tokens"
	it should "throw an invalid token for each token" in {
		val disjunctiveLexer = DisjunctiveLexer("5 + 4 * 3/4")
		assertThrows[ParserException.INVALID_TOKEN] {
			disjunctiveLexer.nextValid
		}
		assertThrows[ParserException.INVALID_TOKEN] {
			disjunctiveLexer.nextValid
		}
		assertThrows[ParserException.INVALID_TOKEN] {
			disjunctiveLexer.nextValid
		}
		assertThrows[ParserException.INVALID_TOKEN] {
			disjunctiveLexer.nextValid
		}
		assertThrows[ParserException.INVALID_TOKEN] {
			disjunctiveLexer.nextValid
		}
		assertThrows[ParserException.INVALID_TOKEN] {
			disjunctiveLexer.nextValid
		}
		assertThrows[ParserException.INVALID_TOKEN] {
			disjunctiveLexer.nextValid
		}
	}
}
