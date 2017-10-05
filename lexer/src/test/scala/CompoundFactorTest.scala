package lexer

import org.scalatest.FlatSpec

class CompoundFactorTest extends FlatSpec {
	behavior of "checkNext"
	it should "throw the correct expected exception or an Invalid Token exception" in {
		val disjunctiveLexer = DisjunctiveLexer("a a")

		assertThrows[ParserException.OPEN_EXPECTED] {
			CompoundFactor.Builder.checkNext(Token.OPEN, disjunctiveLexer)
		}

		val correctDisjunctiveLexer = DisjunctiveLexer("(a and b)")
		CompoundFactor.Builder.checkNext(Token.OPEN,correctDisjunctiveLexer)
		succeed
	}

	behavior of "toString from build with a simple (a and b) expression"
	it should "return the correct left and right expressions" in {
		val disjunctiveLexer = DisjunctiveLexer("a and b)")
		val locationalToken = LocationalToken(Token.of(Token.OPEN, "("),1)

		assert(CompoundFactor.Builder.build(locationalToken, disjunctiveLexer).toString
		== "(a and b)")
	}


	behavior of "toString from build with a complex (Disjunctive expression and Disjunctive expression)"
	it should "return the correct left and right expressions" in {
		val disjunctiveLexer = DisjunctiveLexer("not(a and b) and b)")
		val locationalToken = LocationalToken(Token.of(Token.OPEN, "("),1)

		assert(CompoundFactor.Builder.build(locationalToken, disjunctiveLexer).toString
		== "(not (a and b) and b)")



	}
}
