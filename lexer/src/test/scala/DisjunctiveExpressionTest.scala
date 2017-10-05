package lexer

import org.scalatest.FlatSpec

class DisjunctiveExpressionTest extends FlatSpec {
	behavior of "build with the pattern NOT ID"
	it should "Return a Disjunctive Expression with positive false and of type Identifier" in {
		val disjunctiveLexer = DisjunctiveLexer("a")
		val locationalToken = LocationalToken(Token.of(Token.NOT, "not"), 2)
		val expression = DisjunctiveExpression.Builder.build(locationalToken, disjunctiveLexer)
		assert(expression.factorImplement.getClass.toString == "class lexer.Identifier")
		assert(expression.factorImplement.toString == "a")
		assert(expression.positive == false)
	}

	behavior of "build with the pattern ID"
	it should "Return a Disjunctive Expression with positive true and of type Identifier" in {
		val disjunctiveLexer = DisjunctiveLexer("")
		val locationalToken = LocationalToken(Token.of(Token.ID, "a"), 1)
		val expression = DisjunctiveExpression.Builder.build(locationalToken, disjunctiveLexer)
		assert(expression.factorImplement.getClass.toString == "class lexer.Identifier")
		assert(expression.factorImplement.toString == "a")
		assert(expression.positive == true)
	}

	behavior of "build with the pattern NOT CompoundFactor"
	it should "Return a Disjunctive Expression with positive false and of type CompoundFactor" in {
		val disjunctiveLexer = DisjunctiveLexer("(a and b)")
		val locationalToken = LocationalToken(Token.of(Token.NOT, "not"), 2)
		val expression = DisjunctiveExpression.Builder.build(locationalToken, disjunctiveLexer)
		assert(expression.factorImplement.getClass.toString == "class lexer.CompoundFactor")
		assert(expression.factorImplement.toString == "(a and b)")
		assert(expression.positive == false)
	}

}
