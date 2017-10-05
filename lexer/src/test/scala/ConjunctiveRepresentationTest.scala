package lexer

import org.scalatest.FlatSpec

class ConjunctiveRepresentationTest extends FlatSpec {
	behavior of "CompoundFactor's conjunctiveRepresentation()"
	it should "Return an expression in the form not left or not right, true" in {
		val lexer = DisjunctiveLexer("a and b)")
		val locationalToken = LocationalToken(Token.of(Token.OPEN, "("), 1)
		val compoundFactor = CompoundFactor.Builder.build(locationalToken, lexer)
		assert(compoundFactor.conjunctiveRepresentation.representation == "(not a or not b)")
		assert(compoundFactor.conjunctiveRepresentation.negation)
	}

	behavior of "CompoundFactor's nested conjunctiveRepresentation()"
	it should "Return an expression with the negation flag explicitely as not in inside expression" in {
		val lexer = DisjunctiveLexer("a and (b and c))")
		val locationalToken = LocationalToken(Token.of(Token.OPEN, "("), 1)
		val compoundFactor = CompoundFactor.Builder.build(locationalToken, lexer)
		assert(compoundFactor.conjunctiveRepresentation.representation == "(not a or (not b or not c))")
		assert(compoundFactor.conjunctiveRepresentation.negation)
	}

	behavior of "CompoundFactor's double negation conjunctiveRepresentation()"
	it should "Cancel out all appropriate double not's found in the expression" in {
		val lexer = DisjunctiveLexer("not a and not b)")
		val locationalToken = LocationalToken(Token.of(Token.OPEN, "("), 1)
		val compoundFactor = CompoundFactor.Builder.build(locationalToken, lexer)
		assert(compoundFactor.conjunctiveRepresentation.representation == "(a or b)")
		assert(compoundFactor.conjunctiveRepresentation.negation)
	}

	behavior of "Identifier's conjunctiveRepresentatin()"
	it should "Return the same identifier with negation set to false" in {
		val locationalToken = LocationalToken(Token.of(Token.ID, "a"), 1)
		val identifier = Identifier.Builder.build(locationalToken)
		assert(identifier.conjunctiveRepresentation.representation == "a")
		assert(!identifier.conjunctiveRepresentation.negation)
	}

	behavior of "DisjunctiveExpression conjunctiveRepresentation()"
	it should "Return the conjunctiveRepresentation of its factor implement" in {
		val lexer = DisjunctiveLexer("a and (b and c))")
		val locationalToken = LocationalToken(Token.of(Token.OPEN, "1"), 1)
		val disjunctiveExpression = DisjunctiveExpression.Builder.build(locationalToken, lexer)
		assert(disjunctiveExpression.conjunctiveRepresentation.representation == "not (not a or (not b or not c))")
	}

	behavior of "Complex nested double negative DisjunctiveExpression conjunctiveRepresentation()"
	it should "Return the correct cojunctiveRepresentation and exclude double negatives" in {
		val lexer = DisjunctiveLexer("(not a and b) and (c and not d))")
		val locationalToken = LocationalToken(Token.of(Token.OPEN, "1"), 1)
		val disjunctiveExpression = DisjunctiveExpression.Builder.build(locationalToken, lexer)
		assert(disjunctiveExpression.conjunctiveRepresentation.representation == "not ((a or not b) or (not c or d))")
	}
}

