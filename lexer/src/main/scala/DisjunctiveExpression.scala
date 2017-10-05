package lexer

final case class DisjunctiveExpression (private val _factorImplement: Factor, private val _positive: Boolean) {

	// Getters to aid in DisjunctiveExpressionTest.scala. This helps us verify that factorImplement is of the correct implementation.
	def factorImplement = _factorImplement
	def positive = _positive

	// Returns a copy of this disjunctive expression with the positive value flipped
	final def negate: DisjunctiveExpression = {
		DisjunctiveExpression(factorImplement, !positive)
	}

	// Gets the conjunctive representation of its factor.
	final def conjunctiveRepresentation: ConjunctiveRepresentation = {
		val conjunctiveRepresentation = factorImplement.conjunctiveRepresentation
		val representation = new StringBuilder(conjunctiveRepresentation.representation)
		val negation = conjunctiveRepresentation.negation

		if (negation || !positive) {
			representation.insert(0, "not ") // If this causes a double negation, it gets fixed in CompoundFactor's conjunctiveRepresentation()
		}

		ConjunctiveRepresentation(representation.toString, negation)
	}

	// Used to help print out negative identifiers
	override def toString: String = {
		val notAppend = positive match {
			case true => ""
			case false => "not "
		}

		notAppend + factorImplement.toString
	}
}

object DisjunctiveExpression {

	final case class Builder() {

	}

	object Builder {
		// token renamed locationalToken due to the token parameter in the LocationalToken class
		@throws(classOf[ParserException])
		final def build(locationalToken: LocationalToken, lexer: DisjunctiveLexer): DisjunctiveExpression = {
			/* Tokens must follow the pattern NOT (optional) ID or NOT (optional) CompoundFactor
		 	* Check if the given locationalToken is NOT. If it is, call a helper method to finish the pattern.
		 	* If it isn't call a helper method to check the remaining Token to see if it's in the pattern */
			if (locationalToken.getType == Token.NOT) {
				DisjunctiveExpression(getRemainingNegativePattern(lexer), false)
			}

			else {
				DisjunctiveExpression(checkPositivePattern(locationalToken, lexer), true)
			}
		}

		// If the first locational token was NOT, verify the remaining pattern and return the appropriate class <: Factor
		@throws(classOf[ParserException])
		def getRemainingNegativePattern(lexer: DisjunctiveLexer): Factor = {
			val nextToken = lexer.nextValid.getOrElse(throw ParserException.INVALID_TOKEN())

			// We followed the patter NOT, ID
			if (nextToken.getType == Token.ID) {
				Identifier.Builder.build(nextToken)
			}

			/* Now, check if the rest of the expression is of type CompoundFactor
	     	* first, turn nextToken into a locational token and then check the rest if it fits the pattern */
			else {
				CompoundFactor.Builder.build(nextToken, lexer) // Will throw appropriate exceptions if the pattern is not met
			}
		}

		// If the first locational token was not a NOT, verify the remaining pattern and return the appropriate class <: Factor
		@throws(classOf[ParserException])
		def checkPositivePattern(locationalToken: LocationalToken, lexer: DisjunctiveLexer): Factor = {
			if (locationalToken.getType == Token.ID) {
				Identifier.Builder.build(locationalToken)
			}
			// Check if the expression is a CompoundFactor.
			else {
				CompoundFactor.Builder.build(locationalToken, lexer) // Will throw appropriate exceptions if the pattern is not met
			}
		}
	}

}
