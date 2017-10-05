package lexer

case class CompoundFactor(private val leftExpression: DisjunctiveExpression, private val rightExpression: DisjunctiveExpression) extends Factor{

	override def toString: String = {
		"(" + leftExpression.toString + " and " + rightExpression.toString + ")"
	}

	// Gets the conjunctive representation of its Factor
	override def conjunctiveRepresentation: ConjunctiveRepresentation = {
		val leftCnj = leftExpression.conjunctiveRepresentation
		val rightCnj = rightExpression.conjunctiveRepresentation

		var str = "(not " + leftCnj.representation + " or not " + rightCnj.representation + ")"
		str = str.replaceAll("not not ", "") // Remove double negation

		ConjunctiveRepresentation(str, true)
	}
}

object CompoundFactor {

	final case class Builder() {
	}

	object Builder {
		/* token renamed locationalToken due to the token parameter in the LocationalToken class
	 	* Uses the first token and the rest of the lexer's tokens to extract parentheses, IDs and keywords.
	 	* Throws the appropriate exception if the tokens do not follow the pattern OPEN DisjunctiveExpression AND DisjunctiveExpression CLOSE. */
		@throws(classOf[ParserException])
		final def build(locationalToken: LocationalToken, lexer: DisjunctiveLexer): CompoundFactor = {
			// Make sure the first given locational token is an open parentheses
			ParserException.verify(Token.OPEN, locationalToken)
			// Verify the rest of the tokens and save the identifiers for the expressions
			val leftExpression = getExpression(lexer)
			checkNext(Token.AND, lexer)
			val rightExpression = getExpression(lexer)
			checkNext(Token.CLOSE, lexer)

			CompoundFactor(leftExpression, rightExpression)
		}

		// Returns the DisjunctiveExpression in our lexer, or an appropriate exception if the tokens don't meet the pattern
		@throws(classOf[ParserException])
		def getExpression(lexer: DisjunctiveLexer): DisjunctiveExpression = {
			// Gets the next valid token in the lexer. Throws an exception if its invalid
			val nextLocationalToken = lexer.nextValid
			ParserException.verify(nextLocationalToken)

			// Checks and sees if the pattern follows a DisjunctiveExpression pattern
			DisjunctiveExpression.Builder.build(nextLocationalToken.get, lexer) // Throws appropriate token expected exception if needed
		}

		/* Increments our lexer and checks if we got a token. If we did, check if the next valid token
		 * is equal to a passed in Token.Type.
		 * Helper method for build() to avoid repeated code by writing out this method twice to check AND and CLOSE.
		 * This also allows easier understanding of how the build() method words. */
		@throws(classOf[ParserException])
		def checkNext(_type: Token.Type, lexer: DisjunctiveLexer): Unit = {
			val nextLocationalToken = lexer.nextValid
			ParserException.verify(nextLocationalToken)
			ParserException.verify(_type, nextLocationalToken.get)
		}
	}

}
