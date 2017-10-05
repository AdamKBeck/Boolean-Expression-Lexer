package lexer

final case class Identifier private (private val id: String) extends Factor{
	override def toString: String = {
		id.toString
	}

	// Returns the Identifier's id and false
	override def conjunctiveRepresentation: ConjunctiveRepresentation = {
		ConjunctiveRepresentation(id, false)
	}
}

object Identifier {
	final case class Builder() {
	}

	object Builder {
		/* token renamed locationalToken due to the token parameter in the LocationalToken class
		 * Note: no final on def build because it is redundant */
		@throws(classOf[ParserException])
		def build(locationalToken: LocationalToken): Identifier = {
			// Verifies that our locational token is an ID, and builds the Identifier. Throws appropriate exceptions if not ID
			ParserException.verify(Token.ID, locationalToken)
			Identifier(locationalToken.getData.getOrElse(""))
		}
	}
}
