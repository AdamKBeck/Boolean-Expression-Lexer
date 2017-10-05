package lexer

final case class DisjunctiveLexer(val input: String) {
	private val lexer = Lexer(input)

	def nextValid: Option[LocationalToken] = {
		lexer.nextValid(DisjunctiveLexer.validTokenTypes, DisjunctiveLexer.invalidTokenTypes)
	}
}

object DisjunctiveLexer {
	final val validTokenTypes: Set[Token.Type] = Set(
		Token.AND, Token.ID, Token.NOT, Token.OPEN, Token.CLOSE
	)

	final val invalidTokenTypes: Set[Token.Type] = Set(
		Token.OR, Token.NUMBER, Token.BINARYOP
	)
}
