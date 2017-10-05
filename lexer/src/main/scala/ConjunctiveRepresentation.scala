package lexer

final case class ConjunctiveRepresentation(private val _representation: String,
	private val _negation: Boolean) {

	def representation = _representation
	def negation = _negation

}
