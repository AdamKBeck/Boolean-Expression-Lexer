package lexer

final case class LocationalToken(private val _token: Token, val _location: Int = -1) {

	// Getters
	def token = _token
	def location = _location
	
	// Methods to get token type and ancillary data
	def getType: Token.Type = _token.tokenType
	def getData: Option[String] = _token.data // Returning the option because there is no clear advantageous choice

	override def toString: String = {
		"[" + token.toString + ", " + location + "]"
	}
	
}