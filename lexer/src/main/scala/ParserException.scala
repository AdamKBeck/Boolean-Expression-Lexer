package lexer

final case class ParserException (
	private val _token: LocationalToken,
	private val _errorCode: ParserException.ErrorCode) extends Exception {
	private val serialVersionUID: Long = 293l

	// Getters
	def token = _token
	def errorCode = _errorCode

	override def toString: String = {
		token.toString + "[" + errorCode.getName + "]"
	}

}

object ParserException {

	// Map to store all the disjuntive token error codes to help reduce complexity for verify(Token.Type, Locational)
	val disjunctiveErrorsCache = Map[Token.Type, ParserException.ErrorCode](
		Token.AND -> ParserException.AND_EXPECTED(),
		Token.OPEN -> ParserException.OPEN_EXPECTED(),
		Token.CLOSE -> ParserException.CLOSE_EXPECTED(),
		Token.ID -> ParserException.ID_EXPECTED()
	)

	// Throws a TOKEN_EXPECTED exception if the token is not present
	@throws(classOf[ParserException])
	def verify(token: Option[LocationalToken]): Unit = {
		token match {
			case Some(n) => ; // nominal case, do nothing
			case _ => throw TOKEN_EXPECTED()
		}
	}

	/* Throws the appropriate exception if the token is not of the expected type.
	 * token parameter was renamed to locationalToken because of the "token" paramater in LocationalToken class */
	@throws(classOf[ParserException])
	final def verify(expectedType: Token.Type, locationalToken: LocationalToken): Unit = {
		if (locationalToken.token.tokenType == expectedType) {
			// do not throw an exception
		}
		else {
			// Throw the appropriate expection, or an INVALID_TOKEN exception if the expected type isn't found
			throw disjunctiveErrorsCache.getOrElse(expectedType, ParserException.INVALID_TOKEN())
		}
	}

	// Throws a TRAILING_INPUT exception if the token is present
	@throws(classOf[ParserException])
	def verifyEnd(token: Option[LocationalToken]): Unit = {
		token match {
			case Some(n) => throw TRAILING_INPUT()
			case _ => ; // Do nothing
		}
	}

	// Enum implementation of ParserException.ErrorCode by using case objects
	sealed abstract class ErrorCode(val _code: String) extends Exception(_code){
		// Gets the name of "this" errorCode
		def getName: String = {
			this.getClass.getName.toString.split("\\$").last // Strips the getName name (e.g. Scala230923 $TOKEN_EXPECTED$)
		}
	}

	// Using case classes instead of case objects in order to throw these as exceptions
	case class TOKEN_EXPECTED(val message: String = "Token Expected") extends ErrorCode(message)
	case class INVALID_TOKEN(val message: String = "Invalid Token") extends ErrorCode(message)
	case class TRAILING_INPUT(val message: String = "Trailing Input") extends ErrorCode(message)
	case class AND_EXPECTED(val message: String = "And Expected") extends ErrorCode(message)
	case class OPEN_EXPECTED(val message: String = "Open Expected") extends ErrorCode(message)
	case class CLOSE_EXPECTED(val message: String = "Close Expected") extends ErrorCode(message)
	case class ID_EXPECTED(val message: String = "ID Expected") extends ErrorCode(message)
}