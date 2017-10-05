package lexer

import org.scalatest.FlatSpec

class ParserExceptionTest extends FlatSpec{

	behavior of "verify with a None"
	it should "Throw a TOKEN_EXPECTED exception" in {
		assertThrows[ParserException.TOKEN_EXPECTED] {
			ParserException.verify(None)
		}
	}

	behavior of "Verify with a LocationalToken"
	it should "Not throw an exception" in {
		val token = Token.of(Token.NOT, "not")
		val locationalToken = LocationalToken(token, 0)

		ParserException.verify(Some(locationalToken))
		succeed
	}

	behavior of "verifyEnd with a LocationalToken"
	it should "Throw a TRAILING_INPUT exception" in {
		assertThrows[ParserException.TRAILING_INPUT] {
			ParserException.verifyEnd(Some(LocationalToken(Token.of(Token.NOT, "not"),0)))
		}
	}

	behavior of "verifyEnd with None"
	it should "Not throw an exception" in {
		ParserException.verifyEnd(None)
		succeed
	}

	behavior of "ParserException.ErrorCode Enums"
	it should "Be able to get their errorCodes and locations effectively" in {
		val notTokenTrailingInput = ParserException(new LocationalToken(Token.of(Token.NOT, "none")),ParserException.TRAILING_INPUT())
		val notTokenInvalidToken = ParserException(new LocationalToken(Token.of(Token.NOT, "none")),ParserException.INVALID_TOKEN())
		val notTokenTokenExpected = ParserException(new LocationalToken(Token.of(Token.NOT, "none")),ParserException.TOKEN_EXPECTED())

		assert(notTokenTrailingInput.toString == "[[NOT: \"not\", false][\"none\"], -1][TRAILING_INPUT]")
		assert(notTokenTrailingInput.token.location == -1)
		assert(notTokenTrailingInput.errorCode.getName == ParserException.TRAILING_INPUT.toString())

		assert(notTokenInvalidToken.toString == "[[NOT: \"not\", false][\"none\"], -1][INVALID_TOKEN]")
		assert(notTokenInvalidToken.token.location == -1)
		assert(notTokenInvalidToken.errorCode.getName == ParserException.INVALID_TOKEN.toString())

		assert(notTokenTokenExpected.toString == "[[NOT: \"not\", false][\"none\"], -1][TOKEN_EXPECTED]")
		assert(notTokenTokenExpected.token.location == -1)
		assert(notTokenTokenExpected.errorCode.getName == ParserException.TOKEN_EXPECTED.toString())
	}


	behavior of "the expectedType verify method"
	it should "Throw the correct errorCode if it is not the expected type" in {
		val token = Token.of(Token.NOT, "123123")
		val locationalToken = LocationalToken(token, 3)

		assertThrows[ParserException.ID_EXPECTED] {
			ParserException.verify(Token.ID, locationalToken)
		}

		ParserException.verify(Token.NOT, locationalToken)
		succeed
	}
}
