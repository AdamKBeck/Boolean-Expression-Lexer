package lexer

import org.scalatest.FlatSpec

class LocationalTokenTest extends FlatSpec {
  behavior of "A Locational token created from the LocationalToken class"
  it should "return its appropriate token, location, type, and data" in {
    val token = Token.of(Token.ID, "123123")
    val locationalToken = LocationalToken(token, 3)

    assert(locationalToken.token == token)
    assert(locationalToken.location == 3)
    assert(locationalToken.getType == Token.ID)
    assert(locationalToken.getData == Option("123123"))
  }

	behavior of "A Locational token created from parsing in our Lexer class"
	it should "return its appropriate token, location, type, and data" in {
		val lexer = Lexer("(a and b)")
		if (lexer.hasNext) {
			val locationalToken = lexer.next

			assert(locationalToken.token == Token.of(Token.OPEN, "("))
			assert(locationalToken.location == 0)
		}
	}

}
