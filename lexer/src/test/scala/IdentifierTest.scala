package lexer

import org.scalatest.FlatSpec

class IdentifierTest extends FlatSpec {
	behavior of "build with not an ID token"
	it should "throw an ID_EXPECTED() exception" in {
		val token = Token.of(Token.NOT, "not")
		val locational = LocationalToken(token, 3)

		assertThrows[ParserException.ID_EXPECTED] {
			Identifier.Builder.build(locational)
		}
	}

	behavior of "toString from a build with an ID token"
	it should "output the data for the ID token" in {
		val token = Token.of(Token.ID, "a")
		val locational = LocationalToken(token, 3)

		val identifier = Identifier.Builder.build(locational)
		assert(identifier.toString == "a")
	}
}
