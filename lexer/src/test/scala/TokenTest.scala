package lexer

import org.scalatest.FlatSpec

class TokenTest extends FlatSpec {
	behavior of "A NOT Token type"
	it should "have the pattern not and acillary data false" in {
		val token = Token.NOT
		assert(token.pattern == "not")
		assert(!token.hasData)
	}

	behavior of "two tokens types"
	it should "have different hashcodes, tostring, and equality" in {
		val token1 = Token.NOT
		val token2 = Token.AND

		assert(!token1.equals(token2))
		assert(token1.hashCode != token2.hashCode)
		assert(token1.toString != token2.toString)
	}

	behavior of "A Builder token"
	it should "have different equals and hashcode as another Builder token" in {
		val token = Token.ID
		val builder1 = Token.Builder(token, Option("123"))
		val builder2 = Token.Builder(token, Option("1234"))

		assert(!builder1.equals(builder2))
		assert(builder1.hashCode != builder2.hashCode)
	}

	behavior of "Token construction"
	it should "add to the map distinctly and be able to be looked up" in {
		val token1 = Token.of(Token.ID, "2323")
		val token2 = Token.of(Token.ID, "2323")

		assert(token1 == token2)
	}

	behavior of "A Token getName call"
	it should "have the appropriate getName returned value" in {
		val token = Token.AND
		assert(token.getName == "AND")
	}
}