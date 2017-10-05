package lexer

import org.scalatest.FlatSpec

class LexerTest extends FlatSpec {

	behavior of "tokenToGroupedRegex"
	it should "Return a pattern of correctly formatted regex groups" in {
		val tokenGroupList: List[String] = Token._tokenList.map(t => Lexer.tokenToGroupedRegex(t))

		assert(tokenGroupList(0) == "(?<NOT>not)")
		assert(tokenGroupList(1) == "(?<AND>and)")
		assert(tokenGroupList(2) == "(?<OR>or)")
		assert(tokenGroupList(3) == "(?<OPEN>\\()")
		assert(tokenGroupList(4) == "(?<CLOSE>\\))")
		assert(tokenGroupList(5) == "(?<ID>[a-z]+)")
		assert(tokenGroupList(6) == "(?<NUMBER>\\-\\d+|\\d+)")
		assert(tokenGroupList(7) == "(?<BINARYOP>\\*|\\+|\\-|\\/)")
		assert(tokenGroupList(8) == "(?<WHITESPACE>\\s+)")
	}

	behavior of "hasNext for a valid expression"
	it should "Return true throughout every true matcher.find and false when done" in {
		val lexer = Lexer("a and b not c)")

		assert(lexer.hasNext) // Always true for this example to start out with

		// Exhaust every matcher.find search
		while (lexer.hasNext) {

		}

		assert(!lexer.hasNext)
	}

	behavior of "hasNext for an invalid expression"
	it should "Return false from the start" in {
		val lexer = Lexer("!")
		assert(!lexer.hasNext)
	}

	behavior of "next for valid locational tokens"
	it should "Return a LocationalToken with the correct type and location" in {
		val lexer = Lexer("(a and b not c)")
		if (lexer.hasNext) {
			val locationalToken = lexer.next

			assert(locationalToken.token.tokenType == Token.OPEN)
			assert(locationalToken.location == 0)
			assert(locationalToken.token.data == Some("("))
		}

		// Skip a few and check another one
		lexer.hasNext
		lexer.hasNext

		if (lexer.hasNext) {
			val locationalToken = lexer.next

			assert(locationalToken.token.tokenType == Token.AND)
			assert(locationalToken.location == 3)
			assert(locationalToken.token.data == Some("and"))
		}
	}

	behavior of "next for invalid locational tokens"
	it should "throw a TOKEN_EXPECTED exception" in {
		val lexer = Lexer("(a and not b)")
		while (lexer.hasNext) {

		}

		assertThrows[ParserException.TOKEN_EXPECTED] {
			lexer.next
		}
	}

	behavior of "getFirstMatchedToken"
	it should "Get the first matched token correctly" in {
		val lexer = Lexer("(a and b not c)")

		if (lexer.hasNext) {
			assert(lexer.getFirstMatchedToken == Token.OPEN)
		}
	}

	behavior of "nextValid with valid types"
	it should "Return an option of the locational token found" in {
		val lexer = Lexer("(a and b)")
		val locationalToken = lexer.nextValid(Token._tokenList.toSet, Set.empty)

		assert(locationalToken.get.token.tokenType == Token.OPEN)
	}

	behavior of "nextValid with invalid types"
	it should "Throw an INVALID_TOKEN exception" in {
		val lexer = Lexer("(a and b)")

		assertThrows[ParserException.INVALID_TOKEN] {
			lexer.nextValid(Token._tokenList.toSet, Set(Token.OPEN))
		}
	}

}
