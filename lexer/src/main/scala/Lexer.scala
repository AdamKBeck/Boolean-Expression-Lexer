package lexer

import java.util.regex.Pattern
import scala.util.control.Breaks._

case class Lexer(val input: String) {

	private val matcher = Lexer.tokenPatterns.matcher(input)

	/* Returns true if the matcher finds the next input subsequence in tokenPatterns.
	 * also increments our "iterator" matcher*/
	def hasNext: Boolean = {
		matcher.find
	}

	/* Returns the next locational token found in the input.
	 * if no more tokens are present, throw a TOKEN_EXPECTED exception */
	@throws(classOf[ParserException])
	def next: LocationalToken = {
		// Get the start and end indices
		if (!matcher.hitEnd) {
			getNextLocationalToken
		}
		else {
			try {
				getNextLocationalToken
			}
			catch {
				case e: IllegalStateException => throw ParserException.TOKEN_EXPECTED()
			}
		}
	}

	// Helper method for next to avoid repeated code. Gets the next token as a locational token
	def getNextLocationalToken: LocationalToken = {
		val startLoc = matcher.start

		// Find the token itself and create it to construct our LocationalToken object
		val tokenType = getFirstMatchedToken
		val token = Token.of(tokenType, matcher.group(tokenType.getName))

		LocationalToken(token, startLoc)
	}

	/* Returns the first matched token by filtering out everything else that doesn't match.
	 * Every call to matcher.group(i.getName) will be null if the first matched token isn't "i".
	 * Therefore only one token is left, the matched one, and we return it by getting it from the list head */
	def getFirstMatchedToken: Token.Type = {
		Token._tokenList.filter(i => matcher.group(i.getName) != null).head
	}

	/* Returns the next token in the set of validTypes. Throw an INVALID_TOKEN exception if it finds
	 * a token in the invalidTypes set. Ignore tokens not in either set. Return None if no valid token
	 * is found */
	@throws(classOf[ParserException])
	def nextValid(validTypes: Set[Token.Type], invalidTypes: Set[Token.Type]): Option[LocationalToken] = {
		var locationalToken: Option[LocationalToken] = None

		breakable {
			// Finds the next valid token in our input string
			while (hasNext) {
				val token = next.token

				if (invalidTypes.contains(token.tokenType)) {
					throw ParserException.INVALID_TOKEN(token.toString)
				}
				else if (validTypes.contains(token.tokenType)) {
					locationalToken = Some(LocationalToken(token, matcher.start))
					break
				}
			}
		}

		locationalToken
	}
}

object Lexer {

	/* Returns the first token in the table that matches the input.
	 * Silently ignores any sequence that does not match any of the Token.Type objects. */
	private val tokenPatterns: Pattern = {
		constructPattern
	}

	/* Combines all Token.Type's patterns into a single Pattern object to use for parsing.
	 * Uses named groups in order to aid in the search of the next matching pattern in the sequence */
	def constructPattern: Pattern = {
		val pattern = new StringBuilder

		// Loop through the tail of the token list, piping each element to the head grouping
		for (token <- Token._tokenList) {
			pattern.append(tokenToGroupedRegex(token))
			pattern.append("|")
		}

		pattern.setLength(pattern.length -1); // Remove the last extra pipe, O(1) time
		Pattern.compile(pattern.toString)
	}

	def tokenToGroupedRegex(token: Token.Type): String = {
		"(?<" + token.getName + ">" + token.pattern + ")"
	}

}