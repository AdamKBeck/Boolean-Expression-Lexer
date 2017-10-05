package lexer
import scala.io.StdIn._

object CommandLineInput extends App{
	println("Enter in a boolean expression on the next line surrounded by parenthesis")
	val expression = readLine

	// Setup everything we need to create a DisjunctiveExpression object
	val firstChar = expression.head

	if (firstChar != '(') {
		throw ParserException.OPEN_EXPECTED()
	}

	val lexer = DisjunctiveLexer(expression.tail)
	val locationalToken = LocationalToken(Token.of(Token.OPEN, "("), 1)
	val disjunctiveExpression = DisjunctiveExpression.Builder.build(locationalToken, lexer)

	println("The conjunctive representation is: \n" + disjunctiveExpression.conjunctiveRepresentation.representation)
}
