/**
  * Created by Clarence on 11/24/15.
  */
object Tokens {

  abstract class TOKEN

  case class TOK_KEYWORD(s: String) extends TOKEN // Keywords
  case class TOK_VAR(s: String) extends TOKEN // Variables (IDs)
  case class TOK_OP(s: String) extends TOKEN  // Operator
  case class TOK_NUM(n: Int) extends TOKEN  // Numbers
  case object TOK_SEMI extends TOKEN // Semicolon
  case class TOK_STR(s: String) extends TOKEN // String
  case class TOK_PAR(s: String) extends TOKEN // Parenthesis
  case class TOK_CURL(s: String) extends TOKEN  // Curly brackets
  case object TOK_WHITE extends TOKEN  // White space


}
