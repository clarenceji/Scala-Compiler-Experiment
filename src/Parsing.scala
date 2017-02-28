import Tokens._

/**
  * Created by Clarence on 11/20/15.
  */
object Parsing {
  import scala.language.implicitConversions
  import scala.language.reflectiveCalls

  abstract class Parser[I <% Seq[_], T] {
    def parse(ts: I): Set[(T, I)]

    def parse_all(ts: I) : Set[T] =
      for ((head, tail) <- parse(ts); if (tail.isEmpty)) yield head
  }

  class SeqParser[I <% Seq[_], T, S](p: => Parser[I, T], q: => Parser[I, S]) extends Parser[I, (T, S)] {
    def parse(sb: I) =
      for ((head1, tail1) <- p.parse(sb);
           (head2, tail2) <- q.parse(tail1)) yield ((head1, head2), tail2)
  }

  class AltParser[I <% Seq[_], T](p: => Parser[I, T], q: => Parser[I, T]) extends Parser[I, T] {
    def parse(sb: I) = p.parse(sb) ++ q.parse(sb)
  }

  class FunParser[I <% Seq[_], T, S](p: => Parser[I, T], f: T => S) extends Parser[I, S] {
    def parse(sb: I) =
      for ((head, tail) <- p.parse(sb)) yield (f(head), tail)
  }

  case class StringParser(s: String) extends Parser[String, String] {
    def parse(sb: String) = {
      val (prefix, suffix) = sb.splitAt(s.length)
      if (prefix == s) Set((prefix, suffix)) else Set()
    }
  }

  case class TokenParser(token: TOKEN) extends Parser[List[TOKEN], TOKEN] {
    def parse(tokens: List[TOKEN]) = tokens match {
      case Nil => Set()
      case head :: tail => {
        if (head == token) Set((head, tail)) else Set()
      }
    }
  }
  
  case object StringTokenParser extends Parser[List[TOKEN], String] {
    def parse(tokens: List[TOKEN]) = tokens match {
      case Nil => Set()
      case head :: tail => head match {
        case TOK_STR(s) => Set((s, tail))
        case _ => Set()
      }
    }
  }

  case object NumParser extends Parser[String, Int] {
    val reg = "[0-9]+".r
    def parse(sb: String) = reg.findPrefixOf(sb) match {
      case None => Set()
      case Some(s) => {
        val (head, tail) = sb.splitAt(s.length)
        Set((head.toInt, tail))
      }
    }
  }

  case object NumTokenParse extends Parser[List[TOKEN], Int] {
    def parse(tokens: List[TOKEN]) = tokens match {
      case Nil => Set()
      case head :: tail => head match {
        case TOK_NUM(aNumber) => Set((aNumber, tail))
        case _ => Set()
      }
    }
  }


  implicit def string2parser(s : String) = StringParser(s)
  implicit def stringToken2parser(token: TOKEN) = TokenParser(token)

  implicit def ParserOps[I<% Seq[_], T](p: Parser[I, T]) = new {
    def || (q : => Parser[I, T]) = new AltParser[I, T](p, q)
    def ==>[S] (f: => T => S) = new FunParser[I, T, S](p, f)
    def ~[S] (q : => Parser[I, S]) = new SeqParser[I, T, S](p, q)
  }

  implicit def StringOps(s: String) = new {
    def || (q : => Parser[String, String]) = new AltParser[String, String](s, q)
    def || (r: String) = new AltParser[String, String](s, r)
    def ==>[S] (f: => String => S) = new FunParser[String, String, S](s, f)
    def ~[S](q : => Parser[String, S]) =
      new SeqParser[String, String, S](s, q)
    def ~ (r: String) =
      new SeqParser[String, String, String](s, r)
  }

  implicit def TokenOps(t: TOKEN) = new {
    def || (q: => Parser[List[TOKEN], TOKEN]) = new AltParser[List[TOKEN], TOKEN](t, q)
    def ==>[S] (f: => TOKEN => S) = new FunParser[List[TOKEN], TOKEN, S](t, f)
    def ~[S] (q: => Parser[List[TOKEN], S]) =
      new SeqParser[List[TOKEN], TOKEN, S](t, q)
    def ~ (r: TOKEN) = new SeqParser[List[TOKEN], TOKEN, TOKEN](t, r)
  }

  lazy val E: Parser[String, Int] =
    (T ~ "+" ~ E) ==> { case ((x, y), z) => x + z} || T
  lazy val T: Parser[String, Int] =
    (F ~ "*" ~ T) ==> { case ((x, y), z) => x * z} || F
  lazy val F: Parser[String, Int] =
    ("(" ~ E ~ ")") ==> { case ((x, y), z) => y} || NumParser


  // no left-recursion allowed
  lazy val EL: Parser[String, Int] =
    ((EL ~ "+" ~ EL) ==> { case ((x, y), z) => x + z} ||
      (EL ~ "*" ~ EL) ==> { case ((x, y), z) => x * z} ||
      ("(" ~ EL ~ ")") ==> { case ((x, y), z) => y} ||
      NumParser)

  //println(E.parse_all("1+2+3"))


  // the abstract syntax trees for the WHILE language
  abstract class Stmt
  abstract class AExp
  abstract class BExp

  type Block = List[Stmt]

  case object Skip extends Stmt
  case class If(a: BExp, bl1: Block, bl2: Block) extends Stmt
  case class While(b: BExp, bl: Block) extends Stmt
  case class Assign(s: String, a: AExp) extends Stmt
  case class Write(a: AExp) extends Stmt
  case class WriteStr(s: String) extends Stmt
  case class Read(s: String) extends Stmt

  case class For(a: Assign, i: AExp, b: Block) extends Stmt

  case class Var(s: String) extends AExp
  case class Num(i: Int) extends AExp
  case class Aop(o: String, a1: AExp, a2: AExp) extends AExp

  case object True extends BExp
  case object False extends BExp
  case class Bop(o: String, a1: AExp, a2: AExp) extends BExp


  case object IdParser extends Parser[String, String] {
    val reg = "[a-z][a-z,0-9]*".r
    def parse(sb: String) = reg.findPrefixOf(sb) match {
      case None => Set()
      case Some(s) => Set(sb.splitAt(s.length))
    }
  }

  // TOKEN
  case object IDTokenParse extends Parser[List[TOKEN], String] {
    def parse(token: List[TOKEN]) = token match {
      case Nil => Set()
      case head :: tail => head match {
        case TOK_VAR(variableName) => Set((variableName, tail))
        case _ => Set()
      }
    }
  }

//  lazy val AExp: Parser[String, AExp] =
//    ((Te ~ "+" ~ AExp) ==> { case ((x, y), z) => Aop("+", x, z):AExp } ||
//      (Te ~ "-" ~ AExp) ==> { case ((x, y), z) => Aop("-", x, z):AExp } || Te)

  // TOKEN
  lazy val AExp: Parser[List[TOKEN], AExp] =
    ((Te ~ TOK_OP("+") ~ AExp) ==> { case ((x, y), z) => Aop("+", x, z):AExp } ||
      (Te ~ TOK_OP("-") ~ AExp) ==> { case ((x, y), z) => Aop("-", x, z):AExp } || Te)

  lazy val Te: Parser[List[TOKEN], AExp] =
    (Fa ~ TOK_OP("*") ~ Te) ==> { case ((x, y), z) => Aop("*", x, z):AExp } || Fa
  lazy val Fa: Parser[List[TOKEN], AExp] =
    ((TOK_PAR("(") ~ AExp ~ TOK_PAR(")")) ==> { case ((x, y), z) => y } ||
      IDTokenParse ==> Var ||
      NumTokenParse ==> Num)

  // boolean expressions
  lazy val BExp: Parser[List[TOKEN], BExp] =
    ((AExp ~ TOK_OP("=") ~ AExp) ==> { case ((x, y), z) => Bop("=", x, z): BExp} ||
      (AExp ~ TOK_OP("!=") ~ AExp) ==> { case ((x, y), z) => Bop("!=", x, z): BExp } ||
      (AExp ~ TOK_OP("<") ~ AExp) ==> { case ((x, y), z) => Bop("<", x, z): BExp } ||
      (AExp ~ TOK_OP(">") ~ AExp) ==> { case ((x, y), z) => Bop(">", x, z): BExp } ||
      (AExp ~ TOK_OP("<=") ~ AExp) ==> { case ((x, y), z) => Bop("<=", x, z): BExp } ||
      (AExp ~ TOK_OP(">=") ~ AExp) ==> { case ((x, y), z) => Bop(">=", x, z): BExp } ||
      (TOK_KEYWORD("true") ==> ((_) => True: BExp)) ||
      (TOK_KEYWORD("false") ==> ((_) => False: BExp)) ||
      (TOK_PAR("(") ~ BExp ~ TOK_PAR(")")) ==> { case ((x, y), z) => y})

  lazy val Stmt: Parser[List[TOKEN], Stmt] =
    ((TOK_KEYWORD("skip") ==> ((_) => Skip: Stmt)) ||
      (IDTokenParse ~ TOK_OP(":=") ~ AExp) ==> { case ((x, y), z) => Assign(x, z): Stmt } ||
      (TOK_KEYWORD("if") ~ BExp ~ TOK_KEYWORD("then") ~ Block ~ TOK_KEYWORD("else") ~ Block) ==>
        { case (((((x,y),z),u),v),w) => If(y, u, w): Stmt } ||
      (TOK_KEYWORD("while") ~ BExp ~ TOK_KEYWORD("do") ~ Block) ==> { case (((x, y), z), w) => While(y, w) } ||
      (TOK_KEYWORD("read") ~ IDTokenParse) ==> { case (x, y) => Read(y): Stmt } ||
      (TOK_KEYWORD("write") ~ AExp) ==> { case (x, y) => Write(y): Stmt } ||
      (TOK_KEYWORD("write") ~ StringTokenParser) ==> { case (x, y) => WriteStr(y): Stmt } ||
      (TOK_KEYWORD("for") ~ IDTokenParse ~ TOK_OP(":=") ~ AExp ~ TOK_KEYWORD("upto") ~ AExp ~ TOK_KEYWORD("do") ~ Block) ==> {
        case (((((((_, y), _), u), _), w), _), s) => For(Assign(y, u), w, s)
      }
      )

  lazy val Stmts: Parser[List[TOKEN], Block] =
    (Stmt ~ TOK_SEMI ~ Stmts) ==> { case ((x, y), z) => x :: z : Block } ||
      (Stmt ==> ((s) => List(s) : Block))

  lazy val Block: Parser[List[TOKEN], Block] =
    ((TOK_CURL("{") ~ Stmts ~ TOK_CURL("}")) ==> { case ((x, y), z) => y} ||
      (Stmt ==> ((s) => List(s))))


//  Block.parse_all("x2:=5")
//  Block.parse_all("{x:=5;y:=8}")
//  Block.parse_all("if(false)then{x:=5}else{x:=10}")

  val fib = """{n:=10;minus1:=0;minus2:=1;temp:=0;while(n>0)do{temp:=minus2;minus2:=minus1+minus2;minus1:=temp;n:=n-1};result:=minus2}"""

//  Block.parse_all(fib)

  // an interpreter for the WHILE language
  type Env = Map[String, Int]

  def eval_aexp(a: AExp, env : Env) : Int = a match {
    case Num(i) => i
    case Var(s) => env(s)
    case Aop("+", a1, a2) => eval_aexp(a1, env) + eval_aexp(a2, env)
    case Aop("-", a1, a2) => eval_aexp(a1, env) - eval_aexp(a2, env)
    case Aop("*", a1, a2) => eval_aexp(a1, env) * eval_aexp(a2, env)
  }

  def eval_bexp(b: BExp, env: Env) : Boolean = b match {
    case True => true
    case False => false
    case Bop("=", a1, a2) => eval_aexp(a1, env) == eval_aexp(a2, env)
    case Bop("!=", a1, a2) => !(eval_aexp(a1, env) == eval_aexp(a2, env))
    case Bop(">", a1, a2) => eval_aexp(a1, env) > eval_aexp(a2, env)
    case Bop("<", a1, a2) => eval_aexp(a1, env) < eval_aexp(a2, env)
    case Bop(">=", a1, a2) => eval_aexp(a1, env) >= eval_aexp(a2, env)
    case Bop("<=", a1, a2) => eval_aexp(a1, env) <= eval_aexp(a2, env)
  }

  def eval_stmt(s: Stmt, env: Env) : Env = s match {
    case Skip => env
    case Assign(x, a) => env + (x -> eval_aexp(a, env))
    case If(b, bl1, bl2) => if (eval_bexp(b, env)) eval_bl(bl1, env) else eval_bl(bl2, env)
    case While(b, bl) =>
      if (eval_bexp(b, env)) eval_stmt(While(b, bl), eval_bl(bl, env))
      else env

    case Write(a) => println(eval_aexp(a, env)); env
    case WriteStr(s) => println(s); env
//    case WriteAExp(a) => println(eval_aexp(a, env)); env
    case Read(a) => env + (a -> io.Source.stdin.getLines().next().toInt)

    case For(a, int, b) =>
      if (eval_bexp(Bop("<=", a.a, int), env)) {

        val newAssign = Assign(a.s, Aop("+", a.a, Num(1)))
//        val newBlock: Block = b ::: List(newAssign)

//        return eval_stmt(While(Bop("<=", a.a, int), newBlock), eval_bl(b, eval_stmt(a, env)))

        return eval_stmt(For(newAssign, int, b), env)
      }
      else eval_stmt(a, env)
  }

  def eval_bl(bl: Block, env: Env) : Env = bl match {
    case Nil => env
    case s::bl => eval_bl(bl, eval_stmt(s, env))
  }

  def eval(bl: Block) : Env = eval_bl(bl, Map())

//  eval(Block.parse_all(fib).head)("result")


  // Clarence Ji:

  def tokenizer(): List[Stmt] = {



    return List[Stmt]()
  }

}
