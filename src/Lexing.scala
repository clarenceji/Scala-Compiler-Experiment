/**
  * Created by Clarence on 11/20/15.
  */
object Lexing {

  abstract class Rexp
  case object NULL extends Rexp
  case object EMPTY extends Rexp
  case class CHAR(c: Char) extends Rexp
  case class ALT(r1: Rexp, r2: Rexp) extends Rexp
  case class SEQ(r1: Rexp, r2: Rexp) extends Rexp
  case class STAR(r: Rexp) extends Rexp
  case class RECD(x: String, r: Rexp) extends Rexp
  case class NTIMES(r: Rexp, n: Int) extends Rexp
  case class RANGE(c: List[Char]) extends Rexp
  case class PLUS(r: Rexp) extends Rexp
  case class QUESTION(r: Rexp) extends Rexp
  case class N(r: Rexp, n: Int) extends Rexp


  abstract class Val
  case object EmptyVal extends Val
  case class Chr(c: Char) extends Val
  case class SeqVal(v1: Val, v2: Val) extends Val
  case class Left(v: Val) extends Val
  case class Right(v: Val) extends Val
  case class Stars(vs: List[Val]) extends Val
  case class Rec(x: String, v: Val) extends Val
  case class PlusVal(vs: List[Val]) extends Val
  case class OptVal(v: Val) extends Val
  case class NVal(vs: List[Val]) extends Val
  case class RangeVal(vs: List[Char]) extends Val



  // some convenience for typing in regular expressions
  def charlist2rexp(s : List[Char]): Rexp = s match {
    case Nil => EMPTY
    case c::Nil => CHAR(c)
    case c::s => SEQ(CHAR(c), charlist2rexp(s))
  }
  implicit def string2rexp(s : String) : Rexp = charlist2rexp(s.toList)

  implicit def RexpOps(r: Rexp) = new {
    def | (s: Rexp) = ALT(r, s)
    def % = STAR(r)
    def ~ (s: Rexp) = SEQ(r, s)
  }

  implicit def stringOps(s: String) = new {
    def | (r: Rexp) = ALT(s, r)
    def | (r: String) = ALT(s, r)
    def % = STAR(s)
    def ~ (r: Rexp) = SEQ(s, r)
    def ~ (r: String) = SEQ(s, r)
    def $ (r: Rexp) = RECD(s, r)
  }

  // nullable function: tests whether the regular
  // expression can recognise the empty string
  def nullable (r: Rexp) : Boolean = r match {
    case NULL => false
    case EMPTY => true
    case CHAR(_) => false
    case ALT(r1, r2) => nullable(r1) || nullable(r2)
    case SEQ(r1, r2) => nullable(r1) && nullable(r2)
    case STAR(_) => true
    case RECD(_, r1) => nullable(r1)

    case RANGE(_) => false
    case PLUS(r) => nullable(r)
    case QUESTION(_) => true
    case N(r, i) =>
      if (i == 0) true
      else nullable(r)
  }

  // derivative of a regular expression w.r.t. a character
  def der (c: Char, r: Rexp) : Rexp = r match {
    case NULL => NULL
    case EMPTY => NULL
    case CHAR(d) => if (c == d) EMPTY else NULL
    case ALT(r1, r2) => ALT(der(c, r1), der(c, r2))
    case SEQ(r1, r2) =>
      if (nullable(r1)) ALT(SEQ(der(c, r1), r2), der(c, r2))
      else SEQ(der(c, r1), r2)
    case STAR(r) => SEQ(der(c, r), STAR(r))
    case RECD(_, r1) => der(c, r1)

    case RANGE(charSet) =>
      if (charSet.contains(c)) EMPTY
      else NULL
    case PLUS(r) => SEQ(der(c, r), STAR(r))
    case QUESTION(r) => ALT(EMPTY, der(c,r))
    case N(r, n) =>
      if (n == 0) NULL
      else SEQ(der(c, r), N(r, n - 1))
  }

  // derivative w.r.t. a string (iterates der)
  def ders (s: List[Char], r: Rexp) : Rexp = s match {
    case Nil => r
    case c::s => ders(s, der(c, r))
  }

  // extracts a string from value
  def flatten(v: Val) : String = v match {
    case EmptyVal => ""
    case Chr(c) => c.toString
    case Left(v) => flatten(v)
    case Right(v) => flatten(v)
    case SeqVal(v1, v2) => flatten(v1) + flatten(v2)
    case Stars(vs) => vs.map(flatten).mkString
    case Rec(_, v) => flatten(v)
  }

  // extracts an environment from a value
  def env(v: Val) : List[(String, String)] = v match {
    case EmptyVal => Nil
    case Chr(c) => Nil
    case Left(v) => env(v)
    case Right(v) => env(v)
    case SeqVal(v1, v2) => env(v1) ::: env(v2)
    case Stars(vs) => vs.flatMap(env)
    case Rec(x, v) => (x, flatten(v))::env(v)

    case PlusVal(v) => v.flatMap(env)
    case OptVal(v) => env(v)
    case NVal(v) => v.flatMap(env)
    case RangeVal(_) => Nil
  }

  // injection part
  def mkeps(r: Rexp) : Val = r match {
    case EMPTY => EmptyVal
    case ALT(r1, r2) =>
      if (nullable(r1)) Left(mkeps(r1)) else Right(mkeps(r2))
    case SEQ(r1, r2) => SeqVal(mkeps(r1), mkeps(r2))
    case STAR(_) => Stars(Nil)
    case RECD(x, r) => Rec(x, mkeps(r))

    //    case RANGE(c) => c match {
    //      case Nil => EmptyVal
    //      case head :: rest => mkeps(ALT(CHAR(head), RANGE(rest)))
    //    }
    case PLUS(r) => PlusVal(Nil)
//      SeqVal(mkeps(r), mkeps(STAR(r)))
    case QUESTION(r) => Left(EmptyVal)
    case N(r, n) => NVal(Nil)
//      if (n == 0) EmptyVal else SeqVal(mkeps(r), mkeps(N(r, n - 1)))
  }


  def inj(r: Rexp, c: Char, v: Val) : Val = (r, v) match {
    case (STAR(r), SeqVal(v1, Stars(vs))) => Stars(inj(r, c, v1)::vs)
    case (SEQ(r1, r2), SeqVal(v1, v2)) => SeqVal(inj(r1, c, v1), v2)
    case (SEQ(r1, r2), Left(SeqVal(v1, v2))) => SeqVal(inj(r1, c, v1), v2)
    case (SEQ(r1, r2), Right(v2)) => SeqVal(mkeps(r1), inj(r2, c, v2))
    case (ALT(r1, r2), Left(v1)) => Left(inj(r1, c, v1))
    case (ALT(r1, r2), Right(v2)) => Right(inj(r2, c, v2))
    case (CHAR(d), EmptyVal) => Chr(c)
    case (RECD(x, r1), _) => Rec(x, inj(r1, c, v))

    case (PLUS(r1), SeqVal(v1, Stars(v2))) => PlusVal(inj(r1, c, v1)::v2)
//      Stars(inj(r, c, v1)::v2)
    case (QUESTION(r1), v1) => OptVal(inj(r1, c, v1))
//      Left(inj(r1, c, v1))
//    case (QUESTION(r1), Right(EmptyVal)) => Right(Chr(c))
    case (N(r1, n1), EmptyVal) => NVal(List[Val](Chr(c)))
//      inj(r1, c, EmptyVal)
    case (N(r1, n1), SeqVal(v1, NVal(v2))) => NVal(inj(r1,c,v1)::v2)
    //    case (N(r1, n1), SeqVal(v1, Stars(v2))) => Stars(inj(r1, c, v1)::v2)
  }

  // main lexing function (produces a value)
  def lex(r: Rexp, s: List[Char]) : Val = s match {
    case Nil => if (nullable(r)) mkeps(r) else throw new Exception("Not matched")
    case c::cs => inj(r, c, lex(der(c, r), cs))
  }

  def lexing(r: Rexp, s: String) : Val = lex(r, s.toList)

  lexing(("ab" | "ab") ~ ("b" | EMPTY), "ab")

  // some "rectification" functions for simplification
  def F_ID(v: Val): Val = v
  def F_RIGHT(f: Val => Val) = (v:Val) => Right(f(v))
  def F_LEFT(f: Val => Val) = (v:Val) => Left(f(v))
  def F_ALT(f1: Val => Val, f2: Val => Val) = (v:Val) => v match {
    case Right(v) => Right(f2(v))
    case Left(v) => Left(f1(v))
  }
  def F_SEQ(f1: Val => Val, f2: Val => Val) = (v:Val) => v match {
    case SeqVal(v1, v2) => SeqVal(f1(v1), f2(v2))
  }
  def F_SEQ_Empty1(f1: Val => Val, f2: Val => Val) =
    (v:Val) => SeqVal(f1(EmptyVal), f2(v))
  def F_SEQ_Empty2(f1: Val => Val, f2: Val => Val) =
    (v:Val) => SeqVal(f1(v), f2(EmptyVal))
  def F_RECD(f: Val => Val) = (v:Val) => v match {
    case Rec(x, v) => Rec(x, f(v))
  }
  def F_ERROR(v: Val): Val = throw new Exception("error")

  // simplification of regular expressions returning also an
  // rectification function; no simplification under STAR
  def simp(r: Rexp): (Rexp, Val => Val) = r match {
    case ALT(r1, r2) => {
      val (r1s, f1s) = simp(r1)
      val (r2s, f2s) = simp(r2)
      (r1s, r2s) match {
        case (NULL, _) => (r2s, F_RIGHT(f2s))
        case (_, NULL) => (r1s, F_LEFT(f1s))
        case _ => if (r1s == r2s) (r1s, F_LEFT(f1s))
        else (ALT (r1s, r2s), F_ALT(f1s, f2s))
      }
    }
    case SEQ(r1, r2) => {
      val (r1s, f1s) = simp(r1)
      val (r2s, f2s) = simp(r2)
      (r1s, r2s) match {
        case (NULL, _) => (NULL, F_ERROR)
        case (_, NULL) => (NULL, F_ERROR)
        case (EMPTY, _) => (r2s, F_SEQ_Empty1(f1s, f2s))
        case (_, EMPTY) => (r1s, F_SEQ_Empty2(f1s, f2s))
        case _ => (SEQ(r1s,r2s), F_SEQ(f1s, f2s))
      }
    }
    case RECD(x, r1) => {
      val (r1s, f1s) = simp(r1)
      (RECD(x, r1s), F_RECD(f1s))
    }

    case r => (r, F_ID)
  }

  def lex_simp(r: Rexp, s: List[Char]) : Val = s match {
    case Nil => if (nullable(r)) mkeps(r) else throw new Exception("Not matched")
    case c::cs => {
      val (r_simp, f_simp) = simp(der(c, r))
      inj(r, c, f_simp(lex_simp(r_simp, cs)))
    }
  }

  def lexing_simp(r: Rexp, s: String) : Val = lex_simp(r, s.toList)

  lexing_simp(("a" | "ab") ~ ("b" | ""), "ab")


  def test(): Unit = {
    // Lexing Rules for a Small While Language

    def PLUS_LEX(r: Rexp) = r ~ r.%
    val SYM = "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"
    val DIGIT = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
    val UNDERSCORE = "_"
    val ID = SYM ~ (SYM | DIGIT | UNDERSCORE).%
    val NUM = PLUS_LEX(DIGIT)
    val KEYWORD : Rexp = "skip" | "while" | "do" | "if" | "then" | "else" | "read" | "write" | "true" | "false" | "for" | "to"
    val SEMI: Rexp = ";"
    val OP: Rexp = ":=" | "==" | "-" | "+" | "*" | "!=" | "<" | ">" | "<=" | ">=" | "%" | "/"
    val WHITESPACE = PLUS_LEX(" " | "\n" | "\t")
    val RPAREN: Rexp = ")"
    val LPAREN: Rexp = "("
    val BEGIN: Rexp = "{"
    val END: Rexp = "}"
    val STRING: Rexp = "\"" ~ SYM.% ~ "\""



    val WHILE_REGS = (("k" $ KEYWORD) |
      ("i" $ ID) |
      ("o" $ OP) |
      ("n" $ NUM) |
      ("s" $ SEMI) |
      ("str" $ STRING) |
      ("p" $ (LPAREN | RPAREN)) |
      ("b" $ (BEGIN | END)) |
      ("w" $ WHITESPACE)).%


    println("CW2 Question 2 ->")
    val question2 = "read n;"
    println(env(lexing_simp(WHILE_REGS, question2)).filter(f=>f._1 != "w"))


    println("\nCW2 Question3 Fig 1 ->")
    val figure1 = """
write "fib";
read n;
minus2 := 1;
while n > 0 do {
temp := minus2;
minus2 := minus1 + minus2;
minus1 := temp;
n := n - 1
};
write "result";
write minus2 """
    println(env(lexing_simp(WHILE_REGS ,figure1)).filter(f=>f._1 != "w"))





  }

  def whileProgram(str: String): List[(String, String)] = {


    def PLUS_LEX(r: Rexp) = r ~ r.%
    val SYM = "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" |
      "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" | ":" | "_"
    val DIGIT = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
    val UNDERSCORE = "_"
    val ID = SYM ~ (SYM | DIGIT | UNDERSCORE).%
    val NUM = PLUS_LEX(DIGIT)
    val KEYWORD : Rexp = "skip" | "while" | "do" | "if" | "then" | "else" | "read" | "write" | "true" | "false" | "for" | "upto"
    val SEMI: Rexp = ";"
    val OP: Rexp = ":=" | "==" | "-" | "+" | "*" | "!=" | "<" | ">" | "<=" | ">=" | "%" | "/"
    val WHITESPACE = PLUS_LEX(" " | "\n" | "\t")
    val RPAREN: Rexp = ")"
    val LPAREN: Rexp = "("
    val BEGIN: Rexp = "{"
    val END: Rexp = "}"
    val STRING: Rexp = "\"" ~ SYM.% ~ "\""



    val WHILE_REGS = (
      ("k" $ KEYWORD) |
      ("i" $ ID) |
      ("o" $ OP) |
      ("n" $ NUM) |
      ("s" $ SEMI) |
      ("str" $ STRING) |
      ("p" $ (LPAREN | RPAREN)) |
      ("b" $ (BEGIN | END)) |
      ("w" $ WHITESPACE)).%



//    println("\nCW2 Question 3 Fig 2 ->")
//    val figure2 = """
//start := 1000;
//x := start;
//y := start;
//z := start; while 0 < x do {
//while 0 < y do {
//while 0 < z do { z := z - 1 }; z := start;
//y := y - 1
//};
//y := start; x := x - 1
//}
//                  """
//    println(env(lexing_simp(WHILE_REGS ,figure2)).filter(f=>f._1 != "w"))

//    println("\nCoursework 3 Q2 ->")
//    val cw3q2 = "if (a < b) then skip else a := a * b + 1"
//    val cw3q2 = "{x:=5;x:=7}"
    return env(lexing_simp(WHILE_REGS ,str)).filter(f=>f._1 != "w")

  }
}
