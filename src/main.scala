import scala.language.{implicitConversions, reflectiveCalls}
import Compile._
import Lexing._
import Tokens._

/**
 * Created by Clarence on 11/2/15.
 */

object main extends App {

  def tokenize(l: List[(String, String)]): List[TOKEN] = l match {

    case Nil => List()

    case head :: tail => head match {

      case ("k", keyword) => TOK_KEYWORD(keyword) :: tokenize(tail)

      case ("i", variable) => TOK_VAR(variable) :: tokenize(tail)

      case ("o", operator) => TOK_OP(operator) :: tokenize(tail)

      case ("n", number) => TOK_NUM(number.toInt) :: tokenize(tail)

      case ("s", semicolon) => TOK_SEMI :: tokenize(tail)

      case ("str", string) => TOK_STR(string) :: tokenize(tail)

      case ("p", parenthesis) => TOK_PAR(parenthesis) :: tokenize(tail)

      case ("b", curlybracket) => TOK_CURL(curlybracket) :: tokenize(tail)  // Begin | End

      case ("w", whitespace) => TOK_WHITE :: tokenize(tail)

    }

  }


  def compile(code: String) {

    val listOfTuples = Lexing.whileProgram(code)  // env
    val tokens = tokenize(listOfTuples)
    println(tokens) // tokenize


    println()
    val parseTree = Parsing.Block.parse_all(tokens)
    println(parseTree)
    println()

    println("Interpreter")
    val evaluation = Parsing.eval(parseTree.head)
    println(evaluation)

    println("Compile")
    println(Compile.compile(parseTree.head, "fib"))

  }

  override def main(args: Array[String]) {

    def time[R](block: => R): R = {
      val t0 = System.nanoTime()
      val result = block    // call-by-name
      val t1 = System.nanoTime()
      println()
      println(">>>>> Running Time: " + (t1 - t0) + "nanoseconds <<<<<")
      result
    }


    val result = time {
      val fib = """
      {write "Enter_A_Number:";
      read n;
      minus1 := 0;
      minus2 := 1;
      while n > 0 do {
      temp := minus2;
      minus2 := minus1 + minus2;
      minus1 := temp;
      n := n - 1
      };
      write "result";
      write minus2}"""

      val factorial = """
      {start := 10;
      x := start;
      y := start;
      z := start; while 0 < x do {
      while 0 < y do {
      while 0 < z do { z := z - 1 }; z := start;
      y := y - 1
      };
      y := start; x := x - 1
      }}"""


//      println(">>>>> Fibonacci Number <<<<<")
//      compile(fib)
//
//      println(">>>>> Factorial Number <<<<<")
      compile(factorial)

      val forloop = """
        {for i := 2 upto 4 do {
        write i
        }}"""
      compile(forloop)
    }

  }


}
