package structures

import scala.annotation.tailrec


case class Token(classifier: String)


class Lexer {

  type Lexed = (String, Token)

  def lex(chars: String): List[Lexed] = {
    val max = chars.size

    def loop(i: Int, l: List[Lexed]): List[Lexed] = {
      if(i<max) {
        chars(i) match {
          case cur if cur.isDigit=> {
            val lastInt = lastIntIndex(i, chars)
            val newString = chars.substring(i, lastInt)
            val pair = (newString, Token("number"))
            loop(lastInt, l :+ pair)
          }
          case cur if isOperator(cur)=> {
            val pair = (cur.toString, Token(returnOp(cur.toString)))
            loop(i + 1, l :+ pair)
          }
          case _ => List() // user failed to indicate proper sentence structure
        }
      }
      else {
        l
      }
    }
    val firstPass = loop(0, List())
    lex2(parenthesize(firstPass))
  }


  def parenthesize(tokens: List[Lexed]): String = {
    println(tokens)
    @tailrec
    def loop(parens: String, l: List[Lexed], pFound: Boolean, prevParen: Int, lastR1Pos: Int): String = l match {
      case h :: Nil => {
        val num = h._1
        if(pFound) parens+num+")" else parens+num
      }
      case h :: h2 :: tail => {
        val (num, op) = (h._1, h2._1)
        val (tokenNum, tokenOp) = (h._2.classifier, h2._2.classifier)
        if(!pFound && tokenNum=="number" && tokenOp =="r1") {
          val newParens = parens+"("+num+op
          loop(newParens, tail, true, lastR1Pos, lastR1Pos)
        }
        else if(pFound && tokenNum=="number" && tokenOp =="r2") {
          val newParens = parens+num+")"+op
          loop(newParens, tail, false, prevParen, newParens.size)
        }

        else if(pFound && tokenNum=="number" && tokenOp=="r1") {
          val newParens = rebuild(parens, prevParen)+num+")"+op
          loop(newParens, tail, true, prevParen, lastR1Pos)
        }

        else if(!pFound && tokenNum=="number" && tokenOp =="r2") {
          loop(parens+num+op, tail, false, prevParen, lastR1Pos)
        }
        else {
          println("hopefully no run")
          parens
        } // dno
      }
      case _ => parens
    }
    val finishedP = loop("", tokens, false, 0, 0)
    println(finishedP)
    finishedP
  }

  def lex2(chars: String): List[Lexed] = {
    val max = chars.size
    def loop(i: Int, l: List[Lexed]): List[Lexed] = {
      if(i<max) {
        chars(i) match {
          case c if c=='(' => {
            val pair = (c.toString, Token("o_parenthesis"))
            loop(i+1, l :+ pair)
          }
          case c if c.isDigit => {
            val lastInt = lastIntIndex(i, chars)
            val newString = chars.substring(i, lastInt)
            val pair = (newString, Token("number"))
            loop(lastInt, l :+ pair)
          }
          case c if c==')' => {
            val pair = (")", Token("c_parenthesis"))
            loop(i+1, l :+ pair)
          }
          case cur if isOperator(cur)=> {
            val pair = (cur.toString, Token(returnOp(cur.toString)))
            loop(i + 1, l :+ pair)
          }
        }
      }
      else {
        l
      }
    }
    loop(0, List())
  }

  def rebuild(s: String, i: Int): String =
    s.patch(i, "(", 0)

  def returnOp(c: String): String = c match {
    case "+" => "r2"
    case "-" => "r2"
    case "/" => "r1"
    case "*" => "r1"
    case _ => "ERROR"
  }

  def isR2(c: Char): Boolean = c match {
    case '+' => true
    case '-' => true
    case _ => false
  }

  def isR1(c: Char): Boolean = {
    if(c=='*') true
    else if(c=='/') true
    false
  }


  def isOperator(c: Char): Boolean = {
    if(c=='+') true
    else if(c=='-') true
    else if(c=='/') true
    else if(c=='*') true
    else false
  }


  def buildParens(chars: String, start: Int): String = {
    var newChars = chars
    var ctr = 0

    for(i <- start to lastPIndex(chars, start)-3 by 2) {
      val cur = chars(i)
      val next = chars(i+1)
      // 5*4*3+4 | (5*4*3+4
      // 5*4*3+4 | (5
      newChars = newChars.patch(0, "(", 0)
      ctr+=1
      newChars = newChars.patch(nextCloseParen(newChars, i+1+ctr), ")", 0)
    }
    newChars
  }

  def nextCloseParen(chars: String, start: Int): Int = {

    for(i <- start to chars.size-1) {
      val cur = chars(i)
      if(cur.isDigit) {

        for(j <- i to chars.size-1) {
          val next = chars(j)
          if(!next.isDigit) {
            return j
          }
        }
      }
    }
    chars.size-1
  }

  def lastPIndex(chars: String, start: Int): Int = {
    for(i <- start to chars.size-1) {
      val cur = chars(i)

      if(cur=='+') {
        i
      }
    }
    chars.size-1
  }


  def lastIntIndex(first: Int, chars: String): Int = {
    val size = chars.length

    for(i <- first to size-1) {

      val cur = chars(i)
      try {
        cur.toString.toInt
      } catch {
        case e: NumberFormatException => {
          return i
        }
      }


    }
    chars.size
  }

}

