package structures

class Parser {

  type Lexed = (String, Token)

  // 2-5*75/5/2
  def parse(tokens: List[Lexed]): Expr = {
    def loop(l: List[Lexed]): Expr = l match {
      case op :: paren :: tail if ( isR2Op(op) && isParenthesis(paren)) => {
        val excludedInner = excludeInners(paren :: tail)
        val excludedOuter = excludeOuters(paren::tail)
        val inners = gatherInnerExpr(excludedOuter)
        BinOp(op._1, inners, loop(excludedInner))
      }
      case h1 :: _ :: tail if isParenthesis(h1) => {
        BinOp("+", gatherInnerExpr(excludeOuters(l)), loop(excludeInners(l))) // uses the parameter l here b/c h1="("
      }
      case op :: p :: tail if isParenthesis(p) => {
        val excludedInner = excludeInners(p :: tail)
        BinOp(op._1, gatherInnerExpr(excludeOuters(l)), loop(excludedInner))
      }
      case h1  :: h2 :: Nil => {
        if(h1._1=="+") {
          return Literal(h2._1)
        }
        Literal("-"+h2._1)
      }
      case last :: Nil => Literal(last._1)
      case op :: num :: tail if isR2Op(op) => {
        BinOp(op._1, Literal(num._1), loop(tail))
      }
      case num :: op :: num2 :: tail if ( isR2Op(op) && num2._1.forall(Character.isDigit) ) => {
        BinOp("+", BinOp(op._1, Literal(num._1), Literal(num2._1)), loop(tail))
      }
      case num :: op :: tail => {
        BinOp(op._1, Literal(num._1), loop(tail))
      }
      case _ => Zero
    }
    loop(tokens)
  }

  // ((5*4)*2) + (3*3)
  // returns list of tokens after done with gathering inner expr

  def excludeInners(l: List[Lexed]): List[Lexed] = {
    def loop(l: List[Lexed], encountered: Int): List[Lexed] = l match {
      case _ if encountered<1 => l
      case h :: t if h._1=="(" => loop(t, encountered+1)
      case h :: t if h._1==")" => loop(t, encountered-1)
      case _ :: t => loop(t, encountered)
    }
    loop(l.tail, 1)
  }

  def excludeOuters(l: List[Lexed]): List[Lexed] = {
    def loop(l: List[Lexed], encountered: Int, build: List[Lexed]): List[Lexed] = {
      l match {
        case _ if encountered < 1 => build
        case h :: t if h._1 == "(" => loop(t, encountered + 1, build)
        case h :: t if h._1 == ")" => loop(t, encountered - 1, build)
        case h :: t => loop(t, encountered, build :+ h)
        case Nil => Nil // wat do here
      }
    }
    loop(l.tail, 1, List(l.head))
  }

  def gatherInnerExpr(sublist: List[Lexed]): Expr = {
    val filteredPs = sublist.filter{
      (lexed: Lexed) =>
        (lexed._2.classifier!="c_parenthesis" && lexed._2.classifier!="o_parenthesis")
    }

    def loop(l: List[Lexed]): Expr = {
      l match {
        case h :: _ if isParenthesis(h) => Zero
        case h :: h2 :: tail => {
          val (num, op) = (h._1, h2._1)
          BinOp(op, loop(tail), Literal(num))
        }
        case h :: Nil => Literal(h._1)
        case _ => Zero // this shouldn't run i think
      }
    }
    loop(filteredPs.reverse)
  }


  def parenthesesLastIndex(start: Int, l: List[Lexed]): Int = {
    val max = l.size
    var encountered = 0

    for( i <- start to max-1 ) {
      val curLexed: Lexed = l(i)
      val curToken = curLexed._2.classifier
      if(curToken=="c_parenthesis" && encountered>0) {
        encountered -= 1
      }
      else if(curToken=="o_parenthesis") {
        encountered += 1
      }
      if(curToken=="c_parenthesis" && encountered==0) {
        return i
      }
    }

    max
  }

  def isParenthesis(lexed: Lexed): Boolean = lexed._2 match {
    case cur if cur.classifier=="c_parenthesis" => true
    case cur if cur.classifier=="o_parenthesis" => true
    case _ => false
  }

  def isR2Op(lexed: Lexed): Boolean = lexed._2 match {
    case cur if cur.classifier=="r2" => true
    case _ => false
  }
}

