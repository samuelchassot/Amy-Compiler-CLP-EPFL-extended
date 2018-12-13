package amyc
package parsing

import grammarcomp.parsing._
import utils.Positioned
import ast.NominalTreeModule._
import Tokens._
import amyc.ast.NominalTreeModule

// Implements the translation from parse trees to ASTs for the LL1 grammar,
// that is, this should correspond to Parser.amyGrammarLL1.
// We extend the plain ASTConstructor as some things will be the same -- you should
// override whatever has changed. You can look into ASTConstructor as an example.
class ASTConstructorLL1 extends ASTConstructor {

  @Override
  override def constructQname(pTree: NodeOrLeaf[Token]): (QualifiedName, Positioned) = {
    pTree match {
      case Node('QName ::= _, List(id, opt)) => {

        opt match {
          case Node('QNameOpt ::= _, List(_, opt)) => {
            val (module, modPos) = constructName(id)
            val (name, namePos) = constructName(opt)
            (QualifiedName(Some(module), name), modPos)
          }
          case Node('QNameOpt ::= _, List()) => {
            val (name, pos) = constructName(id)
            (QualifiedName(None, name), pos);
          }
        }
      }
    }
  }

  @Override
  override def constructExpr(ptree: NodeOrLeaf[Token]): (NominalTreeModule.Expr, Option[List[ClassOrFunDef]]) = {
    ptree match {
      case Node('Expr ::= List('StartVal), List(startValTree)) => constructStartVal(startValTree)
      case Node('Expr ::= List('lvl01), List(lvl01)) => constructExprLvl01(lvl01)
    }
  }

  def constructStartVal(ptree: NodeOrLeaf[Token]): (NominalTreeModule.Expr, Option[List[ClassOrFunDef]]) = {
    ptree match {
      case Node('StartVal ::= (VAL() :: _), List(Leaf(vt), param, _, valueTree, _, expr)) =>
        val valueExprAndDef = constructExprLvl02(valueTree)
        val exprExprAndDef = constructExpr(expr)
        (Let(constructParam(param),valueExprAndDef._1 , exprExprAndDef._1).setPos(vt), toOpionalList(valueExprAndDef._2, exprExprAndDef._2))
    }
  }

  def constructExprLvl01(ptree: NodeOrLeaf[Token]): (NominalTreeModule.Expr, Option[List[ClassOrFunDef]]) = {
    ptree match {
      case Node('lvl01 ::= _, List(lvl02Tree, lvl01optTree)) => {
        lvl01optTree match {
          case Node('lvl01opt ::= (SEMICOLON() :: _), List(Leaf(sct), lvl01optoptTree)) =>
            val expr1 = constructExprLvl02(lvl02Tree)
            lvl01optoptTree match {
              case Node('lvl01optopt ::= List('StartVal), List(startValTree)) => {
                val expr2 = constructStartVal(startValTree)
                (Sequence(expr1._1, expr2._1).setPos(expr1._1), toOpionalList(expr1._2, expr2._2))
              }
              case Node('lvl01optopt ::= List('lvl01), List(lvl01Tree)) => {
                val expr2 = constructExprLvl01(lvl01Tree)
                (Sequence(expr1._1, expr2._1).setPos(expr1._1), toOpionalList(expr1._2, expr2._2))
              }
            }
          case Node('lvl01opt ::= List(), List()) =>
            constructExprLvl02(lvl02Tree)
        }
      }
    }
  }

  def constructExprLvl02(ptree: NodeOrLeaf[Token]): (NominalTreeModule.Expr, Option[List[ClassOrFunDef]]) = {
    ptree match {
      case Node('lvl02 ::= List('lvl03, 'lvl02opt), List(lvl03Tree, lvl02optTree)) => {
        lvl02optTree match {
          case Node('lvl02opt ::= (MATCH() :: _), List(Leaf(mt), _, casesTree, _)) =>
            Match(constructExprLvl03(lvl03Tree), constructCases(casesTree))
          case Node('lvl02opt ::= List(), List()) =>
            constructExprLvl03(lvl03Tree)
        }
      }
    }
  }

  def constructExprLvl03(ptree: NodeOrLeaf[Token]): (NominalTreeModule.Expr, Option[List[ClassOrFunDef]]) = {
    ptree match {
      case Node('lvl03 ::= _, List(lvl04Tree, lvl03Opt)) =>
        val (e1, f1) = constructExprLvl04(lvl04Tree)
        (constructOpExpr(e1 , lvl03Opt, constructExprLvl04), f1)
    }
  }

  def constructExprLvl04(ptree: NodeOrLeaf[Token]): (NominalTreeModule.Expr, Option[List[ClassOrFunDef]]) = {
    ptree match {
      case Node('lvl04 ::= _, List(lvl05Tree, lvl04Opt)) =>
        constructOpExpr(constructExprLvl05(lvl05Tree), lvl04Opt, constructExprLvl05)
    }
  }

  def constructExprLvl05(ptree: NodeOrLeaf[Token]): (NominalTreeModule.Expr, Option[List[ClassOrFunDef]]) = {
    ptree match {
      case Node('lvl05 ::= _, List(lvl06Tree, lvl05Opt)) =>
        constructOpExpr(constructExprLvl06(lvl06Tree), lvl05Opt, constructExprLvl06)
    }
  }

  def constructExprLvl06(ptree: NodeOrLeaf[Token]): (NominalTreeModule.Expr, Option[List[ClassOrFunDef]]) = {
    ptree match {
      case Node('lvl06 ::= _, List(lvl07Tree, lvl06Opt)) =>
        constructOpExpr(constructExprLvl07(lvl07Tree), lvl06Opt, constructExprLvl07)
    }
  }

  def constructExprLvl07(ptree: NodeOrLeaf[Token]): (NominalTreeModule.Expr, Option[List[ClassOrFunDef]]) = {
    ptree match {
      case Node('lvl07 ::= _, List(lvl08Tree, lvl07Opt)) =>
        constructOpExpr(constructExprLvl08(lvl08Tree), lvl07Opt, constructExprLvl08)
    }
  }

  def constructExprLvl08(ptree: NodeOrLeaf[Token]): (NominalTreeModule.Expr, Option[List[ClassOrFunDef]]) = {
    ptree match {
      case Node('lvl08 ::= _, List(lvl09Tree, lvl08Opt)) =>
        constructOpExpr(constructExprLvl09(lvl09Tree), lvl08Opt, constructExprLvl09)
    }
  }

  def constructExprLvl09(ptree: NodeOrLeaf[Token]): (NominalTreeModule.Expr, Option[List[ClassOrFunDef]]) = {
    ptree match {
      case Node('lvl09 ::= List(BANG(), 'lvl10), List(Leaf(bt), lvl10Tree)) =>
        Not(constructExprLvl10(lvl10Tree))
      case Node('lvl09 ::= List(MINUS(), 'lvl10), List(Leaf(mt), lvl10Tree)) =>
        Neg(constructExprLvl10(lvl10Tree))
      case Node('lvl09 ::= List('lvl10), List(lvl10Tree)) =>
        constructExprLvl10(lvl10Tree)

    }
  }

  def constructExprLvl10(ptree: NodeOrLeaf[Token]): (NominalTreeModule.Expr, Option[List[ClassOrFunDef]]) = {
    ptree match {
      case Node('lvl10 ::= (IF() :: _), List(Leaf(it), _, cond, _, _, thenn, _, _, _, elze, _)) =>
        Ite(
          constructExpr(cond),
          constructExpr(thenn),
          constructExpr(elze)
        ).setPos(it)
      case Node('lvl10 ::= (ERROR() :: _), List(Leaf(ert), _, msg, _)) =>
        Error(constructExpr(msg)).setPos(ert)
      case Node('lvl10 ::= List('Id, 'IdExprOpt), List(id, idOpt)) => {
        idOpt match {
          case Node('IdExprOpt ::= ('QNameOpt :: _), List(qnameopt, _, argsTree, _)) => {
            qnameopt match {
              case Node('QNameOpt ::= (DOT() :: _), List(_, nameTree)) => {
                val (module, modPos) = constructName(id)
                val (name, namePos) = constructName(nameTree)
                val qname = QualifiedName(Some(module), name)
                val args = constructList(argsTree, constructExpr, hasComma = true)
                Call(qname, args).setPos(modPos)
              }
              case Node('QNameOpt ::= List(), List()) => {
                val (name, namePos) = constructName(id)
                val qname = QualifiedName(None, name)
                val args = constructList(argsTree, constructExpr, hasComma = true)
                Call(qname, args).setPos(namePos)
              }
            }
          }
          case Node('IdExprOpt ::= List(), List()) =>
            val (name, pos) = constructName(id)
            (Variable(name).setPos(pos), None)
        }
      }

      case Node('lvl10 ::= List('LiteralWithoutUnit), List(lit)) =>
        (constructLiteral(lit), None)

      case Node('lvl10 ::= List('Parenthesized), List(parenth)) =>
        parenth match {
          case Node('Parenthesized ::= _, List(_, parentOpt)) => {
            parentOpt match {
              case Node('ParenOpt ::= List(RPAREN()), List(Leaf(rpt))) =>
                //Literal unit value
                (UnitLiteral().setPos(rpt), None)
              case Node('ParenOpt ::= List('Expr, RPAREN()), List(expr, Leaf(rpt))) => {
                val (e, f) = constructExpr(expr)
                (e.setPos(rpt), f)
              }
            }
          }
        }
    }
  }

  override def constructLiteral(pTree: NodeOrLeaf[Token]): Literal[_] = {
    pTree match {
      case Node('LiteralWithoutUnit ::= List(INTLITSENT), List(Leaf(it@INTLIT(i)))) =>
        IntLiteral(i).setPos(it)
      case Node('LiteralWithoutUnit ::= List(STRINGLITSENT), List(Leaf(st@STRINGLIT(s)))) =>
        StringLiteral(s).setPos(st)
      case Node('LiteralWithoutUnit ::= _, List(Leaf(tt@TRUE()))) =>
        BooleanLiteral(true).setPos(tt)
      case Node('LiteralWithoutUnit ::= _, List(Leaf(tf@FALSE()))) =>
        BooleanLiteral(false).setPos(tf)
    }
  }


  @Override
  override def constructPattern(pTree: NodeOrLeaf[Token]): NominalTreeModule.Pattern = {
    pTree match {
      case Node('Pattern ::= List(UNDERSCORE()), List(Leaf(ut))) =>
        WildcardPattern().setPos(ut)
      case Node('Pattern ::= List('LiteralWithoutUnit), List(lit)) =>
        val literal = constructLiteral(lit)
        LiteralPattern(literal).setPos(literal)
      case Node('Pattern ::= List(LPAREN(), RPAREN()), List(Leaf(lpt),_)) =>
        val literal = UnitLiteral().setPos(lpt)
        LiteralPattern(literal).setPos(literal)
      case Node('Pattern ::= List('Id, 'IdPatternOpt), List(id, idPatOpt)) =>
        idPatOpt match {
          case Node('IdPatternOpt ::= List(), List()) => {
            val (name, pos) = constructName(id)
            IdPattern(name).setPos(pos)
          }
          case Node('IdPatternOpt ::= List('QNameOpt, LPAREN(), 'Patterns, RPAREN()), List(qnameopt, _, patternsTree, _)) => {
            qnameopt match {
              case Node('QNameOpt ::= (DOT() :: _), List(_, nameTree)) => {
                val (module, modPos) = constructName(id)
                val (name, namePos) = constructName(nameTree)
                val qname = QualifiedName(Some(module), name)
                val patterns = constructList(patternsTree, constructPattern, hasComma = true)
                CaseClassPattern(qname, patterns).setPos(modPos)
              }
              case Node('QNameOpt ::= List(), List()) => {
                val (name, namePos) = constructName(id)
                val qname = QualifiedName(None, name)
                val patterns = constructList(patternsTree, constructPattern, hasComma = true)
                CaseClassPattern(qname, patterns).setPos(namePos)
              }
            }
          }
        }
    }

  }

  override def constructCase(pTree: NodeOrLeaf[Token]): (MatchCase, Option[List[ClassOrFunDef]]) = {
    pTree match {
      case Node('Case ::= _, List(Leaf(ct), pat, _, expr)) =>
        val (e, f) = constructExpr(expr)
        (MatchCase(constructPattern(pat), e).setPos(ct), f)
    }
  }

  def constructCases(ptree : NodeOrLeaf[Token]) : (List[MatchCase], Option[List[ClassOrFunDef]]) ={
    ptree match{
      case Node('Cases ::= _, List(caseTree, casesOpt)) => {
        casesOpt match{
          case Node('CasesOpt ::= List(), List()) =>
            val (c, f) = constructCase(caseTree)
            (c :: Nil, f)
          case Node('CasesOpt ::= List('Cases), List(casesTree)) =>
            val (c1, f1) = constructCase(caseTree)
            val (c2, f2) = constructCases(casesTree)
            (c1 :: c2, toOpionalList(f1, f2))
        }
      }

    }
  }

  override def constructList1[A](ptree: NodeOrLeaf[Token], constructor: NodeOrLeaf[Token] => A, hasComma: Boolean = false): List[A] = {
    ptree match {
      case Node(_, List(t)) => List(constructor(t))
      case Node(_, List(t, ts)) =>
        constructor(t) :: constructList1(ts, constructor, hasComma)
      case Node(_, List(t, Leaf(COMMA()), ts)) if hasComma =>
        constructor(t) :: constructList1(ts, constructor, hasComma)
    }
  }


  override def constructOp(ptree: NodeOrLeaf[Token]): (Expr, Expr) => Expr = {
    (ptree: @unchecked) match {
      case Leaf(t) => tokenToExpr(t)
    }
  }


  // Important helper method:
  // Because LL1 grammar is not helpful in implementing left associativity,
  // we give you this method to reconstruct it.
  // This method takes the left operand of an operator (leftopd)
  // as well as the tree that corresponds to the operator plus the right operand (ptree)
  // It parses the right hand side and then reconstruct the operator expression
  // with correct associativity.
  // If ptree is empty, it means we have no more operators and the leftopd is returned.
  // Note: You may have to override constructOp also, depending on your implementation
  def constructOpExpr(leftopd: Expr, ptree: NodeOrLeaf[Token], constrFun: NodeOrLeaf[Token] => (Expr, Option[ClassOrFunDef]): Expr = {
    ptree match {
      case Node(_, List()) => //epsilon rule of the nonterminals
        leftopd
      case Node(sym ::= _, List(op, rightNode))
        if Set('lvl03opt, 'lvl04opt, 'lvl05opt, 'lvl06opt, 'lvl07opt, 'lvl08opt) contains sym =>
        rightNode match {
          case Node(_, List(nextOpd, optExpr)) => // 'Expr? ::= Expr? ~ 'OpExpr,
            val nextAtom = constrFun(nextOpd)
            constructOpExpr(constructOp(op)(leftopd, nextAtom).setPos(leftopd), optExpr, constrFun) // captures left associativity
        }
    }
  }

  def toOpionalList(o1 : Option[List[ClassOrFunDef]], o2 : Option[List[ClassOrFunDef]]) : Option[List[ClassOrFunDef]] = {
    val l = o1.getOrElse(Nil) ++ o2.getOrElse(Nil)
    if(l.isEmpty) None else Some(l)
  }

}

