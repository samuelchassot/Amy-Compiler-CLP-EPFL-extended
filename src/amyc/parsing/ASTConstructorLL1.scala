package amyc
package parsing

import grammarcomp.parsing._
import utils.Positioned
import ast.NominalTreeModule._
import Tokens._
import amyc.ast.{Identifier, NominalTreeModule}

// Implements the translation from parse trees to ASTs for the LL1 grammar,
// that is, this should correspond to Parser.amyGrammarLL1.
// We extend the plain ASTConstructor as some things will be the same -- you should
// override whatever has changed. You can look into ASTConstructor as an example.
class ASTConstructorLL1 extends ASTConstructor {

  object FunctionId {
    private var index = 1

    def fresh(): String = {
      val name = "++listComprDesuggar" + index
      index += 1
      name
    }
  }

  object ListId {
    private var index = 1

    def fresh(): String = {
      val name = "--ListId" + index
      index += 1
      name
    }
  }

  override def constructQname(
      pTree: NodeOrLeaf[Token]
  ): (QualifiedName, Positioned) = {
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
            (QualifiedName(None, name), pos)
          }
        }
      }
    }
  }

  override def constructExpr(
      ptree: NodeOrLeaf[Token]
  ): (NominalTreeModule.Expr, Option[List[ClassOrFunDef]]) = {
    ptree match {
      case Node('Expr ::= List('StartVal), List(startValTree)) =>
        constructStartVal(startValTree)
      case Node('Expr ::= List('lvl01), List(lvl01)) =>
        constructExprLvl01(lvl01)
    }
  }

  def constructStartVal(
      ptree: NodeOrLeaf[Token]
  ): (NominalTreeModule.Expr, Option[List[ClassOrFunDef]]) = {
    ptree match {
      case Node(
          'StartVal ::= (VAL() :: _),
          List(Leaf(vt), param, _, valueTree, _, expr)
          ) =>
        val valueExprAndDef = constructExprLvl02(valueTree)
        val exprExprAndDef = constructExpr(expr)
        (
          Let(constructParam(param), valueExprAndDef._1, exprExprAndDef._1)
            .setPos(vt),
          toOptionalList(valueExprAndDef._2, exprExprAndDef._2)
        )
    }
  }

  def constructExprLvl01(
      ptree: NodeOrLeaf[Token]
  ): (NominalTreeModule.Expr, Option[List[ClassOrFunDef]]) = {
    ptree match {
      case Node('lvl01 ::= _, List(lvl02Tree, lvl01optTree)) => {
        lvl01optTree match {
          case Node(
              'lvl01opt ::= (SEMICOLON() :: _),
              List(Leaf(sct), lvl01optoptTree)
              ) =>
            val expr1 = constructExprLvl02(lvl02Tree)
            lvl01optoptTree match {
              case Node('lvl01optopt ::= List('StartVal), List(startValTree)) => {
                val expr2 = constructStartVal(startValTree)
                (
                  Sequence(expr1._1, expr2._1).setPos(expr1._1),
                  toOptionalList(expr1._2, expr2._2)
                )
              }
              case Node('lvl01optopt ::= List('lvl01), List(lvl01Tree)) => {
                val expr2 = constructExprLvl01(lvl01Tree)
                (
                  Sequence(expr1._1, expr2._1).setPos(expr1._1),
                  toOptionalList(expr1._2, expr2._2)
                )
              }
            }
          case Node('lvl01opt ::= List(), List()) =>
            constructExprLvl02(lvl02Tree)
        }
      }
    }
  }

  def constructExprLvl02(
      ptree: NodeOrLeaf[Token]
  ): (NominalTreeModule.Expr, Option[List[ClassOrFunDef]]) = {
    ptree match {
      case Node(
          'lvl02 ::= List('lvl03, 'lvl02opt),
          List(lvl03Tree, lvl02optTree)
          ) => {
        lvl02optTree match {
          case Node(
              'lvl02opt ::= (MATCH() :: _),
              List(Leaf(mt), _, casesTree, _)
              ) =>
            val (e, fe) = constructExprLvl03(lvl03Tree)
            val (c, fc) = constructCases(casesTree)
            (Match(e, c), toOptionalList(fe, fc))
          case Node('lvl02opt ::= List(), List()) =>
            constructExprLvl03(lvl03Tree)
        }
      }
    }
  }

  def constructExprLvl03(
      ptree: NodeOrLeaf[Token]
  ): (NominalTreeModule.Expr, Option[List[ClassOrFunDef]]) = {
    ptree match {
      case Node('lvl03 ::= _, List(lvl04Tree, lvl03Opt)) =>
        val (e, f) = constructExprLvl04(lvl04Tree)
        val leftAssociated =
          constructOpExpr(e, lvl03Opt, None, constructExprLvl04)
        (leftAssociated._1, toOptionalList(leftAssociated._2, f))
    }
  }

  def constructExprLvl04(
      ptree: NodeOrLeaf[Token]
  ): (NominalTreeModule.Expr, Option[List[ClassOrFunDef]]) = {
    ptree match {
      case Node('lvl04 ::= _, List(lvl05Tree, lvl04Opt)) =>
        val (e, f) = constructExprLvl05(lvl05Tree)
        val leftAssociated =
          constructOpExpr(e, lvl04Opt, None, constructExprLvl05)
        (leftAssociated._1, toOptionalList(leftAssociated._2, f))
    }
  }

  def constructExprLvl05(
      ptree: NodeOrLeaf[Token]
  ): (NominalTreeModule.Expr, Option[List[ClassOrFunDef]]) = {
    ptree match {
      case Node('lvl05 ::= _, List(lvl06Tree, lvl05Opt)) =>
        val (e, f) = constructExprLvl06(lvl06Tree)
        val leftAssociated =
          constructOpExpr(e, lvl05Opt, None, constructExprLvl06)
        (leftAssociated._1, toOptionalList(leftAssociated._2, f))
    }
  }

  def constructExprLvl06(
      ptree: NodeOrLeaf[Token]
  ): (NominalTreeModule.Expr, Option[List[ClassOrFunDef]]) = {
    ptree match {
      case Node('lvl06 ::= _, List(lvl07Tree, lvl06Opt)) =>
        val (e, f) = constructExprLvl07(lvl07Tree)
        val leftAssociated =
          constructOpExpr(e, lvl06Opt, None, constructExprLvl07)
        (leftAssociated._1, toOptionalList(leftAssociated._2, f))
    }
  }

  def constructExprLvl07(
      ptree: NodeOrLeaf[Token]
  ): (NominalTreeModule.Expr, Option[List[ClassOrFunDef]]) = {
    ptree match {
      case Node('lvl07 ::= _, List(lvl08Tree, lvl07Opt)) =>
        val (e, f) = constructExprLvl08(lvl08Tree)
        val leftAssociated =
          constructOpExpr(e, lvl07Opt, None, constructExprLvl08)
        (leftAssociated._1, toOptionalList(leftAssociated._2, f))

    }
  }

  def constructExprLvl08(
      ptree: NodeOrLeaf[Token]
  ): (NominalTreeModule.Expr, Option[List[ClassOrFunDef]]) = {
    ptree match {
      case Node('lvl08 ::= _, List(lvl09Tree, lvl08Opt)) =>
        val (e, f) = constructExprLvl09(lvl09Tree)
        val leftAssociated =
          constructOpExpr(e, lvl08Opt, None, constructExprLvl09)
        (leftAssociated._1, toOptionalList(leftAssociated._2, f))

    }
  }

  def constructExprLvl09(
      ptree: NodeOrLeaf[Token]
  ): (NominalTreeModule.Expr, Option[List[ClassOrFunDef]]) = {
    ptree match {
      case Node('lvl09 ::= List(BANG(), 'lvl10), List(Leaf(bt), lvl10Tree)) =>
        val (e, f) = constructExprLvl10(lvl10Tree)
        (Not(e), f)
      case Node('lvl09 ::= List(MINUS(), 'lvl10), List(Leaf(mt), lvl10Tree)) =>
        val (e, f) = constructExprLvl10(lvl10Tree)
        (Neg(e), f)
      case Node('lvl09 ::= List('lvl10), List(lvl10Tree)) =>
        constructExprLvl10(lvl10Tree)

    }
  }

  def constructExprLvl10(
      ptree: NodeOrLeaf[Token]
  ): (NominalTreeModule.Expr, Option[List[ClassOrFunDef]]) = {
    ptree match {
      case Node(
          'lvl10 ::= (IF() :: _),
          List(Leaf(it), _, cond, _, _, thenn, _, _, _, elze, _)
          ) =>
        val (c, fc) = constructExpr(cond)
        val (t, ft) = constructExpr(thenn)
        val (e, fe) = constructExpr(elze)
        (Ite(c, t, e).setPos(it), toOptionalList(toOptionalList(fc, ft), fe))
      case Node('lvl10 ::= (ERROR() :: _), List(Leaf(ert), _, msg, _)) =>
        val (m, fm) = constructExpr(msg)
        (Error(m).setPos(ert), fm)
      case Node('lvl10 ::= List('Id, 'IdExprOpt), List(id, idOpt)) => {
        idOpt match {
          case Node(
              'IdExprOpt ::= ('QNameOpt :: _),
              List(qnameopt, _, argsTree, _)
              ) => {
            qnameopt match {
              case Node('QNameOpt ::= (DOT() :: _), List(_, nameTree)) => {
                val (module, modPos) = constructName(id)
                val (name, namePos) = constructName(nameTree)
                val qname = QualifiedName(Some(module), name)
                val args =
                  constructList(argsTree, constructExpr, hasComma = true)
                (
                  Call(qname, args.map(_._1)).setPos(modPos),
                  args.foldLeft(None: Option[List[ClassOrFunDef]])(
                    (acc, a) => toOptionalList(acc, a._2)
                  )
                )
              }
              case Node('QNameOpt ::= List(), List()) => {
                val (name, namePos) = constructName(id)
                val qname = QualifiedName(None, name)
                val args =
                  constructList(argsTree, constructExpr, hasComma = true)
                (
                  Call(qname, args.map(_._1)).setPos(namePos),
                  args.foldLeft(None: Option[List[ClassOrFunDef]])(
                    (acc, a) => toOptionalList(acc, a._2)
                  )
                )
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
              case Node(
                  'ParenOpt ::= List('Expr, RPAREN()),
                  List(expr, Leaf(rpt))
                  ) => {
                val (e, f) = constructExpr(expr)
                (e.setPos(rpt), f)
              }
            }
          }
        }

      case Node('lvl10 ::= List('ListCompr), List(listCompr)) =>
        listCompr match {
          case Node(
              'ListCompr ::= _,
              List(_, expr, forIn, optionalForIn, optionalIf, Leaf(rbr))
              ) =>
            val (firstFor: (String, String, Expr), fFirstFor) = forIn match {
              case Node('ForIn ::= _, List(_, id, _, listExpr)) =>
                val listId = ListId.fresh()
                val (list, fList) = constructExpr(listExpr)
                ((constructName(id)._1, listId, list), fList)
            }

            val (otherFors, fOtherFor) = fors(optionalForIn)
            val (forIns: List[(String, String, Expr)], fForIns) =
              (firstFor :: otherFors, toOptionalList(fFirstFor, fOtherFor))

            val (insideExpr, fExpr) = constructExpr(expr)
            val (cond, fCond) = optionalIf match {
              case Node('OptionalIf ::= List(), List()) => (None, None)
              case Node('OptionalIf ::= _, List(_, c)) =>
                val (e, f) = constructExpr(c)
                (Some(e), f)
            }

            val knownVar = forIns.map(
              forIn => NominalTreeModule.Variable(forIn._1)
            ) ++ forIns.map(forIn => NominalTreeModule.Variable(forIn._2))
            val extVar = newVariables(knownVar, insideExpr) ++ (if (cond.isDefined)
                                                                  newVariables(
                                                                    knownVar,
                                                                    cond.get
                                                                  )
                                                                else Nil)

            val otherFuns = generateFunctions(forIns.map {
              case (argsId, listId, _) => (argsId, listId)
            }, extVar.toList, cond, insideExpr)

            val allFuns = toOptionalList(
              toOptionalList(
                toOptionalList(fForIns, Some(otherFuns.map(_._1))),
                fExpr
              ),
              fCond
            )

            (
              Call(
                QualifiedName(Some(currentModule), otherFuns.head._2),
                forIns.map(_._3) ++ extVar.map(n => Variable(n))
              ).setPos(rbr),
              allFuns
            )
        }
    }

    /* Fun base :
    [ expr for id1 in id2 if(cond) ] ==>> ++listComprDesuggar1(id2)

    def ++listComprDesuggar1(x1: L.List) = {

      x1 match {
        case L.Cons(id1, tail) => if(cond) L.Cons(expr, ++listComprDesuggar1(tail)) else ++listComprDesuggar1(tail)
        case L.Nil() => L.Nil()
      }
    }
   */

  }

  override def constructLiteral(pTree: NodeOrLeaf[Token]): Literal[_] = {
    pTree match {
      case Node(
          'LiteralWithoutUnit ::= List(INTLITSENT),
          List(Leaf(it @ INTLIT(i)))
          ) =>
        IntLiteral(i).setPos(it)
      case Node(
          'LiteralWithoutUnit ::= List(STRINGLITSENT),
          List(Leaf(st @ STRINGLIT(s)))
          ) =>
        StringLiteral(s).setPos(st)
      case Node('LiteralWithoutUnit ::= _, List(Leaf(tt @ TRUE()))) =>
        BooleanLiteral(true).setPos(tt)
      case Node('LiteralWithoutUnit ::= _, List(Leaf(tf @ FALSE()))) =>
        BooleanLiteral(false).setPos(tf)
    }
  }

  override def constructPattern(
      pTree: NodeOrLeaf[Token]
  ): NominalTreeModule.Pattern = {
    pTree match {
      case Node('Pattern ::= List(UNDERSCORE()), List(Leaf(ut))) =>
        WildcardPattern().setPos(ut)
      case Node('Pattern ::= List('LiteralWithoutUnit), List(lit)) =>
        val literal = constructLiteral(lit)
        LiteralPattern(literal).setPos(literal)
      case Node('Pattern ::= List(LPAREN(), RPAREN()), List(Leaf(lpt), _)) =>
        val literal = UnitLiteral().setPos(lpt)
        LiteralPattern(literal).setPos(literal)
      case Node('Pattern ::= List('Id, 'IdPatternOpt), List(id, idPatOpt)) =>
        idPatOpt match {
          case Node('IdPatternOpt ::= List(), List()) => {
            val (name, pos) = constructName(id)
            IdPattern(name).setPos(pos)
          }
          case Node(
              'IdPatternOpt ::= List('QNameOpt, LPAREN(), 'Patterns, RPAREN()),
              List(qnameopt, _, patternsTree, _)
              ) => {
            qnameopt match {
              case Node('QNameOpt ::= (DOT() :: _), List(_, nameTree)) => {
                val (module, modPos) = constructName(id)
                val (name, namePos) = constructName(nameTree)
                val qname = QualifiedName(Some(module), name)
                val patterns =
                  constructList(patternsTree, constructPattern, hasComma = true)
                CaseClassPattern(qname, patterns).setPos(modPos)
              }
              case Node('QNameOpt ::= List(), List()) => {
                val (name, namePos) = constructName(id)
                val qname = QualifiedName(None, name)
                val patterns =
                  constructList(patternsTree, constructPattern, hasComma = true)
                CaseClassPattern(qname, patterns).setPos(namePos)
              }
            }
          }
        }
    }

  }

  def constructCase(
      pTree: NodeOrLeaf[Token]
  ): (MatchCase, Option[List[ClassOrFunDef]]) = {
    pTree match {
      case Node('Case ::= _, List(Leaf(ct), pat, _, expr)) =>
        val (e, f) = constructExpr(expr)
        (MatchCase(constructPattern(pat), e).setPos(ct), f)
    }
  }

  def constructCases(
      ptree: NodeOrLeaf[Token]
  ): (List[MatchCase], Option[List[ClassOrFunDef]]) = {
    ptree match {
      case Node('Cases ::= _, List(caseTree, casesOpt)) => {
        casesOpt match {
          case Node('CasesOpt ::= List(), List()) =>
            val (c, f) = constructCase(caseTree)
            (c :: Nil, f)
          case Node('CasesOpt ::= List('Cases), List(casesTree)) =>
            val (c1, f1) = constructCase(caseTree)
            val (c2, f2) = constructCases(casesTree)
            (c1 :: c2, toOptionalList(f1, f2))
        }
      }

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
  def constructOpExpr(
      leftopd: Expr,
      ptree: NodeOrLeaf[Token],
      defs: Option[List[ClassOrFunDef]],
      constrFun: NodeOrLeaf[Token] => (Expr, Option[List[ClassOrFunDef]])
  ): (Expr, Option[List[ClassOrFunDef]]) = {
    ptree match {
      case Node(_, List()) => //epsilon rule of the nonterminals
        (leftopd, defs)
      case Node(sym ::= _, List(op, rightNode))
          if Set('lvl03opt, 'lvl04opt, 'lvl05opt, 'lvl06opt, 'lvl07opt,
            'lvl08opt) contains sym =>
        rightNode match {
          case Node(_, List(nextOpd, optExpr)) => // 'Expr? ::= Expr? ~ 'OpExpr,
            val nextAtom = constrFun(nextOpd)
            constructOpExpr(
              constructOp(op)(leftopd, nextAtom._1).setPos(leftopd),
              optExpr,
              toOptionalList(defs, nextAtom._2),
              constrFun
            ) // captures left associativity
        }
    }
  }

  def toOptionalList(
      o1: Option[List[ClassOrFunDef]],
      o2: Option[List[ClassOrFunDef]]
  ): Option[List[ClassOrFunDef]] = {
    val l = o1.getOrElse(Nil) ++ o2.getOrElse(Nil)
    if (l.isEmpty) None else Some(l)
  }

  def fors(
      optionalForIns: NodeOrLeaf[Token]
  ): (List[(String, String, Expr)], Option[List[ClassOrFunDef]]) = {
    optionalForIns match {
      case Node('OptionalForIns ::= List(), List()) => (Nil, None)
      case Node(
          'OptionalForIns ::= List('ForIn, 'OptionalForIns),
          List(curr, tail)
          ) =>
        curr match {
          case Node('ForIn ::= _, List(_, id, _, listExpr)) =>
            val listId = ListId.fresh()
            val (list, fList) = constructExpr(listExpr)
            val (rest, frest) = fors(tail)
            (
              (constructName(id)._1, listId, list) :: rest,
              toOptionalList(fList, frest)
            )
        }
    }
  }

  def generateFunctions(
      lists: List[(String, String)],
      args: List[String],
      cond: Option[Expr],
      expr: Expr
  ): List[(ClassOrFunDef, String)] = {
    lists match {
      case Nil         => Nil
      case head :: Nil => List(funBase(head._1, head._2, args, cond, expr))
      case head :: tail => {
        val funName = FunctionId.fresh()

        val qnameNil = QualifiedName(Some("L"), "Nil")
        val qnameCons = QualifiedName(Some("L"), "Cons")
        val qnameList = QualifiedName(Some("L"), "List")
        val qnameConcat = QualifiedName(Some("L"), "concat")

        val ttList = TypeTree(ClassType(qnameList))
        val ttInt = TypeTree(IntType)

        val newArgs = head._1 :: args
        val newArgsAsParamDef = newArgs.map(ParamDef(_, ttInt))
        val oldArgsAsParamDef = args.map(ParamDef(_, ttInt))

        val rests: List[(ClassOrFunDef, String)] =
          generateFunctions(tail, newArgs, cond, expr)

        val callTail =
          Call(
            QualifiedName(Some(currentModule), funName),
            Variable("tail") :: tail.map {
              case (_, listId) => Variable(listId)
            } ::: args.map(Variable)
          )
        val callRest =
          Call(QualifiedName(Some(currentModule), rests.head._2), tail.map {
            case (_, listId) => Variable(listId)
          } ::: newArgs.map(Variable))
        val funParams = lists.map {
          case (_, listId) => ParamDef(listId, ttList)
        } ::: oldArgsAsParamDef
        val matchCaseNil =
          MatchCase(CaseClassPattern(qnameNil, List()), Call(qnameNil, List()))

        val curr: ClassOrFunDef =
          FunDef(
            funName,
            funParams,
            ttList,
            Match(
              Variable(head._2),
              List(
                MatchCase(
                  CaseClassPattern(
                    qnameCons,
                    List(IdPattern(head._1), IdPattern("tail"))
                  ),
                  Call(qnameConcat, List(callRest, callTail))
                ),
                matchCaseNil
              )
            )
          )

        (curr, funName) :: rests

      }
    }
  }

  def funBase(
      internId: String,
      list: String,
      argsId: List[String],
      cond: Option[Expr],
      expr: Expr
  ): (ClassOrFunDef, String) = {
    val name = FunctionId.fresh()

    val qnameNil = QualifiedName(Some("L"), "Nil")
    val qnameCons = QualifiedName(Some("L"), "Cons")
    val qnameList = QualifiedName(Some("L"), "List")
    val ttList = TypeTree(ClassType(qnameList))
    val ttInt = TypeTree(IntType)
    val funParams = ParamDef(list, ttList) :: argsId.map(ParamDef(_, ttInt))
    val matchCaseNil =
      MatchCase(CaseClassPattern(qnameNil, List()), Call(qnameNil, List()))

    val funDef = cond match {
      case None =>
        FunDef(
          name,
          funParams,
          ttList,
          Match(
            Variable(list),
            List(
              MatchCase(
                CaseClassPattern(
                  qnameCons,
                  List(IdPattern(internId), IdPattern("tail"))
                ),
                Call(
                  qnameCons,
                  List(
                    expr,
                    Call(
                      QualifiedName(Some(currentModule), name),
                      Variable("tail") :: argsId.map(Variable)
                    )
                  )
                )
              ),
              matchCaseNil
            )
          )
        )
      case Some(condExpr) =>
        FunDef(
          name,
          funParams,
          ttList,
          Match(
            Variable(list),
            List(
              MatchCase(
                CaseClassPattern(
                  qnameCons,
                  List(IdPattern(internId), IdPattern("tail"))
                ),
                Ite(
                  condExpr,
                  Call(
                    qnameCons,
                    List(
                      expr,
                      Call(
                        QualifiedName(Some(currentModule), name),
                        Variable("tail") :: argsId.map(Variable)
                      )
                    )
                  ),
                  Call(
                    QualifiedName(Some(currentModule), name),
                    Variable("tail") :: argsId.map(Variable)
                  )
                )
              ),
              matchCaseNil
            )
          )
        )

    }

    (funDef, name)
  }

  /**
    * take a list of Variables which corresponds to the Variable defined after for keywords and expr and returns the
    * new Variables that need to be taken as param in the given expr
    *
    * @param known
    * @param expr
    * @return
    */
  def newVariables(known: List[Variable], expr: Expr): Set[String] = {
    expr match {
      case Variable(name) =>
        if (!known.exists(p => p.name == name)) Set(name) else Set.empty

      case Plus(lhs, rhs) =>
        newVariables(known, lhs) ++ newVariables(known, rhs)
      case Minus(lhs, rhs) =>
        newVariables(known, lhs) ++ newVariables(known, rhs)
      case Times(lhs, rhs) =>
        newVariables(known, lhs) ++ newVariables(known, rhs)
      case Div(lhs, rhs) => newVariables(known, lhs) ++ newVariables(known, rhs)
      case Mod(lhs, rhs) => newVariables(known, lhs) ++ newVariables(known, rhs)
      case LessThan(lhs, rhs) =>
        newVariables(known, lhs) ++ newVariables(known, rhs)
      case LessEquals(lhs, rhs) =>
        newVariables(known, lhs) ++ newVariables(known, rhs)
      case And(lhs, rhs) => newVariables(known, lhs) ++ newVariables(known, rhs)
      case Or(lhs, rhs)  => newVariables(known, lhs) ++ newVariables(known, rhs)
      case Equals(lhs, rhs) =>
        newVariables(known, lhs) ++ newVariables(known, rhs)
      case Concat(lhs, rhs) =>
        newVariables(known, lhs) ++ newVariables(known, rhs)
      case Not(e) => newVariables(known, e)
      case Neg(e) => newVariables(known, e)

      case Call(qname, args) => args.flatMap(a => newVariables(known, a)).toSet

      case Sequence(e1, e2) =>
        newVariables(known, e1) ++ newVariables(known, e2)

      case Let(df, value, body) => newVariables(known, body)

      case Ite(cond, thenn, elze) =>
        newVariables(known, cond) ++ newVariables(known, thenn) ++ newVariables(
          known,
          elze
        )

      case Match(scrut, cases) =>
        newVariables(known, scrut) ++ cases.flatMap(
          c => newVariables(known, c.expr)
        )

      case Error(msg) => newVariables(known, msg)

      case _ => Set.empty

    }
  }
}

