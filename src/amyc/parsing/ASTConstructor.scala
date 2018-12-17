package amyc
package parsing

import grammarcomp.parsing._
import utils.Positioned
import ast.NominalTreeModule._
import Tokens._

// Will construct Amy trees from grammarcomp parse Trees.
// Corresponds to Parser.msGrammar
abstract class ASTConstructor {

  def constructProgram(ptree: NodeOrLeaf[Token]): Program = {
    ptree match {
      case Node('Program ::= _, List(mods)) =>
        val modules = constructList(mods, constructModule)
        val p = Program(modules)
        if (modules.nonEmpty) p.setPos(modules.head) else p
    }
  }

  def constructModule(pTree: NodeOrLeaf[Token]): ModuleDef = {
    pTree match {
      case Node('ModuleDef ::= _, List(Leaf(obj), name, _, defs, optExpr, _, _)) =>
        val modName = constructName(name)._1
        val constructedOption = constructOption(optExpr, constructExpr)
        ModuleDef(
          modName,
          constructList(defs, constructDef).flatten ::: constructedOption._2.getOrElse(Some(Nil)).getOrElse(Nil),
          constructedOption._1
        ).setPos(obj)
    }
  }

  def constructName(ptree: NodeOrLeaf[Token]): (String, Positioned) = {
    ptree match {
      case Node('Id ::= _, List(Leaf(id@ID(name)))) =>
        (name, id)
    }
  }

  def constructDef(pTree: NodeOrLeaf[Token]): List[ClassOrFunDef] = {
    pTree match {
      case Node('Definition ::= _, List(df)) =>
        constructDef0(df)
    }
  }

  def constructDef0(pTree: NodeOrLeaf[Token]): List[ClassOrFunDef] = {
    pTree match {
      case Node('AbstractClassDef ::= _, List(Leaf(abs), _, name)) =>
        List(AbstractClassDef(constructName(name)._1).setPos(abs))
      case Node('CaseClassDef ::= _, List(Leaf(cse), _, name, _, params, _, _, parent)) =>
        List(
            CaseClassDef(
            constructName(name)._1,
            constructList(params, constructParam, hasComma = true).map(_.tt),
            constructName(parent)._1
          ).setPos(cse)
        )
      case Node('FunDef ::= _, List(Leaf(df), name, _, params, _, _, retType, _, _, body, _)) =>
        val (constrBody, newDefs) = constructExpr(body)
        FunDef(
          constructName(name)._1,
          constructList(params, constructParam, hasComma = true),
          constructType(retType),
          constrBody
        ).setPos(df) :: newDefs.getOrElse(Nil)
    }
  }

  def constructParam(pTree: NodeOrLeaf[Token]): ParamDef = {
    pTree match {
      case Node('Param ::= _, List(nm, _, tpe)) =>
        val (name, pos) = constructName(nm)
        ParamDef(name, constructType(tpe)).setPos(pos)
    }
  }

  def constructType(pTree: NodeOrLeaf[Token]): TypeTree = {
    pTree match {
      case Node('Type ::= _, List(Leaf(tp))) =>
        TypeTree((tp: @unchecked) match {
          case INT() => IntType
          case STRING() => StringType
          case BOOLEAN() => BooleanType
          case UNIT() => UnitType
        }).setPos(tp)
      case Node('Type ::= _, List(qn)) =>
        val (qname, pos) = constructQname(qn)
        TypeTree(ClassType(qname)).setPos(pos)
    }
  }

  def constructQname(pTree: NodeOrLeaf[Token]): (QualifiedName, Positioned)


  def tokenToExpr(t: Token): (Expr, Expr) => Expr = {
    (t: @unchecked) match {
      case PLUS() => Plus
      case MINUS() => Minus
      case TIMES() => Times
      case DIV() => Div
      case MOD() => Mod
      case LESSTHAN() => LessThan
      case LESSEQUALS() => LessEquals
      case AND() => And
      case OR() => Or
      case EQUALS() => Equals
      case CONCAT() => Concat
      case SEMICOLON() => Sequence
    }
  }

  def constructOp(ptree: NodeOrLeaf[Token]): (Expr, Expr) => Expr = {
    ptree match {
      case Node(_, List(Leaf(t))) =>
        tokenToExpr(t)
    }
  }
  def constructExpr(ptree: NodeOrLeaf[Token]): (Expr, Option[List[ClassOrFunDef]])

  def constructLiteral(pTree: NodeOrLeaf[Token]): Literal[_] = {
    pTree match {
      case Node('Literal ::= List(INTLITSENT), List(Leaf(it@INTLIT(i)))) =>
        IntLiteral(i).setPos(it)
      case Node('Literal ::= List(STRINGLITSENT), List(Leaf(st@STRINGLIT(s)))) =>
        StringLiteral(s).setPos(st)
      case Node('Literal ::= _, List(Leaf(tt@TRUE()))) =>
        BooleanLiteral(true).setPos(tt)
      case Node('Literal ::= _, List(Leaf(tf@FALSE()))) =>
        BooleanLiteral(false).setPos(tf)
      case Node('Literal ::= _, List(Leaf(lp@LPAREN()), Leaf(RPAREN()))) =>
        UnitLiteral().setPos(lp)
    }
  }

  def constructCase(pTree: NodeOrLeaf[Token]): (MatchCase, Option[List[ClassOrFunDef]])

  def constructPattern(pTree: NodeOrLeaf[Token]): Pattern = {
    pTree match {
      case Node('Pattern ::= List(UNDERSCORE()), List(Leaf(ut))) =>
        WildcardPattern().setPos(ut)
      case Node('Pattern ::= List('Literal), List(lit)) =>
        val literal = constructLiteral(lit)
        LiteralPattern(literal).setPos(literal)
      case Node('Pattern ::= List('Id), List(id)) =>
        val (name, pos) = constructName(id)
        IdPattern(name).setPos(pos)
      case Node('Pattern ::= ('QName :: _), List(qn, _, patts, _)) =>
        val (qname, pos) = constructQname(qn)
        val patterns = constructList(patts, constructPattern, hasComma = true)
        CaseClassPattern(qname, patterns).setPos(pos)
    }
  }

  /** Extracts a List of elements of a generic type A, possibly separated by commas,
    * from a parse tree, by repeating a given parser.
    *
    * The form of the parse tree has to be specific:
    * (t, ts) if there is no comma, and
    * (COMMA(), t, ts) if there is a comma,
    * where t is the tree corresponding to the first element and ts to the rest.
    *
    * @param ptree The input parse tree
    * @param constructor A transformer for an individual object
    * @param hasComma Whether the elements of the list are separated by a COMMA()
    * @tparam A The type of List elements
    * @return A list of parsed elements of type A
    */
  def constructList[A](ptree: NodeOrLeaf[Token], constructor: NodeOrLeaf[Token] => A, hasComma: Boolean = false): List[A] = {
    ptree match {
      case Node(_, List()) => List()
      case Node(_, List(t, ts)) =>
        constructor(t) :: constructList(ts, constructor, hasComma)
      case Node(_, List(Leaf(COMMA()), t, ts)) if hasComma =>
        constructor(t) :: constructList(ts, constructor, hasComma)
    }
  }

  /** Extracts a List of elements of a generic type A, possibly separated by commas,
    * from a parse tree, by repeating a given parser. The list has to be nonempty.
    *
    * The form of the parse tree has to be specific:
    * t if the list has one element,
    * (t, ts) if there is no comma,
    * and (t, COMMA(), ts) if there is a comma,
    * where t is the tree corresponding to the first element and ts to the rest.
    *
    * @param ptree The input parse tree
    * @param constructor A transformer for an individual object
    * @param hasComma Whether the elements of the list are separated by a COMMA()
    * @tparam A The type of List elements
    * @return A list of parsed elements of type A
    */
  def constructList1[A](ptree: NodeOrLeaf[Token], constructor: NodeOrLeaf[Token] => A, hasComma: Boolean = false): List[A] = {
    ptree match {
      case Node(_, List(t)) => List(constructor(t))
      case Node(_, List(t, ts)) =>
        constructor(t) :: constructList1(ts, constructor, hasComma)
      case Node(_, List(t, Leaf(COMMA()), ts)) if hasComma =>
        constructor(t) :: constructList1(ts, constructor, hasComma)
    }
  }

  /** Optionally extract an element from a parse tree.
    *
    * @param ptree The input parse tree
    * @param constructor The extractor of the element if it is present
    * @tparam A The type of the element
    * @return The element wrapped in Some(), or None if the production is empty.
    */
  def constructOption[A, B](ptree: NodeOrLeaf[Token], constructor: NodeOrLeaf[Token] => (A, B)): (Option[A], Option[B]) = {
    ptree match {
      case Node(_, List()) => (None, None)
      case Node(_, List(t)) =>
        val constr = constructor(t)
        (Some(constr._1), Some(constr._2))
    }
  }

  /** Optionally extract an element from a parse tree.
    *
    * The parse tree has to have a specific form: empty production will result in None,
    * and an operator (which will be ignored) followed by the element we need to extract
    * in case of Some.
    *
    * @param ptree The input parse tree
    * @param constructor The extractor of the element if it is present
    * @tparam A The type of the element
    * @return The element wrapped in Some(), or None if the production is empty.
    */
  def constructOpOption[A](ptree: NodeOrLeaf[Token], constructor: NodeOrLeaf[Token] => A): Option[A] = {
    ptree match {
      case Node(_, List()) => None
      case Node(_, List(_, t)) =>
        Some(constructor(t))
    }
  }

}
