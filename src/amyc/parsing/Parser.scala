package amyc
package parsing

import grammarcomp.grammar.CFGrammar._
import grammarcomp.grammar.GrammarDSL._
import grammarcomp.grammar.GrammarUtils.InLL1
import grammarcomp.grammar._
import grammarcomp.parsing._
import amyc.utils._
import ast.NominalTreeModule._
import Tokens._

// The parser for Amy
// Absorbs tokens from the Lexer and then uses grammarcomp to generate parse trees.
// Defines two different grammars, a naive one which does not obey operator precedence (for demonstration purposes)
// and an LL1 grammar that implements the true syntax of Amy
object Parser extends Pipeline[Stream[Token], Program] {

  /* This grammar does not implement the correct syntax of Amy and is not LL1
   * It is given as an example
   */
  val amyGrammar = Grammar('Program, List[Rules[Token]](
    'Program ::= 'ModuleDefs,
    'ModuleDefs ::= 'ModuleDef ~ 'ModuleDefs | epsilon(),
    'ModuleDef ::= OBJECT() ~ 'Id ~ LBRACE() ~ 'Definitions ~ 'OptExpr ~ RBRACE() ~ EOF(),
    'Definitions ::= 'Definition ~ 'Definitions | epsilon(),
    'Definition ::= 'AbstractClassDef | 'CaseClassDef | 'FunDef,
    'AbstractClassDef ::= ABSTRACT() ~ CLASS() ~ 'Id,
    'CaseClassDef ::= CASE() ~ CLASS() ~ 'Id ~ LPAREN() ~ 'Params ~ RPAREN() ~ EXTENDS() ~ 'Id,
    'FunDef ::= DEF() ~ 'Id ~ LPAREN() ~ 'Params ~ RPAREN() ~ COLON() ~ 'Type ~ EQSIGN() ~ LBRACE() ~ 'Expr ~ RBRACE(),
    'Params ::= epsilon() | 'Param ~ 'ParamList,
    'ParamList ::= epsilon() | COMMA() ~ 'Param ~ 'ParamList,
    'Param ::= 'Id ~ COLON() ~ 'Type,
    'OptExpr ::= 'Expr | epsilon(),
    'Type ::= INT() | STRING() | BOOLEAN() | UNIT() | 'QName,
    'QName ::= 'Id | 'Id ~ DOT() ~ 'Id,
    'Expr ::= 'Id | 'Literal | 'Expr ~ 'BinOp ~ 'Expr | BANG() ~ 'Expr | MINUS() ~ 'Expr |
              'QName ~ LPAREN() ~ 'Args ~ RPAREN() | 'Expr ~ SEMICOLON() ~ 'Expr |
              VAL() ~ 'Param ~ EQSIGN() ~ 'Expr ~ SEMICOLON() ~ 'Expr |
              IF() ~ LPAREN() ~ 'Expr ~ RPAREN() ~ LBRACE() ~ 'Expr ~ RBRACE() ~ ELSE() ~ LBRACE() ~ 'Expr ~ RBRACE() |
              'Expr ~ MATCH() ~ LBRACE() ~ 'Cases ~ RBRACE() |
              ERROR() ~ LPAREN() ~ 'Expr ~ RPAREN() |
              LPAREN() ~ 'Expr ~ RPAREN(),
    'Literal ::= TRUE() | FALSE() | LPAREN() ~ RPAREN() | INTLITSENT | STRINGLITSENT,
    'BinOp ::= PLUS() | MINUS() | TIMES() | DIV() | MOD() | LESSTHAN() | LESSEQUALS() |
               AND() | OR() | EQUALS() | CONCAT(),
    'Cases ::= 'Case | 'Case ~ 'Cases,
    'Case ::= CASE() ~ 'Pattern ~ RARROW() ~ 'Expr,
    'Pattern ::= UNDERSCORE() | 'Literal | 'Id | 'QName ~ LPAREN() ~ 'Patterns ~ RPAREN(),
    'Patterns ::= epsilon() | 'Pattern ~ 'PatternList,
    'PatternList ::= epsilon() | COMMA() ~ 'Pattern ~ 'PatternList,
    'Args ::= epsilon() | 'Expr ~ 'ExprList,
    'ExprList ::= epsilon() | COMMA() ~ 'Expr ~ 'ExprList,
    'Id ::= IDSENT
  ))

  // You can start from the example above and work your way from there.
  // Make sure you use the warning (see `run` below) that tells you which part is not in LL1.
  lazy val amyGrammarLL1 = Grammar('Program, List[Rules[Token]](
    'Program ::= 'ModuleDefs,
    'ModuleDefs ::='ModuleDef ~ 'ModuleDefs | epsilon(),
    'ModuleDef ::= OBJECT() ~ 'Id ~ LBRACE() ~ 'Definitions ~ 'OptExpr ~ RBRACE() ~ EOF(),
    'Definitions ::= 'Definition ~ 'Definitions | epsilon(),
    'Definition ::= 'AbstractClassDef | 'CaseClassDef | 'FunDef,
    'AbstractClassDef ::= ABSTRACT() ~ CLASS() ~ 'Id,
    'CaseClassDef ::= CASE() ~ CLASS() ~ 'Id ~ LPAREN() ~ 'Params ~ RPAREN() ~ EXTENDS() ~ 'Id,
    'FunDef ::= DEF() ~ 'Id ~ LPAREN() ~ 'Params ~ RPAREN() ~ COLON() ~ 'Type ~ EQSIGN() ~ LBRACE() ~ 'Expr ~ RBRACE(),
    'Params ::= epsilon() | 'Param ~ 'ParamList,
    'ParamList ::= epsilon() | COMMA() ~ 'Param ~ 'ParamList,
    'Param ::= 'Id ~ COLON() ~ 'Type,
    'OptExpr ::= 'Expr | epsilon(),
    'Type ::= INT() | STRING() | BOOLEAN() | UNIT() | 'QName,
    'QName ::= 'Id ~ 'QNameOpt,
    'QNameOpt ::= DOT() ~ 'Id | epsilon(),

    'Expr ::= 'lvl01 | 'StartVal,
    'StartVal ::= VAL() ~ 'Param ~ EQSIGN() ~ 'lvl02 ~ SEMICOLON() ~ 'Expr,
    'lvl01 ::= 'lvl02 ~ 'lvl01opt ,
    'lvl01opt ::= SEMICOLON() ~ 'lvl01optopt | epsilon(),
    'lvl01optopt ::= 'StartVal | 'lvl01,
    'lvl02 ::= 'lvl03 ~ 'lvl02opt,
    'lvl02opt ::= MATCH() ~ LBRACE() ~ 'Cases ~ RBRACE() | epsilon(),

    'lvl03 ::= 'lvl04 ~ 'lvl03opt,
    'lvl03opt ::= OR() ~ 'lvl03 | epsilon(),

    'lvl04 ::= 'lvl05 ~ 'lvl04opt,
    'lvl04opt ::= AND() ~ 'lvl04 | epsilon(),

    'lvl05 ::= 'lvl06 ~ 'lvl05opt,
    'lvl05opt ::= EQUALS() ~ 'lvl05 | epsilon(),

    'lvl06 ::= 'lvl07 ~ 'lvl06opt,
    'lvl06opt ::= LESSTHAN() ~ 'lvl06 | LESSEQUALS() ~ 'lvl06 | epsilon(),

    'lvl07 ::= 'lvl08 ~ 'lvl07opt,
    'lvl07opt ::= PLUS() ~ 'lvl07 | MINUS() ~ 'lvl07 | CONCAT() ~ 'lvl07 | epsilon(),

    'lvl08 ::= 'lvl09 ~ 'lvl08opt,
    'lvl08opt ::= TIMES() ~ 'lvl08 | DIV() ~ 'lvl08 | MOD() ~ 'lvl08 | epsilon(),

    'lvl09 ::= BANG() ~ 'lvl10 | MINUS() ~ 'lvl10 | 'lvl10,
    'lvl10 ::= IF() ~ LPAREN() ~ 'Expr ~ RPAREN() ~ LBRACE() ~ 'Expr ~ RBRACE() ~ ELSE() ~ LBRACE() ~ 'Expr ~ RBRACE() |
                    ERROR() ~ LPAREN() ~ 'Expr ~ RPAREN() |
                    'Id ~ 'IdExprOpt | 'LiteralWithoutUnit | 'Parenthesized | 'ListCompr,
    'ListCompr ::= LBRACK() ~ 'Expr ~ FOR() ~ 'Id ~ IN() ~ 'Id ~ 'OptionalIf ~ RBRACK(),
    'OptionalIf ::= epsilon() | IF() ~ LPAREN() ~ 'Expr ~ RPAREN(),
    'Parenthesized ::= LPAREN() ~ 'ParenOpt,
    'ParenOpt ::= 'Expr ~ RPAREN() | RPAREN(),

    'IdExprOpt ::= 'QNameOpt ~ LPAREN() ~ 'Args ~ RPAREN() | epsilon(),

    'LiteralWithoutUnit ::= TRUE() | FALSE() | INTLITSENT | STRINGLITSENT,
    'Cases ::= 'Case ~ 'CasesOpt,
    'CasesOpt ::= 'Cases | epsilon(),
    'Case ::= CASE() ~ 'Pattern ~ RARROW() ~ 'Expr,

    'Pattern ::= UNDERSCORE() | 'LiteralWithoutUnit | LPAREN() ~ RPAREN() | 'Id ~ 'IdPatternOpt,
    'IdPatternOpt ::= epsilon() | 'QNameOpt ~ LPAREN() ~ 'Patterns ~ RPAREN(),
    'Patterns ::= epsilon() | 'Pattern ~ 'PatternList,
    'PatternList ::= epsilon() | COMMA() ~ 'Pattern ~ 'PatternList,
    'Args ::= epsilon() | 'Expr ~ 'ExprList,
    'ExprList ::= epsilon() | COMMA() ~ 'Expr ~ 'ExprList,
    'Id ::= IDSENT



  ))

  def run(ctx: Context)(tokens: Stream[Token]): Program = {
    val (grammar, constructor) = (amyGrammarLL1, new ASTConstructorLL1)

    import ctx.reporter._
    implicit val gc = new GlobalContext()
    implicit val pc = new ParseContext()

    GrammarUtils.isLL1WithFeedback(grammar) match {
      case InLL1() =>
         info("Grammar is in LL1")
      case other =>
        warning(other)
    }

    val feedback = ParseTreeUtils.parseWithTrees(grammar, tokens.toList)
    feedback match {
      case s: Success[Token] =>
        constructor.constructProgram(s.parseTrees.head)
      case err@LL1Error(_, Some(tok)) =>
        fatal(s"Parsing failed: $err", tok.obj.position)
      case err =>
        fatal(s"Parsing failed: $err")
    }
  }

}
