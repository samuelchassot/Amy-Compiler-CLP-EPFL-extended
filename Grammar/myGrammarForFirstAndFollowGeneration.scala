'Program -> 'ModuleDefs
'ModuleDefs ->'ModuleDef  'ModuleDefs | EPSILON
'ModuleDef -> OBJECT()  'Id  LBRACE()  'Definitions  'OptExpr  RBRACE()  EOF()
'Definitions -> 'Definition  'Definitions | EPSILON
'Definition -> 'AbstractClassDef | 'CaseClassDef | 'FunDef
'AbstractClassDef -> ABSTRACT()  CLASS()  'Id
'CaseClassDef -> CASE()  CLASS()  'Id  LPAREN()  'Params  RPAREN()  EXTENDS()  'Id
'FunDef -> DEF()  'Id  LPAREN()  'Params  RPAREN()  COLON()  'Type  EQSIGN()  LBRACE()  'Expr  RBRACE()
'Params -> EPSILON | 'Param  'ParamList
'ParamList -> EPSILON | COMMA()  'Param  'ParamList
'Param -> 'Id  COLON()  'Type
'OptExpr -> 'Expr | EPSILON
'Type -> INT() | STRING() | BOOLEAN() | UNIT() | 'QName
'QName -> 'Id  'QNameOpt
'QNameOpt -> DOT()  'Id | EPSILON
'Expr -> 'lvl01
'lvl01 -> VAL()  'Param  EQSIGN()  'lvl02  SEMICOLON()  'Expr | 'lvl02
'lvl02 -> 'lvl03  'lvl02opt
'lvl02opt -> MATCH()  LBRACE()  'Cases  RBRACE() | EPSILON
'lvl03 -> 'lvl04  'lvl03opt
'lvl03opt -> OR()  'lvl03 | EPSILON
'lvl04 -> 'lvl05  'lvl04opt
'lvl04opt -> AND()  'lvl04  'lvl04opt | EPSILON
'lvl05 -> 'lvl06  'lvl05opt
'lvl05opt -> EQUALS()  'lvl05 'lvl05opt | EPSILON
'lvl06 -> 'lvl07  'lvl06opt
'lvl06opt -> LESSTHAN()  'lvl06  'lvl06opt | LESSEQUALS()  'lvl06  'lvl06opt | EPSILON
'lvl07 -> 'lvl08  'lvl07opt
'lvl07opt -> PLUS()  'lvl07  'lvl07opt | MINUS()  'lvl07  'lvl07opt | CONCAT()  'lvl07  'lvl07opt | EPSILON
'lvl08 -> 'lvl09  'lvl08opt
'lvl08opt -> TIMES()  'lvl08  'lvl08opt | DIV()  'lvl08  'lvl08opt | MOD()  'lvl08  'lvl08opt | EPSILON
'lvl09 -> BANG()  'lvl10 | MINUS()  'lvl10 | 'lvl10
'lvl10 -> IF()  LPAREN()  'Expr  RPAREN()  LBRACE()  'Expr  RBRACE()  ELSE()  LBRACE()  'Expr  RBRACE() | ERROR()  LPAREN()  'Expr  RPAREN() | 'Id  'IdExprOpt | 'LiteralWithoutUnit | 'Parenthesized
'Parenthesized -> LPAREN()  'ParenOpt
'ParenOpt -> 'Expr  RPAREN() | RPAREN()
'IdExprOpt -> 'QNameOpt  LPAREN()  'Args  RPAREN() | EPSILON
'LiteralWithoutUnit -> TRUE() | FALSE() | INTLITSENT | STRINGLITSENT
'Cases -> 'Case  'CasesOpt
'CasesOpt -> 'Cases | EPSILON
'Case -> CASE()  'Pattern  RARROW()  'Expr
'Pattern -> UNDERSCORE() | 'LiteralWithoutUnit | LPAREN()  RPAREN() | 'Id  'IdPatternOpt 
'IdPatternOpt -> EPSILON | 'QNameOpt  LPAREN()  'Patterns  RPAREN()
'Patterns -> EPSILON | 'Pattern  'PatternList
'PatternList -> EPSILON | COMMA()  'Pattern  'PatternList
'Args -> EPSILON | 'Expr  'ExprList
'ExprList -> EPSILON | COMMA()  'Expr  'ExprList
'Id -> IDSENT
