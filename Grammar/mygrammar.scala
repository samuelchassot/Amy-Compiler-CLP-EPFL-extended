

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


'Expr ::= 'lvl01,
'lvl01 ::= VAL() ~ 'Param ~ EQSIGN() ~ 'lvl02 ~ SEMICOLON() ~ 'Expr | 'lvl02,
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
    'Id ~ 'IdExprOpt | 'LiteralWithoutUnit | 'Parenthesized,
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















//BEFORE REMOVING LEFT RECURSION



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


'Expr ::= 'lvl01,
'lvl01 ::= VAL() ~ 'Param ~ EQSIGN() ~ 'lvl02 ~ SEMICOLON() ~ 'Expr | 'lvl02,
'lvl02 ::= 'lvl02 ~ MATCH() ~ LBRACE() ~ 'Cases ~ RBRACE() | 'lvl03,
'lvl03 ::= 'lvl03 ~ OR() ~ 'lvl03 ,
'lvl04 ::= ~ AND() ~ | 'lvl05,
'lvl05 ::= 'lvl05 ~ EQUALS() ~ 'lvl05 | 'lvl06,
'lvl06 ::= 'lvl06 ~ LESSTHAN() ~ 'lvl06 | 'lvl06 ~ LESSEQUALS() ~ 'lvl06 | 'lvl07,
'lvl07 ::= 'lvl07 ~ PLUS() ~ 'lvl07 | 'lvl07 ~ MINUS() ~ 'lvl07 | 'lvl07 ~ CONCAT() ~ 'lvl07 | 'lvl08,
'lvl08 ::= 'lvl08 ~ TIMES() ~ 'lvl08 | 'lvl08 ~ DIV() ~ 'lvl08 | 'lvl08 ~ MOD() ~ 'lvl08 | 'lvl09,
'lvl09 ::= BANG() ~ 'lvl09 | MINUS() ~ 'lvl09 | 'lvl10,
'lvl10 ::= IF() ~ LPAREN() ~ 'Expr ~ RPAREN() ~ LBRACE() ~ 'Expr ~ RBRACE() ~ ELSE() ~ LBRACE() ~ 'Expr ~ RBRACE() |
    ERROR() ~ LPAREN() ~ 'Expr ~ RPAREN() | 'QName ~ LPAREN() ~ 'Args ~ RPAREN() |
    'Id | 'LiteralWithoutUnit | 'Parenthesized,
'Parenthesized ::= LPAREN() ~ 'Expr ~ RPAREN(),

'LiteralWithoutUnit ::= TRUE() | FALSE() | LPAREN() ~ RPAREN() | INTLITSENT | STRINGLITSENT,
'Cases ::= 'Case | 'Case ~ 'Cases,
'Case ::= CASE() ~ 'Pattern ~ RARROW() ~ 'Expr,
'Pattern ::= UNDERSCORE() | 'LiteralWithoutUnit | 'Id | 'QName ~ LPAREN() ~ 'Patterns ~ RPAREN(),
'Patterns ::= epsilon() | 'Pattern ~ 'PatternList,
'PatternList ::= epsilon() | COMMA() ~ 'Pattern ~ 'PatternList,
'Args ::= epsilon() | 'Expr ~ 'ExprList,
'ExprList ::= epsilon() | COMMA() ~ 'Expr ~ 'ExprList,
'Id ::= IDSENT