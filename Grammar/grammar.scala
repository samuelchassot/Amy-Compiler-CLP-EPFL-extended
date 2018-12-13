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