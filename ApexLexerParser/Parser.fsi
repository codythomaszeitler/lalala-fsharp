// Signature file for parser generated by fsyacc
module Parser
type token = 
  | EOF
  | ID of (string)
  | ATSIGN
  | ASSIGN
  | DOT
  | COMMA
  | RETURN
  | SEMI
  | RIGHT_PAREN
  | LEFT_PAREN
  | RIGHT_BRACE
  | LEFT_BRACE
  | CLASS
  | STATIC
  | PRIVATE
  | PUBLIC
  | AFTER
  | ABSTRACT
  | MULT
  | PLUS
  | INT of (int)
type tokenId = 
    | TOKEN_EOF
    | TOKEN_ID
    | TOKEN_ATSIGN
    | TOKEN_ASSIGN
    | TOKEN_DOT
    | TOKEN_COMMA
    | TOKEN_RETURN
    | TOKEN_SEMI
    | TOKEN_RIGHT_PAREN
    | TOKEN_LEFT_PAREN
    | TOKEN_RIGHT_BRACE
    | TOKEN_LEFT_BRACE
    | TOKEN_CLASS
    | TOKEN_STATIC
    | TOKEN_PRIVATE
    | TOKEN_PUBLIC
    | TOKEN_AFTER
    | TOKEN_ABSTRACT
    | TOKEN_MULT
    | TOKEN_PLUS
    | TOKEN_INT
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startcompilationUnit
    | NONTERM_compilationUnit
    | NONTERM_typeDeclaration
    | NONTERM_modifier
    | NONTERM_identifier
    | NONTERM_classDeclaration
    | NONTERM_annotationOption
    | NONTERM_modifierList
    | NONTERM_classBody
    | NONTERM_classBodyDeclarationList
    | NONTERM_classBodyDeclaration
    | NONTERM_memberDeclaration
    | NONTERM_fieldDeclaration
    | NONTERM_variableDeclarators
    | NONTERM_variableDeclarator
    | NONTERM_typeRef
    | NONTERM_typeName
    | NONTERM_id
    | NONTERM_methodDeclaration
    | NONTERM_block
    | NONTERM_statementList
    | NONTERM_statement
    | NONTERM_localVariableDeclarationStatement
    | NONTERM_localVariableDeclaration
    | NONTERM_returnStatement
    | NONTERM_expressionStatement
    | NONTERM_primary
    | NONTERM_expression
    | NONTERM_literal
    | NONTERM_methodCall
    | NONTERM_expressions
    | NONTERM_annotation
    | NONTERM_qualifiedName
    | NONTERM_ids
/// This function maps tokens to integer indexes
val tagOfToken: token -> int

/// This function maps integer indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val compilationUnit : (FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> FSharp.Text.Lexing.LexBuffer<'cty> -> (ApexLexerParser.Apex.CompilationUnit) 
