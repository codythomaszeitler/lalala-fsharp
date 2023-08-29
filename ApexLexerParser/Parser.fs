// Implementation file for parser generated by fsyacc
module Parser
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open FSharp.Text.Lexing
open FSharp.Text.Parsing.ParseHelpers
# 1 "Parser.fsy"


# 9 "Parser.fs"
// This type is the type of tokens accepted by the parser
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
// This type is used to give symbolic names to token indexes, useful for error messages
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
// This type is used to give symbolic names to token indexes, useful for error messages
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

// This function maps tokens to integer indexes
let tagOfToken (t:token) = 
  match t with
  | EOF  -> 0 
  | ID _ -> 1 
  | ATSIGN  -> 2 
  | ASSIGN  -> 3 
  | DOT  -> 4 
  | COMMA  -> 5 
  | RETURN  -> 6 
  | SEMI  -> 7 
  | RIGHT_PAREN  -> 8 
  | LEFT_PAREN  -> 9 
  | RIGHT_BRACE  -> 10 
  | LEFT_BRACE  -> 11 
  | CLASS  -> 12 
  | STATIC  -> 13 
  | PRIVATE  -> 14 
  | PUBLIC  -> 15 
  | AFTER  -> 16 
  | ABSTRACT  -> 17 
  | MULT  -> 18 
  | PLUS  -> 19 
  | INT _ -> 20 

// This function maps integer indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_EOF 
  | 1 -> TOKEN_ID 
  | 2 -> TOKEN_ATSIGN 
  | 3 -> TOKEN_ASSIGN 
  | 4 -> TOKEN_DOT 
  | 5 -> TOKEN_COMMA 
  | 6 -> TOKEN_RETURN 
  | 7 -> TOKEN_SEMI 
  | 8 -> TOKEN_RIGHT_PAREN 
  | 9 -> TOKEN_LEFT_PAREN 
  | 10 -> TOKEN_RIGHT_BRACE 
  | 11 -> TOKEN_LEFT_BRACE 
  | 12 -> TOKEN_CLASS 
  | 13 -> TOKEN_STATIC 
  | 14 -> TOKEN_PRIVATE 
  | 15 -> TOKEN_PUBLIC 
  | 16 -> TOKEN_AFTER 
  | 17 -> TOKEN_ABSTRACT 
  | 18 -> TOKEN_MULT 
  | 19 -> TOKEN_PLUS 
  | 20 -> TOKEN_INT 
  | 23 -> TOKEN_end_of_input
  | 21 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startcompilationUnit 
    | 1 -> NONTERM_compilationUnit 
    | 2 -> NONTERM_typeDeclaration 
    | 3 -> NONTERM_modifier 
    | 4 -> NONTERM_modifier 
    | 5 -> NONTERM_modifier 
    | 6 -> NONTERM_identifier 
    | 7 -> NONTERM_classDeclaration 
    | 8 -> NONTERM_annotationOption 
    | 9 -> NONTERM_annotationOption 
    | 10 -> NONTERM_modifierList 
    | 11 -> NONTERM_modifierList 
    | 12 -> NONTERM_classBody 
    | 13 -> NONTERM_classBodyDeclarationList 
    | 14 -> NONTERM_classBodyDeclarationList 
    | 15 -> NONTERM_classBodyDeclarationList 
    | 16 -> NONTERM_classBodyDeclaration 
    | 17 -> NONTERM_memberDeclaration 
    | 18 -> NONTERM_memberDeclaration 
    | 19 -> NONTERM_fieldDeclaration 
    | 20 -> NONTERM_variableDeclarators 
    | 21 -> NONTERM_variableDeclarators 
    | 22 -> NONTERM_variableDeclarators 
    | 23 -> NONTERM_variableDeclarator 
    | 24 -> NONTERM_typeRef 
    | 25 -> NONTERM_typeName 
    | 26 -> NONTERM_id 
    | 27 -> NONTERM_methodDeclaration 
    | 28 -> NONTERM_block 
    | 29 -> NONTERM_statementList 
    | 30 -> NONTERM_statementList 
    | 31 -> NONTERM_statement 
    | 32 -> NONTERM_statement 
    | 33 -> NONTERM_statement 
    | 34 -> NONTERM_localVariableDeclarationStatement 
    | 35 -> NONTERM_localVariableDeclaration 
    | 36 -> NONTERM_returnStatement 
    | 37 -> NONTERM_expressionStatement 
    | 38 -> NONTERM_primary 
    | 39 -> NONTERM_expression 
    | 40 -> NONTERM_expression 
    | 41 -> NONTERM_expression 
    | 42 -> NONTERM_literal 
    | 43 -> NONTERM_methodCall 
    | 44 -> NONTERM_expressions 
    | 45 -> NONTERM_expressions 
    | 46 -> NONTERM_annotation 
    | 47 -> NONTERM_qualifiedName 
    | 48 -> NONTERM_ids 
    | 49 -> NONTERM_ids 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 23 
let _fsyacc_tagOfErrorTerminal = 21

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | EOF  -> "EOF" 
  | ID _ -> "ID" 
  | ATSIGN  -> "ATSIGN" 
  | ASSIGN  -> "ASSIGN" 
  | DOT  -> "DOT" 
  | COMMA  -> "COMMA" 
  | RETURN  -> "RETURN" 
  | SEMI  -> "SEMI" 
  | RIGHT_PAREN  -> "RIGHT_PAREN" 
  | LEFT_PAREN  -> "LEFT_PAREN" 
  | RIGHT_BRACE  -> "RIGHT_BRACE" 
  | LEFT_BRACE  -> "LEFT_BRACE" 
  | CLASS  -> "CLASS" 
  | STATIC  -> "STATIC" 
  | PRIVATE  -> "PRIVATE" 
  | PUBLIC  -> "PUBLIC" 
  | AFTER  -> "AFTER" 
  | ABSTRACT  -> "ABSTRACT" 
  | MULT  -> "MULT" 
  | PLUS  -> "PLUS" 
  | INT _ -> "INT" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | EOF  -> (null : System.Object) 
  | ID _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | ATSIGN  -> (null : System.Object) 
  | ASSIGN  -> (null : System.Object) 
  | DOT  -> (null : System.Object) 
  | COMMA  -> (null : System.Object) 
  | RETURN  -> (null : System.Object) 
  | SEMI  -> (null : System.Object) 
  | RIGHT_PAREN  -> (null : System.Object) 
  | LEFT_PAREN  -> (null : System.Object) 
  | RIGHT_BRACE  -> (null : System.Object) 
  | LEFT_BRACE  -> (null : System.Object) 
  | CLASS  -> (null : System.Object) 
  | STATIC  -> (null : System.Object) 
  | PRIVATE  -> (null : System.Object) 
  | PUBLIC  -> (null : System.Object) 
  | AFTER  -> (null : System.Object) 
  | ABSTRACT  -> (null : System.Object) 
  | MULT  -> (null : System.Object) 
  | PLUS  -> (null : System.Object) 
  | INT _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
let _fsyacc_gotos = [| 0us;65535us;1us;65535us;0us;1us;1us;65535us;0us;2us;5us;65535us;8us;14us;14us;14us;24us;14us;40us;50us;43us;50us;1us;65535us;10us;11us;1us;65535us;0us;3us;3us;65535us;0us;8us;16us;24us;19us;24us;3us;65535us;8us;9us;14us;15us;24us;25us;1us;65535us;11us;12us;2us;65535us;16us;17us;19us;20us;2us;65535us;16us;19us;19us;19us;2us;65535us;16us;21us;19us;21us;2us;65535us;16us;22us;19us;22us;3us;65535us;26us;27us;30us;31us;51us;52us;3us;65535us;26us;29us;30us;29us;51us;29us;2us;65535us;25us;26us;50us;51us;2us;65535us;25us;34us;50us;34us;11us;65535us;25us;35us;26us;33us;30us;32us;40us;58us;43us;58us;50us;35us;51us;32us;53us;58us;64us;58us;68us;58us;74us;73us;2us;65535us;16us;23us;19us;23us;1us;65535us;38us;39us;2us;65535us;40us;41us;43us;44us;2us;65535us;40us;43us;43us;43us;2us;65535us;40us;45us;43us;45us;2us;65535us;40us;48us;43us;48us;2us;65535us;40us;46us;43us;46us;2us;65535us;40us;47us;43us;47us;5us;65535us;40us;59us;43us;59us;53us;59us;64us;59us;68us;59us;5us;65535us;40us;56us;43us;56us;53us;54us;64us;67us;68us;67us;5us;65535us;40us;61us;43us;61us;53us;61us;64us;61us;68us;61us;5us;65535us;40us;60us;43us;60us;53us;60us;64us;60us;68us;60us;2us;65535us;64us;65us;68us;69us;3us;65535us;0us;13us;16us;13us;19us;13us;5us;65535us;40us;63us;43us;63us;53us;63us;64us;63us;68us;63us;6us;65535us;40us;72us;43us;72us;53us;72us;64us;72us;68us;72us;74us;75us;|]
let _fsyacc_sparseGotoTableRowOffsets = [|0us;1us;3us;5us;11us;13us;15us;19us;23us;25us;28us;31us;34us;37us;41us;45us;48us;51us;63us;66us;68us;71us;74us;77us;80us;83us;86us;92us;98us;104us;110us;113us;117us;123us;|]
let _fsyacc_stateToProdIdxsTableElements = [| 1us;0us;1us;0us;1us;1us;1us;2us;1us;3us;1us;4us;1us;5us;1us;6us;1us;7us;1us;7us;1us;7us;1us;7us;1us;7us;1us;9us;1us;11us;1us;11us;1us;12us;1us;12us;1us;12us;2us;14us;15us;1us;15us;1us;16us;1us;17us;1us;18us;2us;19us;27us;2us;19us;27us;2us;19us;27us;1us;19us;1us;19us;2us;21us;22us;1us;22us;1us;22us;1us;23us;2us;23us;27us;1us;24us;1us;25us;1us;26us;1us;27us;1us;27us;1us;27us;1us;28us;1us;28us;1us;28us;1us;30us;1us;30us;1us;31us;1us;32us;1us;33us;1us;34us;1us;34us;1us;35us;1us;35us;1us;35us;1us;36us;1us;36us;1us;36us;1us;37us;1us;37us;3us;38us;48us;49us;1us;39us;1us;40us;1us;41us;1us;42us;1us;43us;1us;43us;1us;43us;1us;43us;2us;44us;45us;1us;45us;1us;45us;1us;46us;1us;46us;1us;47us;2us;48us;49us;1us;49us;1us;49us;|]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us;2us;4us;6us;8us;10us;12us;14us;16us;18us;20us;22us;24us;26us;28us;30us;32us;34us;36us;38us;41us;43us;45us;47us;49us;52us;55us;58us;60us;62us;65us;67us;69us;71us;74us;76us;78us;80us;82us;84us;86us;88us;90us;92us;94us;96us;98us;100us;102us;104us;106us;108us;110us;112us;114us;116us;118us;120us;122us;126us;128us;130us;132us;134us;136us;138us;140us;142us;145us;147us;149us;151us;153us;155us;158us;160us;|]
let _fsyacc_action_rows = 76
let _fsyacc_actionTableElements = [|1us;16392us;2us;70us;0us;49152us;0us;16385us;0us;16386us;0us;16387us;0us;16388us;0us;16389us;0us;16390us;3us;16394us;13us;6us;14us;5us;15us;4us;1us;32768us;12us;10us;1us;32768us;1us;7us;1us;32768us;11us;16us;0us;16391us;0us;16393us;3us;16394us;13us;6us;14us;5us;15us;4us;0us;16395us;2us;16392us;2us;70us;10us;16397us;1us;32768us;10us;18us;0us;16396us;2us;16392us;2us;70us;10us;16397us;0us;16399us;0us;16400us;0us;16401us;0us;16402us;3us;16394us;13us;6us;14us;5us;15us;4us;1us;32768us;1us;36us;1us;16404us;1us;36us;1us;32768us;7us;28us;0us;16403us;1us;16405us;5us;30us;1us;16404us;1us;36us;0us;16406us;0us;16407us;1us;16407us;9us;37us;0us;16408us;0us;16409us;0us;16410us;1us;32768us;8us;38us;1us;32768us;11us;40us;0us;16411us;6us;16413us;1us;36us;6us;53us;13us;6us;14us;5us;15us;4us;20us;62us;1us;32768us;10us;42us;0us;16412us;6us;16413us;1us;36us;6us;53us;13us;6us;14us;5us;15us;4us;20us;62us;0us;16414us;0us;16415us;0us;16416us;0us;16417us;1us;32768us;7us;49us;0us;16418us;1us;32768us;1us;36us;1us;16404us;1us;36us;0us;16419us;2us;32768us;1us;36us;20us;62us;1us;32768us;7us;55us;0us;16420us;1us;32768us;7us;57us;0us;16421us;2us;16422us;4us;74us;9us;16432us;0us;16423us;0us;16424us;0us;16425us;0us;16426us;1us;32768us;9us;64us;2us;32768us;1us;36us;20us;62us;1us;32768us;8us;66us;0us;16427us;1us;16428us;5us;68us;2us;32768us;1us;36us;20us;62us;0us;16429us;1us;32768us;1us;71us;0us;16430us;0us;16431us;1us;16432us;4us;74us;1us;32768us;1us;36us;0us;16433us;|]
let _fsyacc_actionTableRowOffsets = [|0us;2us;3us;4us;5us;6us;7us;8us;9us;13us;15us;17us;19us;20us;21us;25us;26us;29us;31us;32us;35us;36us;37us;38us;39us;43us;45us;47us;49us;50us;52us;54us;55us;56us;58us;59us;60us;61us;63us;65us;66us;73us;75us;76us;83us;84us;85us;86us;87us;89us;90us;92us;94us;95us;98us;100us;101us;103us;104us;107us;108us;109us;110us;111us;113us;116us;118us;119us;121us;124us;125us;127us;128us;129us;131us;133us;|]
let _fsyacc_reductionSymbolCounts = [|1us;1us;1us;1us;1us;1us;1us;5us;0us;1us;0us;2us;3us;0us;1us;2us;1us;1us;1us;5us;0us;1us;3us;1us;1us;1us;1us;7us;3us;0us;2us;1us;1us;1us;2us;3us;3us;2us;1us;1us;1us;1us;1us;4us;1us;3us;2us;1us;1us;3us;|]
let _fsyacc_productionToNonTerminalTable = [|0us;1us;2us;3us;3us;3us;4us;5us;6us;6us;7us;7us;8us;9us;9us;9us;10us;11us;11us;12us;13us;13us;13us;14us;15us;16us;17us;18us;19us;20us;20us;21us;21us;21us;22us;23us;24us;25us;26us;27us;27us;27us;28us;29us;30us;30us;31us;32us;33us;33us;|]
let _fsyacc_immediateActions = [|65535us;49152us;16385us;16386us;16387us;16388us;16389us;16390us;65535us;65535us;65535us;65535us;16391us;16393us;65535us;16395us;65535us;65535us;16396us;65535us;16399us;16400us;16401us;16402us;65535us;65535us;65535us;65535us;16403us;65535us;65535us;16406us;16407us;65535us;16408us;16409us;16410us;65535us;65535us;16411us;65535us;65535us;16412us;65535us;16414us;16415us;16416us;16417us;65535us;16418us;65535us;65535us;16419us;65535us;65535us;16420us;65535us;16421us;65535us;16423us;16424us;16425us;16426us;65535us;65535us;65535us;16427us;65535us;65535us;16429us;65535us;16430us;16431us;65535us;65535us;16433us;|]
let _fsyacc_reductions = lazy [|
# 266 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> ApexLexerParser.Apex.CompilationUnit in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : 'gentype__startcompilationUnit));
# 275 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_typeDeclaration in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 34 "Parser.fsy"
                                            _1
                   )
# 34 "Parser.fsy"
                 : ApexLexerParser.Apex.CompilationUnit));
# 286 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_classDeclaration in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 38 "Parser.fsy"
                                              _1
                   )
# 38 "Parser.fsy"
                 : 'gentype_typeDeclaration));
# 297 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 42 "Parser.fsy"
                                   ApexLexerParser.Apex.Modifier.Public(ApexLexerParser.Location.no_loc)
                   )
# 42 "Parser.fsy"
                 : 'gentype_modifier));
# 307 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 43 "Parser.fsy"
                                    ApexLexerParser.Apex.Modifier.Private(ApexLexerParser.Location.no_loc)
                   )
# 43 "Parser.fsy"
                 : 'gentype_modifier));
# 317 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 44 "Parser.fsy"
                                   ApexLexerParser.Apex.Modifier.Static(ApexLexerParser.Location.no_loc)
                   )
# 44 "Parser.fsy"
                 : 'gentype_modifier));
# 327 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> string in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 48 "Parser.fsy"
                               ApexLexerParser.Common.Identifier(ApexLexerParser.Location.no_loc, _1 )
                   )
# 48 "Parser.fsy"
                 : 'gentype_identifier));
# 338 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_annotationOption in
            let _2 = parseState.GetInput(2) :?> 'gentype_modifierList in
            let _4 = parseState.GetInput(4) :?> 'gentype_identifier in
            let _5 = parseState.GetInput(5) :?> 'gentype_classBody in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 52 "Parser.fsy"
                                                                                     ApexLexerParser.Apex.CompilationUnit.ClassDeclaration(ApexLexerParser.Location.no_loc, _1, _2, _4, _5)
                   )
# 52 "Parser.fsy"
                 : 'gentype_classDeclaration));
# 352 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 56 "Parser.fsy"
                             None 
                   )
# 56 "Parser.fsy"
                 : 'gentype_annotationOption));
# 362 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_annotation in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 57 "Parser.fsy"
                                        Some _1 
                   )
# 57 "Parser.fsy"
                 : 'gentype_annotationOption));
# 373 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 60 "Parser.fsy"
                             [] 
                   )
# 60 "Parser.fsy"
                 : 'gentype_modifierList));
# 383 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_modifier in
            let _2 = parseState.GetInput(2) :?> 'gentype_modifierList in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 61 "Parser.fsy"
                                                   _1 :: _2 
                   )
# 61 "Parser.fsy"
                 : 'gentype_modifierList));
# 395 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = parseState.GetInput(2) :?> 'gentype_classBodyDeclarationList in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 64 "Parser.fsy"
                                                                            _2
                   )
# 64 "Parser.fsy"
                 : 'gentype_classBody));
# 406 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 68 "Parser.fsy"
                             [] 
                   )
# 68 "Parser.fsy"
                 : 'gentype_classBodyDeclarationList));
# 416 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_classBodyDeclaration in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 69 "Parser.fsy"
                                                  [_1] 
                   )
# 69 "Parser.fsy"
                 : 'gentype_classBodyDeclarationList));
# 427 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_classBodyDeclaration in
            let _2 = parseState.GetInput(2) :?> 'gentype_classBodyDeclarationList in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 70 "Parser.fsy"
                                                                           _1 :: _2 
                   )
# 70 "Parser.fsy"
                 : 'gentype_classBodyDeclarationList));
# 439 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_memberDeclaration in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 73 "Parser.fsy"
                                              _1
                   )
# 73 "Parser.fsy"
                 : 'gentype_classBodyDeclaration));
# 450 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_fieldDeclaration in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 77 "Parser.fsy"
                                             _1
                   )
# 77 "Parser.fsy"
                 : 'gentype_memberDeclaration));
# 461 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_methodDeclaration in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 78 "Parser.fsy"
                                              _1
                   )
# 78 "Parser.fsy"
                 : 'gentype_memberDeclaration));
# 472 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_annotationOption in
            let _2 = parseState.GetInput(2) :?> 'gentype_modifierList in
            let _3 = parseState.GetInput(3) :?> 'gentype_typeRef in
            let _4 = parseState.GetInput(4) :?> 'gentype_variableDeclarators in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 82 "Parser.fsy"
                                                                                           ApexLexerParser.Apex.Decl.FieldDeclaration(ApexLexerParser.Location.no_loc, _1, _2, _3, _4)
                   )
# 82 "Parser.fsy"
                 : 'gentype_fieldDeclaration));
# 486 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 86 "Parser.fsy"
                             [] 
                   )
# 86 "Parser.fsy"
                 : 'gentype_variableDeclarators));
# 496 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_variableDeclarator in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 87 "Parser.fsy"
                                                [_1] 
                   )
# 87 "Parser.fsy"
                 : 'gentype_variableDeclarators));
# 507 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_variableDeclarator in
            let _3 = parseState.GetInput(3) :?> 'gentype_variableDeclarators in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 88 "Parser.fsy"
                                                                          _1 :: _3 
                   )
# 88 "Parser.fsy"
                 : 'gentype_variableDeclarators));
# 519 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_id in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 92 "Parser.fsy"
                               ApexLexerParser.Apex.VariableDecl(ApexLexerParser.Location.no_loc, _1)
                   )
# 92 "Parser.fsy"
                 : 'gentype_variableDeclarator));
# 530 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_typeName in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 96 "Parser.fsy"
                                     _1
                   )
# 96 "Parser.fsy"
                 : 'gentype_typeRef));
# 541 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_id in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 101 "Parser.fsy"
                                match _1 with ApexLexerParser.Common.Identifier(_, name) -> ApexLexerParser.Apex.Type(ApexLexerParser.Location.no_loc, name)
                   )
# 101 "Parser.fsy"
                 : 'gentype_typeName));
# 552 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> string in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 105 "Parser.fsy"
                               ApexLexerParser.Common.Identifier(ApexLexerParser.Location.no_loc, _1)
                   )
# 105 "Parser.fsy"
                 : 'gentype_id));
# 563 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_annotationOption in
            let _2 = parseState.GetInput(2) :?> 'gentype_modifierList in
            let _3 = parseState.GetInput(3) :?> 'gentype_typeRef in
            let _4 = parseState.GetInput(4) :?> 'gentype_id in
            let _7 = parseState.GetInput(7) :?> 'gentype_block in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 109 "Parser.fsy"
                                                                                                   ApexLexerParser.Apex.Decl.MethodDeclaration(ApexLexerParser.Location.no_loc, _1, _2, _3, _4, _7)
                   )
# 109 "Parser.fsy"
                 : 'gentype_methodDeclaration));
# 578 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = parseState.GetInput(2) :?> 'gentype_statementList in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 113 "Parser.fsy"
                                                                 _2
                   )
# 113 "Parser.fsy"
                 : 'gentype_block));
# 589 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 117 "Parser.fsy"
                             [] 
                   )
# 117 "Parser.fsy"
                 : 'gentype_statementList));
# 599 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_statement in
            let _2 = parseState.GetInput(2) :?> 'gentype_statementList in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 118 "Parser.fsy"
                                                     _1 :: _2 
                   )
# 118 "Parser.fsy"
                 : 'gentype_statementList));
# 611 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_localVariableDeclarationStatement in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 122 "Parser.fsy"
                                                              _1
                   )
# 122 "Parser.fsy"
                 : 'gentype_statement));
# 622 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_returnStatement in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 123 "Parser.fsy"
                                            _1
                   )
# 123 "Parser.fsy"
                 : 'gentype_statement));
# 633 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_expressionStatement in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 124 "Parser.fsy"
                                                _1
                   )
# 124 "Parser.fsy"
                 : 'gentype_statement));
# 644 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_localVariableDeclaration in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 128 "Parser.fsy"
                                                          ApexLexerParser.Apex.Stmt.LocalVarDeclStmt(ApexLexerParser.Location.no_loc, _1)
                   )
# 128 "Parser.fsy"
                 : 'gentype_localVariableDeclarationStatement));
# 655 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_modifier in
            let _2 = parseState.GetInput(2) :?> 'gentype_typeRef in
            let _3 = parseState.GetInput(3) :?> 'gentype_variableDeclarators in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 132 "Parser.fsy"
                                                                 ApexLexerParser.Apex.LocalVarDecl(ApexLexerParser.Location.no_loc, _1, _2, _3)
                   )
# 132 "Parser.fsy"
                 : 'gentype_localVariableDeclaration));
# 668 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = parseState.GetInput(2) :?> 'gentype_expression in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 136 "Parser.fsy"
                                                   ApexLexerParser.Apex.Stmt.ReturnStmt(ApexLexerParser.Location.no_loc, _2)
                   )
# 136 "Parser.fsy"
                 : 'gentype_returnStatement));
# 679 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_expression in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 140 "Parser.fsy"
                                            ApexLexerParser.Apex.Stmt.ExprStmt(ApexLexerParser.Location.no_loc, _1)
                   )
# 140 "Parser.fsy"
                 : 'gentype_expressionStatement));
# 690 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_id in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 145 "Parser.fsy"
                               ApexLexerParser.Common.Expr.Id(ApexLexerParser.Location.no_loc, _1)
                   )
# 145 "Parser.fsy"
                 : 'gentype_primary));
# 701 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_primary in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 149 "Parser.fsy"
                                    _1
                   )
# 149 "Parser.fsy"
                 : 'gentype_expression));
# 712 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_methodCall in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 150 "Parser.fsy"
                                      _1
                   )
# 150 "Parser.fsy"
                 : 'gentype_expression));
# 723 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_literal in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 151 "Parser.fsy"
                                   _1
                   )
# 151 "Parser.fsy"
                 : 'gentype_expression));
# 734 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> int in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 155 "Parser.fsy"
                                ApexLexerParser.Common.Expr.IntegerLiteral(ApexLexerParser.Location.no_loc, _1)
                   )
# 155 "Parser.fsy"
                 : 'gentype_literal));
# 745 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_qualifiedName in
            let _3 = parseState.GetInput(3) :?> 'gentype_expressions in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 159 "Parser.fsy"
                                                                             ApexLexerParser.Common.Expr.ApexMethodCall(ApexLexerParser.Location.no_loc, _1, _3)
                   )
# 159 "Parser.fsy"
                 : 'gentype_methodCall));
# 757 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_expression in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 163 "Parser.fsy"
                                        [_1] 
                   )
# 163 "Parser.fsy"
                 : 'gentype_expressions));
# 768 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_expression in
            let _3 = parseState.GetInput(3) :?> 'gentype_expressions in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 164 "Parser.fsy"
                                                          _1 :: _3 
                   )
# 164 "Parser.fsy"
                 : 'gentype_expressions));
# 780 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = parseState.GetInput(2) :?> string in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 168 "Parser.fsy"
                                      ApexLexerParser.Apex.Annotation.IsTest(ApexLexerParser.Location.no_loc)
                   )
# 168 "Parser.fsy"
                 : 'gentype_annotation));
# 791 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_ids in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 172 "Parser.fsy"
                                ApexLexerParser.Common.build_qualified_name(_1)
                   )
# 172 "Parser.fsy"
                 : 'gentype_qualifiedName));
# 802 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_id in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 175 "Parser.fsy"
                                [_1] 
                   )
# 175 "Parser.fsy"
                 : 'gentype_ids));
# 813 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_id in
            let _3 = parseState.GetInput(3) :?> 'gentype_ids in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 176 "Parser.fsy"
                                        _1 :: _3 
                   )
# 176 "Parser.fsy"
                 : 'gentype_ids));
|]
# 826 "Parser.fs"
let tables : FSharp.Text.Parsing.Tables<_> = 
  { reductions = _fsyacc_reductions.Value;
    endOfInputTag = _fsyacc_endOfInputTag;
    tagOfToken = tagOfToken;
    dataOfToken = _fsyacc_dataOfToken; 
    actionTableElements = _fsyacc_actionTableElements;
    actionTableRowOffsets = _fsyacc_actionTableRowOffsets;
    stateToProdIdxsTableElements = _fsyacc_stateToProdIdxsTableElements;
    stateToProdIdxsTableRowOffsets = _fsyacc_stateToProdIdxsTableRowOffsets;
    reductionSymbolCounts = _fsyacc_reductionSymbolCounts;
    immediateActions = _fsyacc_immediateActions;
    gotos = _fsyacc_gotos;
    sparseGotoTableRowOffsets = _fsyacc_sparseGotoTableRowOffsets;
    tagOfErrorTerminal = _fsyacc_tagOfErrorTerminal;
    parseError = (fun (ctxt:FSharp.Text.Parsing.ParseErrorContext<_>) -> 
                              match parse_error_rich with 
                              | Some f -> f ctxt
                              | None -> parse_error ctxt.Message);
    numTerminals = 24;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = tables.Interpret(lexer, lexbuf, startState)
let compilationUnit lexer lexbuf : ApexLexerParser.Apex.CompilationUnit =
    engine lexer lexbuf 0 :?> _
