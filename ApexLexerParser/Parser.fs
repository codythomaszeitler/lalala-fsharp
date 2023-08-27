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
let _fsyacc_gotos = [| 0us;65535us;1us;65535us;0us;1us;|]
let _fsyacc_sparseGotoTableRowOffsets = [|0us;1us;|]
let _fsyacc_stateToProdIdxsTableElements = [| 1us;0us;1us;0us;|]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us;2us;|]
let _fsyacc_action_rows = 2
let _fsyacc_actionTableElements = [|0us;16385us;0us;49152us;|]
let _fsyacc_actionTableRowOffsets = [|0us;1us;|]
let _fsyacc_reductionSymbolCounts = [|1us;0us;|]
let _fsyacc_productionToNonTerminalTable = [|0us;1us;|]
let _fsyacc_immediateActions = [|65535us;49152us;|]
let _fsyacc_reductions = lazy [|
# 186 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> string in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : 'gentype__startcompilationUnit));
# 195 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 34 "Parser.fsy"
                              "Nothing to see here" 
                   )
# 34 "Parser.fsy"
                 : string));
|]
# 206 "Parser.fs"
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
let compilationUnit lexer lexbuf : string =
    engine lexer lexbuf 0 :?> _