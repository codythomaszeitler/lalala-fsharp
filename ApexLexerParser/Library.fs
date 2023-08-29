namespace ApexLexerParser

module Location =
    type Location = { line: int; row: int; column: int }

    let no_loc: Location = { line = 0; row = 0; column = 0 }

module Common =
    type Identifier = Identifier of Location.Location * string

    let build_qualified_name identifiers : Identifier =
        if List.isEmpty identifiers then failwith("Cannot build qualified name with an empty list of identifiers.")
        let get_name =
            function
            | Identifier(_, name) -> name in

        let rec get_qualified_name_string (identifiers: Identifier list) : string =
            match identifiers with
            | h :: [] -> get_name h
            | h :: t -> get_name h + "." + get_qualified_name_string t
            | [] -> "" in

        Identifier(Location.no_loc, get_qualified_name_string identifiers)

    type Operator =
        | Add of Location.Location
        | Sub of Location.Location
        | Mul of Location.Location
        | Div of Location.Location

    type Expr =
        | Id of Location.Location * Identifier
        | Binary of Location.Location * Expr * Operator * Expr
        | IntegerLiteral of Location.Location * int
        | LongLiteral of Location.Location * int
        | StringLiteral of Location.Location * string
        | BooleanLiteral of Location.Location * bool
        | NullLiteral of Location.Location
        | ApexMethodCall of Location.Location * Identifier * Expr list

module Apex =

    type Modifier =
        | Global of Location.Location
        | Public of Location.Location
        | Protected of Location.Location
        | Private of Location.Location
        | Transient of Location.Location
        | Static of Location.Location
        | Abstract of Location.Location
        | Final of Location.Location
        | Webservice of Location.Location
        | Override of Location.Location
        | Virtual of Location.Location
        | Testmethod of Location.Location
        | WithSharing of Location.Location
        | WithoutSharing of Location.Location
        | InheritedSharing of Location.Location

    type Type = Type of Location.Location * string

    type VariableDecl = VariableDecl of Location.Location * Common.Identifier

    type LocalVarDecl = LocalVarDecl of Location.Location * Modifier * Type * VariableDecl list

    type Annotation = IsTest of Location.Location

    type Stmt =
        | LocalVarDeclStmt of Location.Location * LocalVarDecl
        | ReturnStmt of Location.Location * Common.Expr
        | ExprStmt of Location.Location * Common.Expr

    type Decl =
        | FieldDeclaration of Location.Location * Annotation option * Modifier list * Type * VariableDecl list
        | MethodDeclaration of
            Location.Location *
            Annotation option *
            Modifier list *
            Type *
            Common.Identifier *
            Stmt list
    type CompilationUnit =
        | ClassDeclaration of
            Location.Location
            * Annotation option
            * Modifier list
            * Common.Identifier
            * Decl list
