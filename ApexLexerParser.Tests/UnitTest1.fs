module ApexLexerParser.Tests

open FSharp.Text.Lexing

open NUnit.Framework

[<SetUp>]
let Setup () = ()

[<Test>]
let ``it should be able to parse simple empty class defintion`` () =
    let rawString = "public class Foo{}"
    let lexbuf = LexBuffer<char>.FromString rawString
    let parser = Parser.compilationUnit Lexer.read_token lexbuf

    let expected =
        Apex.CompilationUnit.ClassDeclaration(
            Location.no_loc,
            None,
            [ Apex.Modifier.Public(Location.no_loc) ],
            Common.Identifier(Location.no_loc, "Foo"),
            []
        )

    Assert.AreEqual(expected, parser)

[<Test>]
let ``it should be able to parse method decl within class`` () =
    let rawString =
        "public class Foo{\
        public Integer bar() {\
            return 5;\
        }\
    }"

    let lexbuf = LexBuffer<char>.FromString rawString
    let parser = Parser.compilationUnit Lexer.read_token lexbuf

    let expected =
        Apex.CompilationUnit.ClassDeclaration(
            Location.no_loc,
            None,
            [ Apex.Modifier.Public(Location.no_loc) ],
            Common.Identifier(Location.no_loc, "Foo"),
            [ Apex.MethodDeclaration(
                  Location.no_loc,
                  None,
                  [ Apex.Modifier.Public(Location.no_loc) ],
                  Apex.Type(Location.no_loc, "Integer"),
                  Common.Identifier(Location.no_loc, "bar"),
                  [ Apex.Stmt.ReturnStmt(Location.no_loc, Common.Expr.IntegerLiteral(Location.no_loc, 5)) ]
              ) ]
        )

    Assert.AreEqual(expected, parser)

[<Test>]
let ``it should be able to create a no loc`` () =
    let rawString = "public class Foo{}"

    let lexbuf = LexBuffer<char>.FromString rawString
    let shouldBePublic = Lexer.read_token lexbuf
    let shouldBeClass = Lexer.read_token lexbuf
    let shouldBeFoo = Lexer.read_token lexbuf
    let shouldBeLeftFancyBracket = Lexer.read_token lexbuf
    let shouldBeRightFancyBracket = Lexer.read_token lexbuf

    Assert.AreEqual(Parser.PUBLIC, shouldBePublic)
    Assert.AreEqual(Parser.CLASS, shouldBeClass)
    Assert.AreEqual(Parser.ID("Foo"), shouldBeFoo)
    Assert.AreEqual(Parser.LEFT_BRACE, shouldBeLeftFancyBracket)
    Assert.AreEqual(Parser.RIGHT_BRACE, shouldBeRightFancyBracket)

[<Test>]
let ``it should be to build an identifier from a set of identifiers`` () =
    // Foo.Bar.CONSTANT
    let fooIdentifier =
        ApexLexerParser.Common.Identifier(ApexLexerParser.Location.no_loc, "Foo")

    let barIdentifier =
        ApexLexerParser.Common.Identifier(ApexLexerParser.Location.no_loc, "Bar")

    let constantIdentifier =
        ApexLexerParser.Common.Identifier(ApexLexerParser.Location.no_loc, "CONSTANT")

    let expected =
        ApexLexerParser.Common.Identifier(ApexLexerParser.Location.no_loc, "Foo.Bar.CONSTANT")

    let actual =
        ApexLexerParser.Common.build_qualified_name ([ fooIdentifier; barIdentifier; constantIdentifier ])

    Assert.AreEqual(expected, actual)

[<Test>]
let ``it should be able to build an identifier from just a single identifier`` () =
    let fooIdentifier =
        ApexLexerParser.Common.Identifier(ApexLexerParser.Location.no_loc, "Foo")

    let expected = fooIdentifier

    Assert.AreEqual(expected, ApexLexerParser.Common.build_qualified_name [ fooIdentifier ])

[<Test>]
let ``it should throw an exception when an empty list is given to build qualified name`` () =
    Assert.Throws(fun () -> ApexLexerParser.Common.build_qualified_name [] |> ignore)
    |> ignore
