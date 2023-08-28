module ApexLexerParser.Tests

open FSharp.Text.Lexing

open NUnit.Framework

[<SetUp>]
let Setup () = ()

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
    Assert.AreEqual(Parser.ID("foo"), shouldBeFoo)
    Assert.AreEqual(Parser.LEFT_BRACE, shouldBeLeftFancyBracket)
    Assert.AreEqual(Parser.RIGHT_BRACE, shouldBeRightFancyBracket)

[<Test>]
let ``it should be to build an identifier from a set of identifiers`` () =
    // Foo.Bar.CONSTANT
    let fooIdentifier = ApexLexer.Common.Identifier(ApexLexer.Location.no_loc, "Foo")
    let barIdentifier = ApexLexer.Common.Identifier(ApexLexer.Location.no_loc, "Bar")

    let constantIdentifier =
        ApexLexer.Common.Identifier(ApexLexer.Location.no_loc, "CONSTANT")

    let expected =
        ApexLexer.Common.Identifier(ApexLexer.Location.no_loc, "Foo.Bar.CONSTANT")

    let actual =
        ApexLexer.Common.build_qualified_name ([ fooIdentifier; barIdentifier; constantIdentifier ])

    Assert.AreEqual(expected, actual)

[<Test>]
let ``it should be able to build an identifier from just a single identifier`` () =
    let fooIdentifier = ApexLexer.Common.Identifier(ApexLexer.Location.no_loc, "Foo");
    let expected = fooIdentifier

    Assert.AreEqual(expected, ApexLexer.Common.build_qualified_name [fooIdentifier])

[<Test>]
let ``it should throw an exception when an empty list is given to build qualified name`` () =
    Assert.Throws(fun () -> ApexLexer.Common.build_qualified_name [] |> ignore) |> ignore