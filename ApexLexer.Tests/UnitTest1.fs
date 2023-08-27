module ApexLexer.Tests

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
