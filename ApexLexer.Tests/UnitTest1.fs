module ApexLexer.Tests

open FSharp.Text.Lexing
open Lexer
open Parser

open NUnit.Framework

[<SetUp>]
let Setup () =
    ()

[<Test>]
let Test4 () =
    let input = "2"
    let lexbuf = LexBuffer<char>.FromString input
    let output = Parser.parse Lexer.tokenize lexbuf
    Assert.AreEqual("2", string output)