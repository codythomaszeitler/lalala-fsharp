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
    let input = "(1 + 2) - 3"
    let lexbuf = LexBuffer<char>.FromString input
    Assert.AreEqual(0, lexbuf.StartPos.pos_lnum)