module ApexLexer.Tests

open NUnit.Framework

[<SetUp>]
let Setup () =
    ()

[<Test>]
let Test1 () =
    Assert.Pass()

let Test2 () = 
    Assert.AreEqual(2, Say.add 1 1)

let Test3 () = 
    Assert.AreEqual(5, Say.add 2 3 )