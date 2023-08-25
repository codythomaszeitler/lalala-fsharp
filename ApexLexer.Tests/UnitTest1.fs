module ApexLexer.Tests

open NUnit.Framework

[<SetUp>]
let Setup () = ()

[<Test>]
let ``it should be able to create a no loc`` () =
    let location = Location.no_loc
    let no_location = { line = 0; row = 0; column = 0 }: Location.Location
    Assert.AreEqual(location, no_location)
