namespace Ontology.NET.OBO.Tests


open Expecto

open Ontology.NET.OBO
open OboProvider



module OboProviderTests =

    // use this for in-IDE support.
    //let [<Literal>] oboPath = @"tests/Ontology.NET.Tests/OBO/References/go.obo"

    // use this for performing the test execution
    // ATTENTION! Seems to be dependent on version of SDK. Sometimes this is enough: [.NET 8]
    //let [<Literal>] oboPath = @"tests/Ontology.NET.Tests/OBO/References/go.obo"
    // other times it must be like this: [.NET 9]
    let [<Literal>] oboPath = @"OBO/References/go.obo"

    type goTerms = OboTermsProvider<oboPath>
    type goTypeDefs = OboTypeDefsProvider<oboPath>

    [<Tests>]
    let oboOntologyTest =
        testList "OboProvider" [
            test "go term 'adult gena' is provided" {
                let actual = goTerms.``adult gena``
                Expect.equal actual.Id "TGMA:0000031" "Expected term ID for 'adult gena' to be TGMA:0000031"
                Expect.equal actual.Name "adult gena" "Expected term name for 'adult gena' to be 'adult gena'"
                Expect.equal actual.Synonyms.Length 7 "Expected 'adult gena' to have 7 synonyms"
            }
            test "go typedef is provied" {
                let actual = goTypeDefs.``part of``
                Expect.equal actual.Id "part_of" "Expected typedef ID for 'part of' to be 'part_of'"
                Expect.equal actual.Name "part of" "Expected typedef Name for 'part of' to be 'part of'"
                Expect.isTrue actual.Is_transitive "Expected typedef 'part of' to be transitive"
            }
        ]