namespace Ontology.NET.Tests


open Expecto

open Ontology.NET


module OntologyTests =

    [<Tests>]
    let ontologyTest =
        testList "Ontology" [

            testList "GetXrefs" [
                testCase "returns correct Xrefs on term ID test:01" <| fun _ ->
                    let actual = ReferenceObjects.testOnto1.GetXrefs "test:01" |> Seq.toList
                    let expected = ["test:02"; "test:03"]
                    Expect.sequenceEqual actual expected "Xref list is not correct"

                testCase "returns correct Xrefs on term ID test:02" <| fun _ ->
                    let actual = ReferenceObjects.testOnto1.GetXrefs "test:02" |> Seq.toList
                    let expected = ["test:03"; "test:01"]
                    Expect.sequenceEqual actual expected "Xref list is not correct"

                testCase "returns correct Xrefs on term ID test:03" <| fun _ ->
                    let actual = ReferenceObjects.testOnto1.GetXrefs "test:03" |> Seq.toList
                    let expected = ["test:02"; "test:01"]
                    Expect.sequenceEqual actual expected "Xref list is not correct"
            ]

        ]