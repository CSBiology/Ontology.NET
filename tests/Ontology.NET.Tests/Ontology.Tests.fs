namespace Ontology.NET.Tests


open Expecto
open Graphoscope

open ControlledVocabulary
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

                testCase "returns correct Xrefs on term ID test:04" <| fun _ ->
                    let actual = ReferenceObjects.testOnto1.GetXrefs "test:04" |> Seq.toList
                    Expect.isTrue (List.isEmpty actual) "Xref list is not empty"
            ]

            testList "AddTerm" [
                testCase "adds terms correctly" <| fun _ ->
                    let testOnto = Ontology()
                    let testTerm = CvTerm.create("test:0", "testTerm", "test")
                    testOnto.AddTerm testTerm |> ignore
                    Expect.isTrue (FGraph.containsNode testTerm.Accession testOnto) "Does not contain newly added test term"
            ]

            testList "RemoveTerm" [
                testCase "removes term correctly" <| fun _ ->
                    let testOnto = Ontology()
                    FGraph.addNode "test:0" (CvTerm.create("test:0", "testTerm", "test")) testOnto |> ignore
                    testOnto.RemoveTerm "test:0" |> ignore
                    Expect.isEmpty testOnto "Does contain any test term"
            ]

            testList "RemoveRelations" [
                testCase "removes relations correctly" <| fun _ ->
                    let testOntology = Ontology()
                    FGraph.addElement "1" (CvTerm.create "") "2" (CvTerm.create "") (Set.singleton <| Custom "") testOntology |> ignore
                    testOntology.RemoveRelations("1", "2") |> ignore
                    Expect.isFalse (FGraph.containsEdge "1" "2" testOntology) "Does contain relation(s) that should be removed"
            ]

            testList "RemoveRelation" [
                testCase "removes single relation correctly" <| fun _ ->
                    let testOntology = Ontology()
                    FGraph.addElement "1" (CvTerm.create "") "2" (CvTerm.create "") (Set [Custom ""; Xref]) testOntology |> ignore
                    testOntology.RemoveRelation("1", "2", Custom "") |> ignore
                    let expected = Set [Xref]
                    let _, _, actual = FGraph.findEdge "1" "2" testOntology
                    Expect.sequenceEqual actual expected "Relations are different"
            ]

            testList "GetTargetTermsWithXrefsBy" [
                testCase "returns all target terms correctly" <| fun _ ->
                    
            ]

        ]