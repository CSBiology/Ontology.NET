namespace Ontology.NET.Tests


open Expecto
open Graphoscope
open FSharpAux

open ControlledVocabulary
open Ontology.NET


module OntologyTests =

    [<Tests>]
    let ontologyTest =
        testList "Ontology" [

            testList "fromOboOntology" [
                testCase "parses OboOntology correctly" <| fun _ ->
                    let oboO = OBO.OboOntology.fromFile false (System.IO.Path.Combine(__SOURCE_DIRECTORY__, "OBO", "References", "testOboFile3.obo"))
                    let actual = Ontology.fromOboOntology oboO |> FGraph.toSeq |> Seq.toList
                    let expected = [("TO3:01", { Accession = "TO3:01"; Name = "testTerm1"; RefUri = "TO3" }, "TO3:02", { Accession = "TO3:02"; Name = "testTerm2"; RefUri = "TO3" }, set [IsA]); ("TO3:01", { Accession = "TO3:01"; Name = "testTerm1"; RefUri = "TO3" }, "TO4:1", { Accession = "TO4:1"; Name = "<missing>"; RefUri = "<missing>" }, set [Xref]); ("TO3:02", { Accession = "TO3:02"; Name = "testTerm2"; RefUri = "TO3" }, "TO5:02", { Accession = "TO5:02"; Name = "<missing>"; RefUri = "<missing>" }, set [Xref]); ("TO3:03", { Accession = "TO3:03"; Name = "testTerm3"; RefUri = "TO3" }, "TO3:02", {Accession = "TO3:02"; Name = "testTerm2"; RefUri = "TO3" }, set [Custom "has_a"])]
                    Expect.sequenceEqual actual expected "Ontology seqs differ"
            ]

            testList "AddTerm" [
                testCase "adds term correctly" <| fun _ ->
                    let testOnto = Ontology()
                    let testTerm = CvTerm.create("test:0", "testTerm", "test")
                    testOnto.AddTerm testTerm |> ignore
                    Expect.isTrue (FGraph.containsNode testTerm.Accession testOnto) "Does not contain newly added test term"
            ]

            testList "AddRelation" [
                testCase "adds relation correctly" <| fun _ ->
                    let testOnto = Ontology()
                    ["test:0"; "test:1"]
                    |> List.iter (
                        fun id ->
                            FGraph.addNode id (CvTerm.create(id, "testTerm", "test")) testOnto |> ignore
                    )
                    testOnto.AddRelation("test:0", "test:1", Custom "") |> ignore
                    let actual = 
                        try FGraph.findEdge "test:0" "test:1" testOnto |> Some with
                        | _ -> None
                    let expected = Some ("test:0", "test:1", set [Custom ""])
                    Expect.isSome actual "is None although it should be Some"
                    Expect.equal actual expected "id * id * Relation set differ"
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

            testList "GetTargetTermsWithXrefsBy" [
                 testCase "returns all target terms correctly, Case: Hund is_a ..." <| fun _ ->
                     let actual = ReferenceObjects.testOnto2.GetTargetTermsWithXrefsBy("Hund", fun _ _ relations -> Set.contains IsA relations) |> Seq.toList
                     let expected = ["Räuber"; "Höhere Säugetiere"; "Höhere Säuger"; "Eutheria"; "Raubtiere"; "Carnivora"; "Laurasiatheria"]
                     Expect.sequenceEqual actual expected "Target terms differ"

                 testCase "returns all target terms correctly, Case: Carnivora Sprache ..." <| fun _ ->
                     let actual = ReferenceObjects.testOnto2.GetTargetTermsWithXrefsBy("Carnivora", fun _ _ relations -> Set.contains (Custom "Sprache") relations) |> Seq.toList
                     let expected = ["Latein"; "Lateinisch"]
                     Expect.sequenceEqual actual expected "Target terms differ"

                 testCase "returns all target terms correctly, Case: Lateinisch ist nicht ..." <| fun _ ->
                     let actual = ReferenceObjects.testOnto2.GetTargetTermsWithXrefsBy("Lateinisch", fun _ _ relations -> Set.contains (Custom "ist nicht") relations) |> Seq.toList
                     let expected = ["Deutsch"]
                     Expect.sequenceEqual actual expected "Target terms differ"

                 testCase "returns all target terms correctly, Case: term ID has space(s)" <| fun _ ->
                     let actual = ReferenceObjects.testOnto2.GetTargetTermsWithXrefsBy("Eutheria", fun termID _ _ -> String.contains " " termID) |> Seq.toList
                     let expected = ["Höhere Säugetiere"; "Höhere Säuger"]
                     Expect.sequenceEqual actual expected "Target terms differ"
            ]

        ]