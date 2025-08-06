namespace Ontology.NET.Tests


open Expecto
open Graphoscope
open FSharpAux

open ControlledVocabulary
open Ontology.NET
open Ontology.NET.OBO


open type Ontology.NET.RelationType


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

            testList "GetTerm" [
                testCase "gets term correctly" <| fun _ ->
                    let actual = ReferenceObjects.testOnto1.GetTerm("test:01")
                    let expected = CvTerm.create("test:01", "Frosch", "test")
                    Expect.equal actual expected "CvTerms differ"
            ]

            testList "GetRelations" [
                testCase "gets relations correctly" <| fun _ ->
                    let actual = ReferenceObjects.testOnto1.GetRelations("test:01", "test:02")
                    let expected = set [Xref]
                    Expect.equal actual expected "Relations are not equal"
            ]

            testList "RemoveTerm" [
                testCase "removes term correctly" <| fun _ ->
                    let testOnto = Ontology()
                    FGraph.addNode "test:0" (CvTerm.create("test:0", "testTerm", "test")) testOnto |> ignore
                    testOnto.RemoveTerm "test:0" |> ignore
                    Expect.isEmpty testOnto "Does contain any test term"
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

            testList "RemoveRelations" [
                testCase "removes relations correctly" <| fun _ ->
                    let testOntology = Ontology()
                    FGraph.addElement "1" (CvTerm.create "") "2" (CvTerm.create "") (Set.singleton <| Custom "") testOntology |> ignore
                    testOntology.RemoveRelations("1", "2") |> ignore
                    Expect.isFalse (FGraph.containsEdge "1" "2" testOntology) "Does contain relation(s) that should be removed"
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

            testList "GetTargetTermRelations" [
                testCase "returns all target term relations correctly" <| fun _ ->
                    let actual = ReferenceObjects.testOnto1.GetTargetTermRelations("test:01") |> Seq.toList
                    let expected = ["test:02", Set.singleton Xref; "test:04", Set.singleton IsA]
                    Expect.sequenceEqual actual expected "Target term relations differ"
            ]

            testList "GetTargetTermsBy" [
                testCase "returns target terms correctly" <| fun _ ->
                    let actual = ReferenceObjects.testOnto1.GetTargetTermsBy("test:01", fun _ _ e -> Set.contains IsA e) |> Seq.toList
                    let expected = ["test:04"]
                    Expect.sequenceEqual actual expected "Target term list differs"
            ]

            testList "GetTargetTermsWithXrefsBy" [
                testCase "returns all target terms and their Xrefs correctly, Case: Hund is_a ..." <| fun _ ->
                    let actual = ReferenceObjects.testOnto2.GetTargetTermsWithXrefsBy("Hund", fun _ _ relations -> Set.contains IsA relations) |> Seq.toList
                    let expected = ["Räuber"; "Höhere Säugetiere"; "Höhere Säuger"; "Eutheria"; "Raubtiere"; "Carnivora"; "Laurasiatheria"]
                    Expect.sequenceEqual actual expected "Target terms and/or their Xrefs differ"

                testCase "returns all target terms and their Xrefs correctly, Case: Carnivora Sprache ..." <| fun _ ->
                    let actual = ReferenceObjects.testOnto2.GetTargetTermsWithXrefsBy("Carnivora", fun _ _ relations -> Set.contains (Custom "Sprache") relations) |> Seq.toList
                    let expected = ["Latein"; "Lateinisch"]
                    Expect.sequenceEqual actual expected "Target terms and/or their Xrefs differ"

                testCase "returns all target terms and their Xrefs correctly, Case: Lateinisch ist nicht ..." <| fun _ ->
                    let actual = ReferenceObjects.testOnto2.GetTargetTermsWithXrefsBy("Lateinisch", fun _ _ relations -> Set.contains (Custom "ist nicht") relations) |> Seq.toList
                    let expected = ["Deutsch"]
                    Expect.sequenceEqual actual expected "Target terms and/or their Xrefs differ"

                testCase "returns all target terms and their Xrefs correctly, Case: term ID has space(s)" <| fun _ ->
                    let actual = ReferenceObjects.testOnto2.GetTargetTermsWithXrefsBy("Eutheria", fun termID _ _ -> String.contains " " termID) |> Seq.toList
                    let expected = ["Höhere Säugetiere"; "Höhere Säuger"]
                    Expect.sequenceEqual actual expected "Target terms and/or their Xrefs differ"
            ]

            testList "GetTargetTermsWithDepth" [
                testCase "returns all target terms correctly" <| fun _ ->
                    let actual = ReferenceObjects.testOnto2.GetTargetTermsWithDepth("Hund",2) |> Seq.toList
                    let expected = ["Deutsch"; "Carnivora"; "Lateinisch"; "Laurasiatheria"; "Raubtiere"]
                    Expect.sequenceEqual actual expected "Target terms differ"
            ]

            testList "GetTargetTermsWithXrefsWithDepth" [
                testCase "returns all target terms and their Xrefs correctly" <| fun _ ->
                    let actual = ReferenceObjects.testOnto2.GetTargetTermsWithXrefsWithDepth("Hund",2) |> Seq.toList
                    let expected = ["Deutsch"; "Räuber"; "Höhere Säugetiere"; "Höhere Säuger"; "Eutheria"; "Latein"; "Lateinisch"; "Raubtiere"; "Carnivora"; "Laurasiatheria"]
                    Expect.sequenceEqual actual expected "Target terms and/or their Xrefs differ"
            ]

            testList "GetTargetTermsWithDepthBy" [
                testCase "returns all target terms correctly" <| fun _ ->
                    let actual = ReferenceObjects.testOnto2.GetTargetTermsWithDepthBy("Hund", 2, fun _ _ e -> Set.contains IsA e || Set.contains (Custom "Sprache") e) |> Seq.toList
                    let expected = ["Deutsch"; "Carnivora"; "Lateinisch"; "Laurasiatheria"]
                    Expect.sequenceEqual actual expected "Target terms differ"
            ]

            testList "GetTargetTermsWithXrefsWithDepthBy" [
                testCase "returns all target terms and their Xrefs correctly" <| fun _ ->
                    let actual = ReferenceObjects.testOnto2.GetTargetTermsWithXrefsWithDepthBy("Hund", 2, fun _ _ e -> Set.contains IsA e) |> Seq.toList
                    let expected = ["Räuber"; "Höhere Säugetiere"; "Höhere Säuger"; "Eutheria"; "Raubtiere"; "Carnivora"; "Laurasiatheria"]
                    Expect.sequenceEqual actual expected "Target terms and/or their Xrefs differ"
            ]

            testList "GetSourceTermRelations" [
                testCase "returns all source term relations correctly" <| fun _ ->
                    let actual = ReferenceObjects.testOnto2.GetSourceTermRelations("Deutsch") |> Seq.toList
                    let expected = ["Hund", Set.singleton (Custom "Sprache"); "Lateinisch", Set.singleton (Custom "ist nicht")]
                    Expect.sequenceEqual actual expected "Source term relations differ"
            ]

            testList "GetSourceTermsBy" [
                testCase "returns all source terms correctly" <| fun _ ->
                    let actual = ReferenceObjects.testOnto2.GetSourceTermsBy("Deutsch", fun _ _ e -> Set.contains (Custom "Sprache") e) |> Seq.toList
                    let expected = List.singleton "Hund"
                    Expect.sequenceEqual actual expected "Source terms differ"
            ]

            testList "GetSourceTermsWithXrefsBy" [
                testCase "returns all source terms and their Xrefs correctly" <| fun _ ->
                    let actual = ReferenceObjects.testOnto2.GetSourceTermsWithXrefsBy("Deutsch", fun _ _ e -> Set.contains (Custom "Sprache") e || Set.contains (Custom "ist nicht") e) |> Seq.toList
                    let expected = ["Latein"; "Lateinisch"; "Räuber"; "Raubtiere"; "Carnivora"; "Höhere Säugetiere"; "Höhere Säuger"; "Eutheria"; "Hund"]
                    Expect.sequenceEqual actual expected "Source terms and/or their Xrefs differ"
            ]

            testList "GetSourceTermsWithDepth" [
                testCase "returns all source terms correctly" <| fun _ ->
                    let actual = ReferenceObjects.testOnto2.GetSourceTermsWithDepth("Deutsch", 2) |> Seq.toList
                    let expected = ["Lateinisch"; "Carnivora"; "Eutheria"; "Hund"]
                    Expect.sequenceEqual actual expected "Source terms differ"
            ]

            testList "GetSourceTermsWithXrefsWithDepth" [
                testCase "returns all source terms and their Xrefs correctly" <| fun _ ->
                    let actual = ReferenceObjects.testOnto2.GetSourceTermsWithXrefsWithDepth("Deutsch", 1) |> Seq.toList
                    let expected = ["Latein"; "Lateinisch"; "Hund"]
                    Expect.sequenceEqual actual expected "Source terms and/or their Xrefs differ"
            ]

            testList "GetSourceTermsWithDepthBy" [
                testCase "returns all source terms correctly" <| fun _ ->
                    let actual = ReferenceObjects.testOnto2.GetSourceTermsWithDepthBy("Deutsch", 2, fun _ _ e -> Set.contains (Custom "Sprache") e || Set.contains (Custom "ist nicht") e) |> Seq.toList
                    let expected = ["Lateinisch"; "Carnivora"; "Eutheria"; "Hund"]
                    Expect.sequenceEqual actual expected "Source terms differ"
            ]

            testList "GetSourceTermsWithXrefsWithDepthBy" [
                testCase "returns all source terms and their Xrefs correctly" <| fun _ ->
                    let actual = ReferenceObjects.testOnto2.GetSourceTermsWithXrefsWithDepthBy("Deutsch", 2, fun _ _ e -> Set.contains (Custom "Sprache") e || Set.contains (Custom "ist nicht") e) |> Seq.toList
                    let expected = ["Latein"; "Lateinisch"; "Räuber"; "Raubtiere"; "Carnivora"; "Höhere Säugetiere"; "Höhere Säuger"; "Eutheria"; "Hund"]
                    Expect.sequenceEqual actual expected "Source terms and/or their Xrefs differ"
            ]

            testList "MergeWith" [
                testCase "merges Ontologies correctly" <| fun _ ->
                    let testOnto1 =
                        let o = Ontology()
                        [CvTerm.create("TO1:1", "test1", "TO1"); CvTerm.create("TO1:2", "test2", "TO1"); CvTerm.create("TO3:1", "<missing>", "<missing>"); CvTerm.create("TO2:1", "<missing>", "<missing>")]
                        |> List.iter (fun termId -> o.AddTerm(termId) |> ignore)
                        ["TO1:1", "TO1:2", IsA; "TO1:1", "TO3:1", Xref; "TO1:1", "TO2:1", Xref]
                        |> List.map o.AddRelation
                        |> ignore
                        o
                    let testOnto2 =
                        let o = Ontology()
                        [CvTerm.create("TO2:1", "test1", "TO2"); CvTerm.create("TO2:2", "test2", "TO2"); CvTerm.create("TO3:1", "<missing>", "<missing>")]
                        |> List.iter (fun termId -> o.AddTerm(termId) |> ignore)
                        ["TO2:1", "TO2:2", Custom "has_a"; "TO2:1", "TO3:1", Xref]
                        |> List.map o.AddRelation
                        |> ignore
                        o
                    let res = testOnto1.MergeWith(testOnto2)
                    let actual1 = res.GetTerms() |> Seq.toList
                    let actual2 = res.GetAllRelations() |> Seq.toList
                    let expected1 = [
                        "TO1:1", CvTerm.create("TO1:1", "test1", "TO1"); "TO1:2", CvTerm.create("TO1:2", "test2", "TO1"); "TO3:1", CvTerm.create("TO3:1", "<missing>", "<missing>"); "TO2:1", CvTerm.create("TO2:1", "test1", "TO2"); "TO2:2", CvTerm.create("TO2:2", "test2", "TO2")
                    ]
                    let expected2 = [
                        ("TO1:1", "TO1:2", set [IsA]); ("TO1:1", "TO3:1", set [Xref]); ("TO1:1", "TO2:1", set [Xref]); ("TO2:1", "TO2:2", set [Custom "has_a"]); ("TO2:1", "TO3:1", set [Xref])
                    ]
                    Expect.sequenceEqual actual1 expected1 "Terms differ"
                    Expect.sequenceEqual actual2 expected2 "Relations differ"
            ]

            testList "ContainsTerm" [
                let testOnto = Ontology().AddTerm(CvTerm.create("TO:0", "test", "TO"))

                testCase "gives correct check: true" <| fun _ ->
                    let actual = testOnto.ContainsTerm("TO:0")
                    Expect.isTrue actual "Returns false but should be true"

                testCase "gives correct check: false" <| fun _ ->
                    let actual = testOnto.ContainsTerm("TO:1")
                    Expect.isFalse actual "Returns true but should be false"
            ]

            testList "GetAllRelations" [
                testCase "returns all relations correctly" <| fun _ ->
                    let actual = ReferenceObjects.testOnto1.GetAllRelations()
                    let expected = [
                        "test:01", "test:02", Set.singleton Xref
                        "test:01", "test:04", Set.singleton IsA
                        "test:02", "test:04", Set.singleton IsA
                        "test:03", "test:02", Set.singleton Xref
                        "test:03", "test:04", Set.singleton IsA
                    ]
                    Expect.sequenceEqual actual expected "Relations differ"
            ]

            testList "TryGetRelations" [
                testCase "gets Some relations" <| fun _ ->
                    let actual = ReferenceObjects.testOnto1.TryGetRelations("test:01", "test:02")
                    Expect.isSome actual "Relations are not there though they should"

                testCase "gets relations correctly" <| fun _ ->
                    let actual = ReferenceObjects.testOnto1.TryGetRelations("test:01", "test:02")
                    let expected = Some <| set [Xref]
                    Expect.equal actual expected "Relations are not equal"

                testCase "gets None when relations are not existing" <| fun _ ->
                    let actual = ReferenceObjects.testOnto1.TryGetRelations("test:02", "test:01")
                    Expect.isNone actual "Relations are there though they shouldn't"
            ]

            testList "HasRelations" [
                testCase "gives correct check: true" <| fun _ ->
                    let actual = ReferenceObjects.testOnto1.HasRelations("test:01", "test:02")
                    Expect.isTrue actual "Returns false but should be true"

                testCase "gives correct check: false" <| fun _ ->
                    let actual = ReferenceObjects.testOnto1.HasRelations("test:02", "test:01")
                    Expect.isFalse actual "Returns true but should be false"
            ]

            testList "HasRelation" [
                testCase "gives correct check: true" <| fun _ ->
                    let actual = ReferenceObjects.testOnto1.HasRelation("test:01", "test:02", Xref)
                    Expect.isTrue actual "Returns false but should be true"

                testCase "gives correct check: false" <| fun _ ->
                    let actual = ReferenceObjects.testOnto1.HasRelation("test:01", "test:02", IsA)
                    Expect.isFalse actual "Returns true but should be false"
            ]

            testList "fromTriplets" [
                testCase "returns correct Ontology" <| fun _ ->
                    let trips = [
                        CvTerm.create("TR:1", "tripletTerm1", "TR"), IsA, CvTerm.create("TR:2", "tripletTerm2", "TR")
                        CvTerm.create("TR:1", "tripletTerm1", "TR"), Xref, CvTerm.create("TR:3", "tripletTerm3", "TR")
                        CvTerm.create("TR:2", "tripletTerm2", "TR"), Custom "has_a", CvTerm.create("TR:3", "tripletTerm3", "TR")
                        CvTerm.create("TR:1", "tripletTerm1", "TR"), Term (CvTerm.create("RO:9999999", "uses", "RO")), CvTerm.create("TR:4", "tripletTerm4", "TR")
                        CvTerm.create("TR:1", "tripletTerm1", "TR"), Custom "optional use", CvTerm.create("TR:4", "tripletTerm4", "TR")
                    ]
                    let actual = Ontology.fromTriplets trips |> FGraph.toSeq
                    let expected = [
                        "TR:1", CvTerm.create("TR:1", "tripletTerm1", "TR"), "TR:2", CvTerm.create("TR:2", "tripletTerm2", "TR"), Set.singleton IsA
                        "TR:1", CvTerm.create("TR:1", "tripletTerm1", "TR"), "TR:3", CvTerm.create("TR:3", "tripletTerm3", "TR"), Set.singleton Xref
                        "TR:1", CvTerm.create("TR:1", "tripletTerm1", "TR"), "TR:4", CvTerm.create("TR:4", "tripletTerm4", "TR"), Set [Term (CvTerm.create("RO:9999999", "uses", "RO")); Custom "optional use"]
                        "TR:2", CvTerm.create("TR:2", "tripletTerm2", "TR"), "TR:3", CvTerm.create("TR:3", "tripletTerm3", "TR"), Set.singleton <| Custom "has_a"
                    ]
                    Expect.sequenceEqual actual expected "Ontologies differ but they shouldn't"
            ]

            testList "toOboOntology" [
                testCase "returns correct OboOntology" <| fun _ ->

                    let oboOntoHeaderTags = OboOntologyHeaderTags.Create("1.4", Ontology = "test")
                    let terms = [
                        OboTerm.Create("test:01", "Frosch", Xrefs = [{Name = "test:02"; Description = ""; Modifiers = ""}], IsA = ["test:04"])
                        OboTerm.Create("test:02", "Kröte", IsA = ["test:04"])
                        OboTerm.Create("test:03", "Quakendes Geschöpf", IsA = ["test:04"], Xrefs = [{Name = "test:02"; Description = ""; Modifiers = ""}])
                        OboTerm.Create("test:04", "Tier")
                    ]

                    let prepare terms =
                        terms
                        |> List.map (fun ot -> ot.Id, ot.Name, ot.IsA, ot.Relationships)

                    let actual = ReferenceObjects.testOnto1.ToOboOntology(oboOntoHeaderTags)
                    let expected = OboOntology.Create(terms, [], oboOntoHeaderTags)
                    Expect.sequenceEqual (prepare actual.Terms) (prepare expected.Terms) "Terms differ"
                    Expect.equal actual.FormatVersion expected.FormatVersion "format-version differs"
                    Expect.equal actual.Ontology expected.Ontology "ontology (i.e., ontology name) differs"
            ]

        ]