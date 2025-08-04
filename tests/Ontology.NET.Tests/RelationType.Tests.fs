namespace Ontology.NET.Tests


open Expecto
open Graphoscope
open FSharpAux

open ControlledVocabulary
open Ontology.NET


module RelationTypeTests =

    [<Tests>]
    let relationTypeTests =
        testList "RelationType" [

            let dummyTerm = CvTerm.create("DT:1", "dummyTerm", "DT")

            testList "fromString" [
                testCase "creates IsA from 'is_a'" <| fun _ ->
                    let actual = RelationType.fromString "is_a"
                    Expect.equal actual IsA "Expected RelationType.IsA"

                testCase "creates IsA from 'is a'" <| fun _ ->
                    let actual = RelationType.fromString "is a"
                    Expect.equal actual IsA "Expected RelationType.IsA"

                testCase "creates IsA from 'ISA'" <| fun _ ->
                    let actual = RelationType.fromString "ISA"
                    Expect.equal actual IsA "Expected RelationType.IsA"

                testCase "creates Xref from 'xref'" <| fun _ ->
                    let actual = RelationType.fromString "xref"
                    Expect.equal actual Xref "Expected RelationType.Xref"

                testCase "creates Xref from 'x_ref'" <| fun _ ->
                    let actual = RelationType.fromString "x_ref"
                    Expect.equal actual Xref "Expected RelationType.Xref"

                testCase "creates Xref from 'x ref'" <| fun _ ->
                    let actual = RelationType.fromString "x ref"
                    Expect.equal actual Xref "Expected RelationType.Xref"

                testCase "creates Custom from unknown string" <| fun _ ->
                    let input = "related_to"
                    let actual = RelationType.fromString input
                    let expected = Custom input
                    Expect.equal actual expected "Expected Custom relation type"
            ]

            testList "ToString instance method" [
                testCase "returns 'is_a' for IsA" <| fun _ ->
                    let actual = IsA.ToString()
                    Expect.equal actual "is_a" "ToString failed for IsA"

                testCase "returns 'xref' for Xref" <| fun _ ->
                    let actual = Xref.ToString()
                    Expect.equal actual "xref" "ToString failed for Xref"

                testCase "returns original string for Custom" <| fun _ ->
                    let actual = (Custom "related_to").ToString()
                    Expect.equal actual "related_to" "ToString failed for Custom"

                testCase "returns CvTerm name for Term" <| fun _ ->
                    let actual = (Term dummyTerm).ToString()
                    Expect.equal actual "dummyTerm" "ToString failed for Term"
            ]

            testList "toString static method" [
                testCase "returns 'is_a' for IsA" <| fun _ ->
                    let actual = RelationType.toString IsA
                    Expect.equal actual "is_a" "toString failed for IsA"

                testCase "returns 'xref' for Xref" <| fun _ ->
                    let actual = RelationType.toString Xref
                    Expect.equal actual "xref" "toString failed for Xref"

                testCase "returns string for Custom" <| fun _ ->
                    let actual = RelationType.toString (Custom "foobar")
                    Expect.equal actual "foobar" "toString failed for Custom"

                testCase "returns term name for Term" <| fun _ ->
                    let actual = RelationType.toString (Term dummyTerm)
                    Expect.equal actual "dummyTerm" "toString failed for Term"
            ]

        ]