module CvParamTests


open System.Collections.Generic

open Expecto

open ControlledVocabulary
open ReferenceObjects


[<Tests>]
let cvParamTests = testList "CvParamTests" [

    testList "InstanceMemberTests" [
        testCase "Accession" <| fun _ ->
            let expected = [testAccession1; testAccession1; testAccession2]
            let actual = testCvParams |> List.map (fun x -> x.Accession)
            Expect.equal actual expected "Accessions should match"

        testCase "Name" <| fun _ ->
            let expected = [testName1; testName1; testName2]
            let actual = testCvParams |> List.map (fun x -> x.Name)
            Expect.equal actual expected "Names should match"

        testCase "RefUri" <| fun _ ->
            let expected = [testRef1; testRef1; testRef2]
            let actual = testCvParams |> List.map (fun x -> x.RefUri)
            Expect.equal actual expected "RefUris should match"

        testCase "Value" <| fun _ ->
            let expected = [
                ParamValue.Value 5
                ParamValue.CvValue testTerm2
                ParamValue.WithCvUnitAccession (5, testTerm1)
            ]
            let actual = testCvParams |> List.map (fun x -> x.Value)
            Expect.equal actual expected "Values should match"
    ]


    //let testCvp1 = CvParam("test", "test", "test", ParamValue.Value "test", Generic.Dictionary<string,IParam>() |> fun d -> d.Add("test", testCvParams.Head); d) 
    //let testCvp2 = CvParam("test", "test", "test", ParamValue.Value "test", Generic.Dictionary<string,IParam>() |> fun d -> d.Add("test", testCvParams.Head); d) 
    //let testAttr1 = CvAttributeCollection(Generic.Dictionary<string,IParam>() |> fun d -> d.Add("test", testCvp1); d)
    //let testCvp3 = CvParam("test", "test", "test", ParamValue.Value "test")
    //let testCvp4 = CvParam("test", "test", "test", ParamValue.Value "test")
    //let testCvp5 = CvParam("test", "test", "test", ParamValue.Value "test", Generic.Dictionary<string,IParam>() |> fun d -> d.Add("test", testCvp1); d)
    //let testCvp6 = CvParam("test", "test", "test", ParamValue.Value "test", Generic.Dictionary<string,IParam>() |> fun d -> d.Add("test", testCvp2); d)
    //let testCvp7 = CvParam("", "", "", ParamValue.Value "", Generic.Dictionary<string,IParam>() |> fun d -> d.Add("test", testCvParams[1]); d)
    //let testCvp8 = CvParam("", "", "", ParamValue.Value "", Generic.Dictionary<string,IParam>() |> fun d -> d.Add("test", testCvp7); d)

    let mkDict (k, v) =
        let d = Dictionary<string, IParam>()
        d.Add(k, v)
        d

    let testCvp1 = CvParam("test", "test", "test", ParamValue.Value "test", mkDict("test", testCvParams.Head))
    let testCvp2 = CvParam("test", "test", "test", ParamValue.Value "test", mkDict("test", testCvParams.Head))
    let testCvp3 = CvParam("test", "test", "test", ParamValue.Value "test")
    let testCvp4 = CvParam("test", "test", "test", ParamValue.Value "test")
    let testCvp5 = CvParam("test", "test", "test", ParamValue.Value "test", mkDict("test", testCvp1))
    let testCvp6 = CvParam("test", "test", "test", ParamValue.Value "test", mkDict("test", testCvp2))
    let testCvp7 = CvParam("", "", "", ParamValue.Value "", mkDict("test", testCvParams[1]))
    let testCvp8 = CvParam("", "", "", ParamValue.Value "", mkDict("test", testCvp7))

    testList "Equals" [
        testCase "identical CvParams, empty Attributes" <| fun _ ->
            Expect.isTrue (testCvp3 = testCvp4) "Should be equal"

        testCase "identical CvParams, filled Attributes with filled Attributes" <| fun _ ->
            Expect.isTrue (testCvp5 = testCvp6) "Should be equal"

        testCase "identical CvParams, filled Attributes with same Attributes" <| fun _ ->
            Expect.isTrue (testCvp1 = testCvp2) "Should be equal"

        testCase "different CvParams, empty Attributes" <| fun _ ->
            Expect.isFalse (testCvp3 = testCvParams.Head) "Should not be equal"

        testCase "different CvParams, filled vs empty Attributes" <| fun _ ->
            Expect.isFalse (testCvp1 = testCvp7) "Should not be equal"

        testCase "different CvParams, different Attributes" <| fun _ ->
            Expect.isFalse (testCvp5 = testCvp8) "Should not be equal"
    ]

    testList "StaticMemberTests" [
        testCase "getParamValue" <| fun _ ->
            let expected = [
                ParamValue.Value 5
                ParamValue.CvValue testTerm2
                ParamValue.WithCvUnitAccession (5, testTerm1)
            ]
            let actual = testCvParams |> List.map CvParam.getParamValue
            Expect.equal actual expected "ParamValues should match"

        testCase "getValue" <| fun _ ->
            let expected : System.IConvertible list = [5; testTerm2.Name; 5]
            let actual = testCvParams |> List.map CvParam.getValue
            Expect.sequenceEqual actual expected "Values should match"

        testCase "getValueAsString" <| fun _ ->
            let expected = ["5"; testTerm2.Name; "5"]
            let actual = testCvParams |> List.map CvParam.getValueAsString
            Expect.equal actual expected "String values should match"

        testCase "getValueAsInt" <| fun _ ->
            let expected = [5; 5; 5]
            let actual = testCvParams |> List.map CvParam.getValueAsInt
            Expect.equal actual expected "Ints should match"

        testCase "getValueAsTerm" <| fun _ ->
            let expected = [
                CvTerm.create(name = "5")
                testTerm2
                CvTerm.create(name = "5")
            ]
            let actual = testCvParams |> List.map CvParam.getValueAsTerm
            Expect.equal actual expected "Terms should match"

        testCase "tryGetValueAccession" <| fun _ ->
            let expected = [None; Some testAccession2; None]
            let actual = testCvParams |> List.map CvParam.tryGetValueAccession
            Expect.equal actual expected "Accessions should match"

        testCase "tryGetValueRef" <| fun _ ->
            let expected = [None; Some testRef2; None]
            let actual = testCvParams |> List.map CvParam.tryGetValueRef
            Expect.equal actual expected "Refs should match"

        testCase "tryGetCvUnit" <| fun _ ->
            let expected = [None; None; Some testTerm1]
            let actual = testCvParams |> List.map CvParam.tryGetCvUnit
            Expect.equal actual expected "CvUnits should match"

        testCase "tryGetCvUnitValue" <| fun _ ->
            let expected : (System.IConvertible option) list = [None; None; Some 5]
            let actual = testCvParams |> List.map CvParam.tryGetCvUnitValue
            Expect.equal actual expected "CvUnit values should match"

        testCase "tryGetCvUnitTermName" <| fun _ ->
            let expected = [None; None; Some testName1]
            let actual = testCvParams |> List.map CvParam.tryGetCvUnitTermName
            Expect.equal actual expected "Term names should match"

        testCase "tryGetCvUnitTermAccession" <| fun _ ->
            let expected = [None; None; Some testAccession1]
            let actual = testCvParams |> List.map CvParam.tryGetCvUnitTermAccession
            Expect.equal actual expected "Term accessions should match"

        testCase "tryGetCvUnitTermRef" <| fun _ ->
            let expected = [None; None; Some testRef2]
            let actual = testCvParams |> List.map CvParam.tryGetCvUnitTermRef
            Expect.equal actual expected "Term refs should match"

        testCase "mapValue" <| fun _ ->
            let expected = [ParamValue.Value 1; ParamValue.Value 1; ParamValue.Value 1]
            let actual = testCvParams |> List.map (CvParam.mapValue (fun _ -> ParamValue.Value 1) >> CvParam.getParamValue)
            Expect.equal actual expected "Mapped values should match"

        testCase "tryMapValue" <| fun _ ->
            let expected = [Some (ParamValue.Value 1); Some (ParamValue.Value 1); Some (ParamValue.Value 1)]
            let actual = testCvParams |> List.map (CvParam.tryMapValue (fun _ -> Some (ParamValue.Value 1)) >> Option.map CvParam.getParamValue)
            Expect.equal actual expected "TryMapped values should match"

        testCase "tryAddName" <| fun _ ->
            let expected = [Some testName1; None; None]
            let actual = testCvParams |> List.map (CvParam.tryAddName testName1 >> Option.map CvParam.getCvName)
            Expect.equal actual expected "TryAddName results should match"

        testCase "tryAddAccession" <| fun _ ->
            let expected = [None; None; None]
            let actual = testCvParams |> List.map (CvParam.tryAddAccession testAccession1 >> Option.map CvParam.getCvAccession)
            Expect.equal actual expected "TryAddAccession results should match"

        testCase "tryAddReference" <| fun _ ->
            let expected = [None; None; None]
            let actual = testCvParams |> List.map (CvParam.tryAddReference testRef1 >> Option.map CvParam.getCvRef)
            Expect.equal actual expected "TryAddReference results should match"

        testCase "tryAddUnit" <| fun _ ->
            let expected = [Some (ParamValue.WithCvUnitAccession (5, testTerm1)); None; None]
            let actual = testCvParams |> List.map (CvParam.tryAddUnit testTerm1 >> Option.map CvParam.getParamValue)
            Expect.equal actual expected "TryAddUnit results should match"

        testCase "getCvAccession" <| fun _ ->
            let expected = [testAccession1; testAccession1; testAccession2]
            let actual = testCvParams |> List.map CvParam.getCvAccession
            Expect.equal actual expected "CvAccessions should match"

        testCase "getCvName" <| fun _ ->
            let expected = [testName1; testName1; testName2]
            let actual = testCvParams |> List.map CvParam.getCvName
            Expect.equal actual expected "CvNames should match"

        testCase "getCvRef" <| fun _ ->
            let expected = [testRef1; testRef1; testRef2]
            let actual = testCvParams |> List.map CvParam.getCvRef
            Expect.equal actual expected "CvRefs should match"

        testCase "getTerm" <| fun _ ->
            let expected = [testTerm1; testTerm1; testTerm2]
            let actual = testCvParams |> List.map CvParam.getTerm
            Expect.equal actual expected "Terms should match"

        testCase "equalsTerm" <| fun _ ->
            List.zip [testTerm1; testTerm1; testTerm2] testCvParams
            |> List.iter (fun (t, cvp) ->
                Expect.isTrue (CvParam.equalsTerm t cvp) $"Should match for term {t.Name}"
            )

        testCase "equals" <| fun _ ->
            List.zip [
                CvParam(testTerm1, ParamValue.Value 5)
                CvParam(testTerm1, ParamValue.CvValue testTerm2)
                CvParam(testTerm2, ParamValue.WithCvUnitAccession (5, testTerm1))
            ] testCvParams
            |> List.iter (fun (a,b) -> Expect.isTrue (CvParam.equals a b) "Should be equal")

        testCase "equalsName" <| fun _ ->
            List.zip [
                CvParam(testTerm1, ParamValue.Value 5)
                CvParam(testTerm1, ParamValue.CvValue testTerm2)
                CvParam(testTerm2, ParamValue.WithCvUnitAccession (5, testTerm1))
            ] testCvParams
            |> List.iter (fun (a,b) -> Expect.isTrue (CvParam.equalsName a b) "Should be equal")
    ]

]