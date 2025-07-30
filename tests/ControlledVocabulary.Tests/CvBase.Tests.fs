module CvBaseTests


open Expecto

open ControlledVocabulary


let assayTerm = CvTerm.create(accession = "ARCO:1234", name = "Assay", ref = "ARCO")

[<Tests>]
let cvBaseTests =
    testList "CvBaseTests" [

        testCase "ICvBase can be cast to CvParam using generic tryAs" <| fun _ ->
            let v = CvParam(assayTerm, ParamValue.Value 5) :> ICvBase
            let result = CvBase.tryAs<CvParam> v
            Expect.isSome result "Casting ICvBase to CvParam should succeed (generic)"

            let result2 = result.Value |> Param.getValueAsInt
            Expect.equal result2 5 "Value should be 5"

        testCase "ICvBase can be cast to CvParam using tryCvParam" <| fun _ ->
            let v = CvParam(assayTerm, ParamValue.Value 5) :> ICvBase
            let result = CvBase.tryCvParam v
            Expect.isSome result "Casting ICvBase to CvParam should succeed (specialized)"

            let result2 = result.Value |> Param.getValueAsInt
            Expect.equal result2 5 "Value should be 5"

        testCase "ICvBase can be cast to UserParam using generic tryAs" <| fun _ ->
            let v = UserParam("MyParam", ParamValue.Value 5) :> ICvBase
            let result = CvBase.tryAs<UserParam> v
            Expect.isSome result "Casting ICvBase to UserParam should succeed (generic)"

            let result2 = result.Value |> Param.getValueAsInt
            Expect.equal result2 5 "Value should be 5"

        testCase "ICvBase can be cast to UserParam using tryUserParam" <| fun _ ->
            let v = UserParam("MyParam", ParamValue.Value 5) :> ICvBase
            let result = CvBase.tryUserParam v
            Expect.isSome result "Casting ICvBase to UserParam should succeed (specialized)"

            let result2 = result.Value |> Param.getValueAsInt
            Expect.equal result2 5 "Value should be 5"

        testCase "ICvBase can be cast to CvContainer using generic tryAs" <| fun _ ->
            let v = CvContainer(assayTerm) :> ICvBase
            let result = CvBase.tryAs<CvContainer> v
            Expect.isSome result "Casting ICvBase to CvContainer should succeed (generic)"

        testCase "ICvBase can be cast to CvContainer using tryCvContainer" <| fun _ ->
            let v = CvContainer(assayTerm) :> ICvBase
            let result = CvContainer.tryCvContainer v
            Expect.isSome result "Casting ICvBase to CvContainer should succeed (specialized)"
    ]