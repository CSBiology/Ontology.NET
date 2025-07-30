module CvTermTests


open ControlledVocabulary
open ReferenceObjects


open Expecto


[<Tests>]
let cvTermTests = testList "CvTermTests" [

    testList "CheckForUri" [
        testCase "correct check, actual URL 1" <| fun _ ->
            let check = CvTerm.checkForUri testAccession3
            Expect.isTrue check "Should be URI but seemingly isn't"

        testCase "correct check, actual URL 2" <| fun _ ->
            let check = CvTerm.checkForUri "https://purl.org/TO_00000003"
            Expect.isTrue check "Should be URI but seemingly isn't"

        testCase "correct check, no URL" <| fun _ ->
            let check = CvTerm.checkForUri "purl/123_abc"
            Expect.isFalse check "Should not be URI"
    ]


    testList "UriToTan" [
        testCase "correct TAN returned" <| fun _ ->
            let expected = "TO:00000003"
            let actual = CvTerm.uriToTan testAccession3
            Expect.equal actual expected "TAN should match expected value"
    ]


    testList "RefOfAccession" [
        testCase "correct TSR returned" <| fun _ ->
            let expected = "TO"
            let actual = CvTerm.refOfAccession testAccession1
            Expect.equal actual expected "TSR should match expected value"
    ]


    testList "Create" [
        testCase "correct CvTerm, primary create function overload" <| fun _ ->
            let expected = testTerm1
            let actual = CvTerm.create(testAccession1, testName1, testRef1)
            Expect.equal actual expected "CvTerm should match expected"

        testCase "correct CvTerm, secondary create function overload (only name given)" <| fun _ ->
            let expected = testTerm4
            let actual = CvTerm.create(testName2)
            Expect.equal actual expected "CvTerm should match expected"

        testCase "correct CvTerm, create function with URL as accession" <| fun _ ->
            let expected = testTerm3
            let actual = CvTerm.create(testAccession3, testName2, testRef2)
            Expect.equal actual expected "CvTerm should match expected"
    ]

]