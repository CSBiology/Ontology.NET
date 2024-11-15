#r @"C:\Repos\CSBiology\Ontology.NET\src\Ontology.NET\bin\Debug\netstandard2.0\Ontology.NET.dll"
#r @"C:\Repos\CSBiology\Ontology.NET\src\Ontology.NET\bin\Debug\netstandard2.0\ControlledVocabulary.dll"

//#r "nuget: OBO.NET"
#r "nuget: ARCtrl.Core"
#r "nuget: FSharpAux.Core"


open Ontology.NET
open Ontology.NET.OBO
open ControlledVocabulary

open System.IO


let dbpo = OboOntology.fromFile true @"C:\Users\revil\Downloads\dpbo.obo"

let dbpo_onto = Ontology.fromOboOntology dbpo

dbpo.Remarks
dbpo_onto.Terms |> List.ofSeq
dbpo_onto.Relations |> List.ofSeq

let testOnto = OboOntology.fromFile true @"C:\Repos\CSBiology\Ontology.NET\tests\Ontology.NET.Tests\References\HeaderXRefUrl.obo"
let testOnto_onto = Ontology.fromOboOntology dbpo

let loadedOntos = testOnto.LoadAllOntologies()
let headLoadedOnto = Seq.head loadedOntos
fst headLoadedOnto
let headLoadedOntoContent = snd headLoadedOnto

headLoadedOntoContent.Terms.Length


//let pattern = System.Text.RegularExpressions.Regex @"^(?<Name>[A-Za-z]+)_URL:\s(?<URL>https?:\/\/[^\s]+)$"
//(pattern.Match @"CL_URL: https://raw.githubusercontent.com/obophenotype/cell-ontology/refs/heads/master/cl.obo").Groups["Name"].Value
//(pattern.Match @"CL_URL: https://raw.githubusercontent.com/obophenotype/cell-ontology/refs/heads/master/cl.obo").Groups["URL"].Value

// ----

let synCont = SynonymContext()

SynonymContext.addPair (CvTerm.create("id1","","")) (CvTerm.create("id2","","")) synCont |> ignore
SynonymContext.addPair (CvTerm.create("id1","","")) (CvTerm.create("id3","","")) synCont |> ignore

synCont

// ----

//let obo1 = OBO.NET.OboOntology.fromFile true (Path.Combine(__SOURCE_DIRECTORY__, "tests", "fixtures", "testOboFile1.obo"))
//let obo2 = OBO.NET.OboOntology.fromFile true (Path.Combine(__SOURCE_DIRECTORY__, "tests", "fixtures", "testOboFile2.obo"))

//let synsOfObo1 = 
//    obo1.Terms 
//    |> Seq.map (
//        fun o -> 
//            CvTerm.create(o.Id, o.Name, ""),
//            o.Xrefs 
//            |> Seq.choose (
//                fun xr -> 
//                    obo2.Terms
//                    |> List.tryFind (fun o2 -> o2.Id = xr.Name)
//                    |> Option.map (fun t -> CvTerm.create(t.Id, t.Name, ""))
//            )
//    )

//synsOfObo1 |> Seq.map (fun (a,b) -> a, List.ofSeq b) |> List.ofSeq

//let synCont2 = SynonymContext()

//synsOfObo1
//|> Seq.iter (fun (source, targets) -> SynonymContext.addSynonymsOfTerm source targets synCont2 |> ignore)

//synCont2