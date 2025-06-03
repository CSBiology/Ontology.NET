#I @"C:\Repos\CSBiology\Ontology.NET\src\Ontology.NET\bin\Debug\netstandard2.0\"
#I @"C:\Repos\CSBiology\Ontology.NET\src\Ontology.NET\bin\Release\netstandard2.0\"

#r @"Ontology.NET.dll"
#r @"ControlledVocabulary.dll"

//#r "nuget: OBO.NET"


open Ontology.NET
open ControlledVocabulary
//open OBO.NET
//open System.IO


let synCont = SynonymContext()

SynonymContext.addPair (CvTerm.create("id1","","")) (CvTerm.create("id2","","")) synCont |> ignore
SynonymContext.addPair (CvTerm.create("id1","","")) (CvTerm.create("id3","","")) synCont |> ignore

synCont

type A = {
    B   : int
    C   : int
}

let a = {B = 1; C = 100}
let b = {B = 2; C = 3}

a = a
a > b
a < b

let s = set [a; b]

"A".CompareTo 1

[<CustomComparison; CustomEquality>]
type Lol = {
        Ak  : int
        Ax  : int
    } with
        override this.GetHashCode() =
            hash this.Ak
        override this.Equals o =
            match o with
            | :? Lol as lol -> lol.Ak = this.Ak
            | _ -> false
        interface System.IComparable<Lol> with
            member this.CompareTo lol =
                compare this.Ak lol.Ak
        interface System.IComparable with
            member this.CompareTo o =
                match o with
                | :? Lol as lol -> compare this.Ak lol.Ak
                | _ -> raise (System.ArgumentException("bla"))


let l = {Ak = 1; Ax = 100}
let l2 = {Ak = 2; Ax = 3}

l > l2



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