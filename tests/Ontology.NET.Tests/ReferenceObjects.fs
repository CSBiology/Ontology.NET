namespace Ontology.NET.Tests


open Graphoscope
open ControlledVocabulary

open Ontology.NET


module ReferenceObjects =

    let testOnto1 =
        let onto = Ontology()

        [
            CvTerm.create("test:01", "Frosch", "test")
            CvTerm.create("test:02", "Kröte", "test")
            CvTerm.create("test:03", "Quakendes Geschöpf", "test")
            CvTerm.create("test:04", "Tier", "test")
        ]
        |> List.iter (fun cvt -> FGraph.addNode cvt.Accession cvt onto |> ignore)

        [
            "test:01", "test:02", Set.singleton Xref
            "test:03", "test:02", Set.singleton Xref
            "test:01", "test:04", Set.singleton IsA
            "test:02", "test:04", Set.singleton IsA
            "test:03", "test:04", Set.singleton IsA
        ]
        |> List.iter (fun (st,tt,e) -> FGraph.addEdge st tt e onto |> ignore)

        onto