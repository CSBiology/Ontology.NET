namespace Ontology.NET


open ControlledVocabulary
open Ontology.NET.OBO

open System.Collections.Generic


type GenericOntology() =

    inherit Dictionary<CvTerm,(string*CvTerm) Set>()

    ///
    static member fromOboOntology (obo : OboOntology) =
        let newGO = GenericOntology()
        obo.Terms
        |> List.iter (
            fun ot ->
                let rels =
                    let xrefCvTs =
                        ot.Xrefs
                        |> List.map
        )
        let cvts, rels =
            obo.Terms
            |> List.iter (
                fun ot -> 
                    let term = CvTerm.create(ot.Id, ot.Name, CvTerm.refOfAccession ot.Id)
                    let rels =
                        let xrefCvTs =
                            ot.Xrefs
                            |> List.map (
                                fun xref -> CvTerm.create(xref.Name, "<missing>", CvTerm.refOfAccession xref.Name)
                            )
                        let isACvTs =
                            ot.IsA
                            |> List.map (
                                fun isA -> 
                                    obo.Terms
                                    |> List.tryFind (
                                        fun ot2 ->
                                            ot2.Id = isA
                                    )
                                    |> Option.defaultValue (CvTerm.create())
                            )
                        
                    newGO.Add(term, rels)
            )
        let rels =
            obo.Terms