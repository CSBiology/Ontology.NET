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
                    ot.
        )
        let cvts, rels =
            obo.Terms
            |> List.map (
                fun ot -> CvTerm.create(ot.Id, ot.Name, CvTerm.refOfAccession ot.Id)
            )
        let rels =
            obo.Terms