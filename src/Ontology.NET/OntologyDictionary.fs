namespace Ontology.NET


open ControlledVocabulary
open Ontology.NET.OBO

open System.Collections.Generic


type OntologyDictionary() =

    inherit Dictionary<CvTerm,OntologyRelation Set>()

    ///
    static member fromOboOntology (oboOnto : OboOntology) =

        let onto = Ontology()

        let cvts = Seq.map OboTerm.toCvTerm oboOnto.Terms

        let rels =
            oboOnto.Terms
            |> List.iter (
                fun oboTerm ->
                    let cvt = OboTerm.toCvTerm oboTerm
                    let isAs =
                        oboTerm.IsA
                        |> Seq.map (
                            fun isATermId ->
                                cvts
                                |> Seq.tryFind (
                                    fun searchedTerm ->
                                        searchedTerm.Accession = isATermId
                                )
                                |> Option.defaultValue (CvTerm.create(accession = isATermId, name = "<missing>", ref = "<missing>"))
                        )
                        |> Seq.map (OntologyRelation.create IsA)
                    let xrefs =
                        oboTerm.Xrefs
                        |> Seq.map (
                            fun xref ->
                                cvts
                                |> Seq.tryFind (
                                    fun searchedTerm ->
                                        searchedTerm.Accession = xref
                                )
                                xref.Name
                        )


            )

        onto