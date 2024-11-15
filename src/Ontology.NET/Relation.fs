namespace Ontology.NET


open ControlledVocabulary
open Ontology.NET.OBO


type Relationship =
    | Synonym
    | Xref
    | Is_a
    | Part_of
    | Custom of string

    with
        ///
        static member createFromString str =
            match str with
            | "is_a"    -> Is_a
            | "part_of" -> Part_of
            | "synonym" -> Synonym
            | "xref"    -> Xref
            | _         -> Custom str

        ///
        static member fromOboTerm (oboTerm : OboTerm) =

            let sourceTerm = OboTerm.toCvTerm oboTerm

            let oboRelationships = 
                oboTerm.Relationships
                |> Seq.map (
                    fun r ->
                        let rString, targetTermString = OboTerm.deconstructRelationship r
                        let targetTerm = CvTerm.create(name = "<missing>", accession = targetTermString, ref = "<missing>")
                        let relationship = Relationship.createFromString rString
                        sourceTerm, relationship, targetTerm
                )

            let synonyms =
                oboTerm.Synonyms
                |> Seq.collect (
                    fun s ->
                        let targetTerms = 
                            s.DBXrefs 
                            |> Seq.map DBXref.toCvTerm
                        targetTerms
                        |> Seq.map (fun tt -> sourceTerm, Synonym, tt)
                )

            let isAs =
                oboTerm.IsA
                |> Seq.map (
                    fun i ->
                        let targetTerm = CvTerm.create(name = "<missing>", accession= i, ref = "<missing>")
                        sourceTerm, Is_a, targetTerm
                )

            let xRefs =
                let targetTerms =
                    oboTerm.Xrefs
                    |> Seq.map DBXref.toCvTerm
                targetTerms
                |> Seq.map (fun tt -> sourceTerm, Xref, tt)

            Seq.concat [oboRelationships; synonyms; isAs; xRefs]


type Relation =
    | Single of Relationship
    | Composite of Relationship

    with
        static member fromOboTerm (oboTerm: OboTerm) =
            Relationship.fromOboTerm oboTerm
            |> Seq.map (fun (st,r,tt) -> st, Single r, tt)