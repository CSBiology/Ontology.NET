namespace Ontology.NET


open ControlledVocabulary
open Ontology.NET.OBO


type Ontology = 
    {
        Terms       : CvTerm seq
        Relations   : (CvTerm * Relation * CvTerm) seq
    }

    static member ofTerms terms =
        {Terms = terms; Relations = Seq.empty}

    static member ofRelations relations =
        {Relations = relations; Terms = Seq.empty}

    static member ofTermsAndRelations terms relations =
        {Terms = terms; Relations = relations}

    static member fromOboOntology (oboOntology : OboOntology) =
        {
            Terms = 
                Seq.map OboTerm.toCvTerm oboOntology.Terms
            Relations = 
                oboOntology.Terms
                |> Seq.collect Relation.fromOboTerm
        }

