namespace Ontology.NET


open ControlledVocabulary


/// Model for a generic type of relation in an ontology. The relation can be of type CvTerm when defined (recommended) or string when not defined.
type RelationType =
    | Term of CvTerm
    | Custom of string


/// Model for a generic ontological relation. Consists of fields RelationType that inhabits the type of the relation, and Target for the targeted CvTerm of the relation.
type OntologyRelation = {
    RelationType    : RelationType
    Target          : CvTerm
} with

    /// Creates an OntologyRelation with the given RelationType and target CvTerm.
    static member create relationType targetTerm = {
        RelationType    = relationType
        Target          = targetTerm
    }


/// Collection of the most important relations from Relation Ontology (RO).
module Relations =

    let isA =
        CvTerm.create("", "is a", "")

    let hasA =
        CvTerm.create("", "has a", "")

    let partOf =
        CvTerm.create("BFO:0000050", "part of", "RO")   // BEWARE! It's BFO:00000050 but it's located in the RO. Wanna know why? Read here: https://github.com/BFO-ontology/BFO/issues/218

    let xref =
        CvTerm.create("", "xref", "")