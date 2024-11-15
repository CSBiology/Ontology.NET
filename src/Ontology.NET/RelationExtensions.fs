namespace Ontology.NET


open ControlledVocabulary
open Ontology.NET.OBO


module OboTerm =

    /// Creates ontology relations from a given OboTerm.
    let toRelations (oboTerm : OboTerm) =
        Relationship.fromOboTerm oboTerm

    /// Creates ontology relationships from a given OboTerm.
    let toRelationships (oboTerm : OboTerm) =
        Relation.fromOboTerm oboTerm