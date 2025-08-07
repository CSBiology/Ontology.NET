namespace Ontology.NET


open ControlledVocabulary


/// Model for a generic type of relation in an ontology. The relation can be of type CvTerm when defined (recommended) or string when not defined.
type RelationType =
    | IsA
    | Xref
    | Term of CvTerm
    | Custom of string

    with
        /// <summary>
        /// Creates a RelationType from a given string.
        /// </summary>
        /// <param name="str">The string that is used to create a RelationType.</param>
        static member fromString (str : string) =
            match str.ToLower() with
            | "is_a"
            | "is a"
            | "isa" -> IsA
            | "xref"
            | "x_ref"
            | "x ref" -> Xref
            | _ -> Custom str

        /// <summary>
        /// Returns the RelationType as a string.
        /// </summary>
        override this.ToString() : string = 
            match this with
            | IsA       -> "is_a"
            | Xref      -> "xref"
            | Custom r  -> r
            | Term cvt  -> cvt.Name

        /// <summary>
        /// Returns a RelationType as a string.
        /// </summary>
        static member toString (rt : RelationType) =
            rt.ToString()


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

    let partOf =
        CvTerm.create("BFO:0000050", "part of", "RO")   // BEWARE! It's BFO:00000050 but it's located in the RO. Wanna know why? Read here: https://github.com/BFO-ontology/BFO/issues/218

    let hasPart =
        CvTerm.create("BFO:0000051", "has part", "RO")  // same as with part_of.