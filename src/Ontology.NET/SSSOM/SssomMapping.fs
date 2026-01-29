namespace Ontology.NET.SSSOM


////// Represents a single mapping in the SSSOM model.
type SssomMapping =
    {
        /// The subject identifier of the mapping.
        SubjectID               : string

        /// The subject label of the mapping.
        SubjectLabel            : string option

        /// The predicate identifier of the mapping.
        PredicateID             : string

        /// The object identifier of the mapping.
        ObjectID                : string

        /// The object label of the mapping.
        ObjectLabel             : string option

        /// The mapping type.
        MappingJustification    : string option

        /// The subject source of the mapping.
        SubjectSource           : string option

        /// The object source of the mapping.
        ObjectSource            : string option

        /// A comment regarding the mapping.
        Comment                 : string option
    }

    /// <summary>
    /// Creates an SSSOMMapping from the given parameters.
    /// </summary>
    /// <param name="subjectID">The subject identifier of the mapping.</param>
    /// <param name="subjectLabel">The subject label of the mapping.</param>
    /// <param name="predicateID">The predicate identifier of the mapping.</param>
    /// <param name="objectID">The object identifier of the mapping.</param>
    /// <param name="objectLabel">The object label of the mapping.</param>
    /// <param name="mappingJustification">The mapping type.</param>
    /// <param name="subjectSource">The subject source of the mapping.</param>
    /// <param name="objectSource">The object source of the mapping.</param>
    /// <param name="comment">A comment regarding the mapping.</param>
    static member Create(
        subjectID,
        subjectLabel,
        predicateID,
        objectID,
        objectLabel,
        mappingJustification,
        subjectSource,
        objectSource,
        comment
    ) =
        {
            SubjectID               = subjectID
            SubjectLabel            = subjectLabel
            PredicateID             = predicateID
            ObjectID                = objectID
            ObjectLabel             = objectLabel
            MappingJustification    = mappingJustification
            SubjectSource           = subjectSource
            ObjectSource            = objectSource
            Comment                 = comment
        }