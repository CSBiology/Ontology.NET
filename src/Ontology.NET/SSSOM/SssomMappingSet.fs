namespace Ontology.NET.SSSOM


open Ontology.NET.SSSOM


/// Represents a mapping set object in the SSSOM model.
type SssomMappingSet =
    {
        /// The identifier of the mapping set object (mapping_set_id).
        MappingSetId            : string
        /// The title of the mapping set (mapping_set_title).
        MappingSetTitle         : string option
        /// The description of the mapping set (mapping_set_description).
        MappingSetDescription   : string option
        /// The version of the mapping set (mapping_set_version).
        MappingSetVersion       : string option
        /// The confidence (how confident it represents the depicted mappings) of the mapping set (mapping_set_confidence).
        MappingSetConfidence    : string option
        /// The source (e.g. URL) of the mapping set (mapping_set_source).
        MappingSetSource        : string option

        /// The date when the mapping set was created (mapping_date).
        MappingDate             : string option
        /// The provider of the mapping set (mapping_provider).
        MappingProvider         : string option

        /// The tool used to generate the mapping set (mapping_tool).
        MappingTool             : string option
        /// The ID of the tool used to generate the mapping set (mapping_tool_id).
        MappingToolId           : string option
        /// The version of the tool used to generate the mapping set (mapping_tool_version).
        MappingToolVersion      : string option

        /// The list of mappings in the mapping set.
        Mappings                : SssomMapping seq

        /// The Curie map used in the mapping set (curie_map).
        CurieMap                : Map<string, string>

        /// The creator's ID of the mapping set (creator).
        CreatorId               : string option
        /// The creator's label (name) of the mapping set (creator_label).
        CreatorLabel            : string option

        /// The license under which the mapping set is provided (license).
        License                 : string option
        /// The publication date of the mapping set (publication_date).
        PublicationDate         : string option
        /// The version of SSSOM used (sssom_version).
        SssomVersion            : string option

        /// Additional metadata fields as per SSSOM specification (subject_match_field).
        SubjectMatchField       : string option
        /// Preprocessing applied to the subject identifiers (subject_preprocessing).
        SubjectPreprocessing    : string option
        /// The source of the subject identifiers (subject_source).
        SubjectSource           : string option
        /// The version of the subject source (subject_source_version).
        SubjectSourceVersion    : string option
        /// The type of the subject entities (subject_type).
        SubjectType             : string option

        /// The predicate type used in the mappings (predicate_type).
        PredicateType           : string option

        /// Additional metadata fields as per SSSOM specification (object_match_field).
        ObjectMatchField        : string option
        /// Preprocessing applied to the object identifiers (object_preprocessing).
        ObjectPreprocessing     : string option
        /// The source of the object identifiers (object_source).
        ObjectSource            : string option
        /// The version of the object source (object_source_version).
        ObjectSourceVersion     : string option
        /// The type of the object entities (object_type).
        ObjectType              : string option

        /// The cardinality scope of the mappings (cardinality_scope).
        CardinalityScope        : string option
        /// The similarity measure used in the mappings (similarity_measure).
        SimilarityMeasure       : string option

        /// The extension definitions used in the mapping set (extension_definitions).
        ExtensionDefinitions    : string option

        /// The curation rule applied to the mapping set (curation_rule).
        CurationRule            : string option
        /// The text description of the curation rule (curation_rule_text).
        CurationRuleText        : string option

        /// The issue tracker URL for the mapping set (issue_tracker).
        IssueTracker            : string option
        /// Additional comments regarding the mapping set (comment).
        Comment                 : string option
        /// Any other metadata fields not covered by the standard SSSOM fields (other).
        Other                   : string option
        /// References to related works or publications (see_also).
        SeeAlso                 : string option
    }

    /// <summary>
    /// Creates an SssomMappingSet from the given parameters.
    /// </summary>
    /// <param name="prefixDeclarations">The prefix declarations used in the mapping set.</param>
    /// <param name="mappings">The list of mappings in the mapping set.</param>
    static member Create(
        mappingSetId,
        curieMap,
        mappings,
        mappingSetTitle,
        mappingSetDescription,
        mappingSetVersion,
        mappingSetConfidence,
        mappingSetSource,
        mappingDate,
        mappingProvider,
        mappingTool,
        mappingToolId,
        mappingToolVersion,
        creatorId,
        creatorLabel,
        license,
        publicationDate,
        sssomVersion,
        subjectMatchField,
        subjectPreprocessing,
        subjectSource,
        subjectSourceVersion,
        subjectType,
        predicateType,
        objectMatchField,
        objectPreprocessing,
        objectSource,
        objectSourceVersion,
        objectType,
        cardinalityScope,
        similarityMeasure,
        extensionDefinitions,
        curationRule,
        curationRuleText,
        issueTracker,
        comment,
        other,
        seeAlso
    ) =
        {
            CurieMap                = curieMap
            Mappings                = mappings
            MappingSetId            = mappingSetId
            MappingSetTitle         = mappingSetTitle
            MappingSetDescription   = mappingSetDescription
            MappingSetVersion       = mappingSetVersion
            MappingSetConfidence    = mappingSetConfidence
            MappingSetSource        = mappingSetSource
            MappingDate             = mappingDate
            MappingProvider         = mappingProvider
            MappingTool             = mappingTool
            MappingToolId           = mappingToolId
            MappingToolVersion      = mappingToolVersion
            CreatorId               = creatorId
            CreatorLabel            = creatorLabel
            License                 = license
            PublicationDate         = publicationDate
            SssomVersion            = sssomVersion
            SubjectMatchField       = subjectMatchField
            SubjectPreprocessing    = subjectPreprocessing
            SubjectSource           = subjectSource
            SubjectSourceVersion    = subjectSourceVersion
            SubjectType             = subjectType
            PredicateType           = predicateType
            ObjectMatchField        = objectMatchField
            ObjectPreprocessing     = objectPreprocessing
            ObjectSource            = objectSource
            ObjectSourceVersion     = objectSourceVersion
            ObjectType              = objectType
            CardinalityScope        = cardinalityScope
            SimilarityMeasure       = similarityMeasure
            ExtensionDefinitions    = extensionDefinitions
            CurationRule            = curationRule
            CurationRuleText        = curationRuleText
            IssueTracker            = issueTracker
            Comment                 = comment
            Other                   = other
            SeeAlso                 = seeAlso
        }

    