namespace Ontology.NET.OBO


/// Model of raw OboEntries, divided into Terms (as OboTerms) and Typedefs (as OboTypedefs).
type OboEntry =
    | Term of OboTerm
    | TypeDef of OboTypedef