namespace Ontology.NET.OBO


open ARCtrl


module OboTerm =

    /// Translates an OBO term into an ISADotNet OntologyAnnotation.
    let toOntologyAnnotation (term : OboTerm) =
        OntologyAnnotation(term.Name,tan = term.Id)

    /// Translates an ISADotNet OntologyAnnotation into an OBO term.
    let ofOntologyAnnotation (term : OntologyAnnotation) =
        OboTerm.Create(term.TermAccessionShort,term.NameText)