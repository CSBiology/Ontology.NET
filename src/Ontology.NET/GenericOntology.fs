namespace Ontology.NET


open ControlledVocabulary

open System.Collections.Generic


type GenericOntology() =

    inherit Dictionary<CvTerm,CvTerm Set>()

    ///
    static member fromOboOntology obo =
        