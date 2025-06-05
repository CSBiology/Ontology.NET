namespace Ontology.NET


open ControlledVocabulary

open System.Collections.Generic


type DBContext() =

    inherit Dictionary<CvTerm,CvTerm Set>()

    /// 
    static member fromOntology onto =
        