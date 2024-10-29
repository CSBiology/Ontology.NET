namespace Ontology.NET


open ControlledVocabulary

open System.Collections.Generic


type SynonymContext() =

    inherit Dictionary<CvTerm,CvTerm Set>()

    //new()     // TO DO


module SynonymContext =

    let addPair sourceTerm targetTerm (synCont : SynonymContext) =

        if synCont.ContainsKey sourceTerm then
            let oldVal = synCont[sourceTerm]
            let newVal = Set.add targetTerm oldVal
            synCont[sourceTerm] <- newVal
        else
            synCont.Add(sourceTerm, set [targetTerm])

        synCont


    let ofTermSynonymPairs (synonyms : (CvTerm * CvTerm) seq) =
        let synCont = SynonymContext()
        synonyms
        |> Seq.iter (
            fun (t1,t2) -> addPair t1 t2 synCont |> ignore
        )

        synCont

