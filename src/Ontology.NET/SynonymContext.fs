namespace Ontology.NET


open ControlledVocabulary

open System.Collections.Generic


module SynonymContext =

    let ofTermSynonymPairs (synonyms : (CvTerm * CvTerm) seq) =
        let dict = Dictionary<CvTerm,CvTerm Set>()
        synonyms
        |> Seq.iter (
            fun (t1,t2) ->
                if dict.ContainsKey t1 then
                    let oldVal = dict[t1]
                    let newVal = Set.add t2 oldVal
                    dict[t1] <- newVal
                else
                    dict.Add(t1, set [t2])
        )
        dict