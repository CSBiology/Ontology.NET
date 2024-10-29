#r @"C:\Repos\CSBiology\Ontology.NET\src\Ontology.NET\bin\Debug\netstandard2.0\Ontology.NET.dll"
#r @"C:\Repos\CSBiology\Ontology.NET\src\Ontology.NET\bin\Debug\netstandard2.0\ControlledVocabulary.dll"


open Ontology.NET
open ControlledVocabulary


let synCont = SynonymContext()

SynonymContext.addPair (CvTerm.create("id1","","")) (CvTerm.create("id2","","")) synCont |> ignore
SynonymContext.addPair (CvTerm.create("id1","","")) (CvTerm.create("id3","","")) synCont |> ignore

synCont