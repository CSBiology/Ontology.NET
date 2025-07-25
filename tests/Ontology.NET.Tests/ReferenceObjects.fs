namespace Ontology.NET.Tests


open Graphoscope
open ControlledVocabulary

open Ontology.NET


module ReferenceObjects =

    let testOnto1 =
        let onto = Ontology()

        [
            CvTerm.create("test:01", "Frosch", "test")
            CvTerm.create("test:02", "Kröte", "test")
            CvTerm.create("test:03", "Quakendes Geschöpf", "test")
            CvTerm.create("test:04", "Tier", "test")
        ]
        |> List.iter (fun cvt -> FGraph.addNode cvt.Accession cvt onto |> ignore)

        [
            "test:01", "test:02", Set.singleton Xref
            "test:03", "test:02", Set.singleton Xref
            "test:01", "test:04", Set.singleton IsA
            "test:02", "test:04", Set.singleton IsA
            "test:03", "test:04", Set.singleton IsA
        ]
        |> List.iter (fun (st,tt,e) -> FGraph.addEdge st tt e onto |> ignore)

        onto

    // Mermaidchart of this ontology:
    //flowchart TD
    //  Hund
    //  Carnivora
    //  Raubtiere
    //  R["Räuber"]
    //  Eutheria
    //  HSt["Höhere Säugetiere"]
    //  HS["Höhere Säuger"]
    //  Lsch("Lateinisch")
    //  D("Deutsch")
    //  L("Latein")
    //  Laurasiatheria

    //  Hund -->|is_a| Carnivora
    //  Carnivora -->|xref| Raubtiere
    //  Carnivora -->|is_a| Laurasiatheria
    //  Raubtiere -->|xref| R
    //  R -->|is_a| Eutheria
    //  HSt -->|xref| HS
    //  Eutheria -->|xref| HS
    //  Eutheria -->|Sprache| Lsch
    //  Carnivora -->|Sprache| Lsch
    //  Hund -->|Sprache| D
    //  Laurasiatheria -->|darunterliegend| Raubtiere
    //  Lsch -->|xref| L
    //  Lsch -->|ist nicht| D
    let testOnto2 =
        let onto = Ontology()

        FGraph.addElement       "Hund" (CvTerm.create "")               "Carnivora" (CvTerm.create "")      (Set.singleton IsA) onto
        |> FGraph.addElement    "Carnivora" (CvTerm.create "")          "Raubtiere" (CvTerm.create "")      (Set.singleton Xref)
        |> FGraph.addElement    "Carnivora" (CvTerm.create "")          "Laurasiatheria" (CvTerm.create "") (Set.singleton IsA)
        |> FGraph.addElement    "Raubtiere" (CvTerm.create "")          "Räuber" (CvTerm.create "")         (Set.singleton Xref)
        |> FGraph.addElement    "Räuber" (CvTerm.create "")             "Eutheria" (CvTerm.create "")       (Set.singleton IsA)
        |> FGraph.addElement    "Höhere Säugetiere" (CvTerm.create "")  "Höhere Säuger" (CvTerm.create "")  (Set.singleton Xref)
        |> FGraph.addElement    "Eutheria" (CvTerm.create "")           "Höhere Säuger" (CvTerm.create "")  (Set.singleton Xref)
        |> FGraph.addElement    "Eutheria" (CvTerm.create "")           "Lateinisch" (CvTerm.create "")     (Set.singleton <| Custom "Sprache")
        |> FGraph.addElement    "Carnivora" (CvTerm.create "")          "Lateinisch" (CvTerm.create "")     (Set.singleton <| Custom "Sprache")
        |> FGraph.addElement    "Hund" (CvTerm.create "")               "Deutsch" (CvTerm.create "")        (Set.singleton <| Custom "Sprache")
        |> FGraph.addElement    "Laurasiatheria" (CvTerm.create "")     "Raubtiere" (CvTerm.create "")      (Set.singleton <| Custom "darunterliegend")
        |> FGraph.addElement    "Lateinisch" (CvTerm.create "")         "Latein" (CvTerm.create "")         (Set.singleton Xref)
        |> FGraph.addElement    "Lateinisch" (CvTerm.create "")         "Deutsch" (CvTerm.create "")        (Set.singleton <| Custom "ist nicht") :?> Ontology