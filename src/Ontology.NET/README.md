# Ontology

## Getting started

Parse an `Ontology` from an `OboOntology` using the respective function:

```fsharp
open Ontology.NET
open Ontology.NET.OBO


let path = @"C:\myOboFile.obo"

let oboOntology = OboOntology.fromFile false path

let ontology = Ontology.fromOboOntology oboOntology
```

## Working with an Ontology

Once you have parsed your Ontology, you can access and manipulate its terms and relations.

### Add a term:

```fsharp
open ControlledVocabulary


let newTerm = CvTerm.create("TEST:1", "test term 1", "TEST")

Ontology.addTerm newTerm ontology
```

### Get all terms

```fsharp
let allTerms = Ontology.getTerms ontology
```

### Get a specific term

```fsharp
let term = Ontology.getTerm "TEST:1" ontology
```

### Try to get a term (returns option)

```fsharp
match ontology |> Ontology.tryGetTerm "TEST:1" with
| Some term -> printfn "Found: %s" term.Name
| None -> printfn "Not found."
```

### Update a term

```fsharp
let updatedTerm = CvTerm.create("TEST:1", "new name", "TEST")
Ontology.updateTerm "TEST:1" updatedTerm ontology
```

### Remove a term

```fsharp
let termX = CvTerm.create("TEST:X", "test term X", "TEST")
Ontology.addTerm termX ontology

Ontology.removeTerm "TEST:X" ontology
```

### Add a relation

```fsharp
open RelationTypes


Ontology.addTerm (CvTerm.create("TEST:2", "test term 2", "TEST")) ontology

Ontology.addRelation "TEST:1" "TEST:2" IsA ontology
```

### Get relations between two terms

```fsharp
let relations = Ontology.getRelation "TEST:1" "TEST:2" ontology
```

### Remove a specific relation

```fsharp
Ontology.removeRelation "TEST:1" "TEST:2" IsA ontology |> ignore
```

### Remove all relations between two terms

```fsharp
Ontology.removeRelations "TEST:1" "TEST:2" ontology |> ignore
```