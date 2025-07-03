namespace Ontology.NET


open ControlledVocabulary
open Ontology.NET.OBO
open Graphoscope


module internal OntologyGraphHelpers =

    let setOrAddEdge sourceTerm searchedTermKey (relationType : RelationType) onto graph =
        let cvtTarget = OboOntology.getOrCreateTerm searchedTermKey onto |> OboTerm.toCvTerm
        if FGraph.containsNode cvtTarget.Accession graph then
            printfn $"Node exists: {cvtTarget.Accession}"
            match FGraph.tryFindEdge sourceTerm.Accession cvtTarget.Accession graph with
            | Some (nk1,nk2,alreadyExistingEdge) -> 
                printfn $"Edge exists: {alreadyExistingEdge}"
                FGraph.setEdgeData sourceTerm.Accession cvtTarget.Accession (Set.add relationType alreadyExistingEdge) graph
            | None -> 
                printfn "Edge exists not"
                FGraph.addEdge sourceTerm.Accession cvtTarget.Accession (Set (List.singleton relationType)) graph
            |> ignore
        else 
            printfn $"Node exists not: {cvtTarget.Accession}"
            let missingTargetTerm = CvTerm.create(searchedTermKey, "<missing>", "<missing>")
            FGraph.addElement sourceTerm.Accession sourceTerm missingTargetTerm.Accession missingTargetTerm (Set (List.singleton relationType)) graph |> ignore


open OntologyGraphHelpers


type Ontology() =


    inherit FGraph<string,CvTerm,RelationType Set>()

    /// <summary>Takes a given OboOntology and transforms it into an Ontology, with the term IDs as node keys, the terms as CvTerms as node data and the relations as edges.</summary>
    /// <remarks>If a relation points to a term that is not present in the given OboOntology, initializes them as new CvTerms but with name and ref = "&lt;missing&gt;".</remarks>
    static member fromOboOntology (oboOnto : OboOntology) =

        let onto = Ontology()

        // Add OboTerms as CvTerms to graph as verteces
        oboOnto.Terms
        |> List.iter (
            fun oboTerm -> 
                let cvTerm = OboTerm.toCvTerm oboTerm
                FGraph.addNode cvTerm.Accession cvTerm onto |> ignore
        )

        // Add relations as edges
        oboOnto.Terms
        |> List.iter (
            fun oboTerm ->

                let cvtSource = OboTerm.toCvTerm oboTerm

                // Add is_a relations
                oboTerm.IsA
                |> List.iter (
                    fun isATerm ->
                        setOrAddEdge cvtSource isATerm IsA oboOnto onto
                )

                // Add xref relations
                oboTerm.Xrefs
                |> List.iter (
                    fun xref ->
                        setOrAddEdge cvtSource xref.Name Xref oboOnto onto
                )

                // Add relationships
                oboTerm.Relationships
                |> List.iter (
                    fun relShip ->
                        let relShipName, relShipTermId = OboTerm.deconstructRelationship relShip
                        setOrAddEdge cvtSource relShipTermId (Custom relShipName) oboOnto onto
                )
        )

        onto