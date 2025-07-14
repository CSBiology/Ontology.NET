namespace Ontology.NET


open System.Collections.Generic

open ControlledVocabulary
open Graphoscope

open GraphoscopeAux
open Ontology.NET.OBO


module internal OntologyGraphHelpers =

    let setOrAddEdgeObo sourceTerm searchedTermKey (relationType : RelationType) oboOnto graph =
        let cvtTarget = OboOntology.getOrCreateTerm searchedTermKey oboOnto |> OboTerm.toCvTerm
        if FGraph.containsNode cvtTarget.Accession graph then
            match FGraph.tryFindEdge sourceTerm.Accession cvtTarget.Accession graph with
            | Some (nk1,nk2,alreadyExistingEdge) -> 
                FGraph.setEdgeData sourceTerm.Accession cvtTarget.Accession (Set.add relationType alreadyExistingEdge) graph
            | None -> 
                FGraph.addEdge sourceTerm.Accession cvtTarget.Accession (Set (List.singleton relationType)) graph
            |> ignore
        else 
            let missingTargetTerm = CvTerm.create(searchedTermKey, "<missing>", "<missing>")
            FGraph.addElement sourceTerm.Accession sourceTerm missingTargetTerm.Accession missingTargetTerm (Set (List.singleton relationType)) graph |> ignore

    // CAUTION: fails if one of the terms doesn't exist in the given Ontology!
    let setOrAddEdge sourceTerm targetTerm (relation : RelationType) onto =
        match FGraph.tryFindEdge sourceTerm targetTerm onto with
        | Some (_, _, edgeData) ->
            FGraph.setEdgeData sourceTerm targetTerm (Set.add relation edgeData) onto
        | None -> 
            FGraph.addEdge sourceTerm targetTerm (Set.singleton relation) onto


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
                        setOrAddEdgeObo cvtSource isATerm IsA oboOnto onto
                )

                // Add xref relations
                oboTerm.Xrefs
                |> List.iter (
                    fun xref ->
                        setOrAddEdgeObo cvtSource xref.Name Xref oboOnto onto
                )

                // Add relationships
                oboTerm.Relationships
                |> List.iter (
                    fun relShip ->
                        let relShipName, relShipTermId = OboTerm.deconstructRelationship relShip
                        setOrAddEdgeObo cvtSource relShipTermId (Custom relShipName) oboOnto onto
                )
        )

        onto


    // Basic functionality:

    /// <summary>
    /// Adds a CvTerm to the Ontology.
    /// </summary>
    /// <param name="term">The CvTerm that gets added to the Ontology.</param>
    member this.AddTerm(term : CvTerm) =
        FGraph.addNode term.Accession term this :?> Ontology

    /// <summary>
    /// Adds a relation of source term to target term to the Ontology.
    /// </summary>
    /// <param name="sourceTerm">The ID of the term from which the relation originates.</param>
    /// <param name="targetTerm">The ID of the term that is related to the source term.</param>
    /// <param name="relation">The relation between both terms.</param>
    /// <exception
    member this.AddRelation(sourceTerm, targetTerm, relation) =
        match FGraph.containsNode sourceTerm this, FGraph.containsNode targetTerm this with
        | true, true -> 
            setOrAddEdge sourceTerm targetTerm relation this :?> Ontology
        | false, true ->
            raise (System.ArgumentException($"source term {sourceTerm} does not exist in the Ontology.", sourceTerm))
        | true, false ->
            raise (System.ArgumentException($"target term {targetTerm} does not exist in the Ontology.", targetTerm))
        | false, false ->
            raise (System.ArgumentException($"terms {sourceTerm} and {targetTerm} do not exist in the Ontology."))

    /// <summary>
    /// Removes the given term from the Ontology. Also removes all of its relations.
    /// </summary>
    /// <param name="term">The ID of the term that gets removed.</param>
    member this.RemoveTerm(term) =
        FGraph.removeNode term this :?> Ontology


    // Xref functionality:

    /// Returns the term IDs of all terms that have an Xref relation to the given term.
    member this.GetXrefs termId =
        let visited = HashSet()
        let stack = Stack()

        stack.Push(termId)
        visited.Add(termId) |> ignore

        seq {
            while stack.Count > 0 do
                let nodeKey = stack.Pop()
                let (a, nd, d) = this[nodeKey]
                if nodeKey <> termId then 
                    yield nodeKey

                for kv in a do
                    if not(visited.Contains(kv.Key)) && Set.contains Xref a[kv.Key] then
                        stack.Push(kv.Key)
                        visited.Add(kv.Key) |> ignore

                for kv in d do
                    if not(visited.Contains(kv.Key)) && Set.contains Xref d[kv.Key] then
                        stack.Push(kv.Key)
                        visited.Add(kv.Key) |> ignore
        }

    /// Returns the term IDs of all terms that have an Xref relation to the given term with the given Ontology.
    static member getXrefs termId (onto : Ontology) =
        onto.GetXrefs termId

    /// Returns the terms (as CvTerms) of all terms that have an Xref relation to the given term.
    member this.GetXrefsAsTerms termId =
        FContext.neighbours this[termId]
        |> Seq.choose (
            fun (targetNodeKey,relationTypes) ->
                if Set.contains Xref relationTypes then
                    Some (FGraph.getNodeLabel this targetNodeKey)
                else None
        )

    /// Returns the terms (as CvTerms) of all terms that have an Xref relation to the given term with the given Ontology.
    static member getXrefsAsTerms termId (onto : Ontology) =
        onto.GetXrefsAsTerms termId


    // Target relation functionality:

    /// <summary>
    /// Returns all terms that are transitively target-related to the given term ID, following only those relations for which the provided predicate returns true. The traversal is performed depth-first.
    /// </summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="predicate">A function that takes the current term ID, the corresponding CvTerm, and its outgoing relations, and returns a boolean indicating whether traversal should follow that term.</param>
    /// <returns>A sequence of term IDs and corresponding CvTerms representing all target-related terms reachable by recursively following valid relations as defined by the predicate.</returns>
    /// <remarks>A target relation is an outgoing relation. E.g. "Term A -> Term B", Term B is the target-related term to Term A.</remarks>
    member this.GetTargetTermsBy(termID, predicate) =
        Algorithms.DFS.ofFGraphBy termID predicate this

    /// <summary>
    /// Returns all terms that are transitively target-related to the given term ID, following only those relations for which the provided predicate returns true. The traversal is performed depth-first.
    /// </summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="predicate">A function that takes the current term ID, the corresponding CvTerm, and its outgoing relations, and returns a boolean indicating whether traversal should follow that term.</param>
    /// <param name="onto">The Ontology on which the operation is performed.</param>
    /// <returns>A sequence of term IDs and corresponding CvTerms representing all target-related terms reachable by recursively following valid relations as defined by the predicate.</returns>
    /// <remarks>A target relation is an outgoing relation. E.g. "Term A -> Term B", Term B is the target-related term to Term A.</remarks>
    static member getTargetTermsBy termID predicate (onto : Ontology) =
        onto.GetTargetTermsBy(termID, predicate)

    /// <summary>
    /// Returns all terms that are transitively target-related to the given term ID, but limits the traversal to the specified depth. The traversal is performed depth-first.</summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="depth">The maximum depth to traverse. A depth of 0 returns only the starting term.</param>
    /// <returns>A sequence of term IDs and corresponding CvTerms representing all target-related terms that can be reached within the given depth, where depth corresponds to the number of relation steps (edges) from the starting term.</returns>
    /// <remarks>A target relation is an outgoing relation. E.g. "Term A -> Term B", Term B is the target-related term to Term A.</remarks>
    member this.GetTargetTermsWithDepth(termID, depth) =
        Algorithms.DFS.ofFGraphWithDepth termID depth this

    /// <summary>
    /// Returns all terms that are transitively target-related to the given term ID, but limits the traversal to the specified depth. The traversal is performed depth-first.</summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="depth">The maximum depth to traverse. A depth of 0 returns only the starting term.</param>
    /// <param name="onto">The Ontology on which the operation is performed.</param>
    /// <returns>A sequence of term IDs and corresponding CvTerms representing all target-related terms that can be reached within the given depth, where depth corresponds to the number of relation steps (edges) from the starting term.</returns>
    /// <remarks>A target relation is an outgoing relation. E.g. "Term A -> Term B", Term B is the target-related term to Term A.</remarks>
    static member getTargetTermsWithDepth termID depth (onto : Ontology) =
        onto.GetTargetTermsWithDepth(termID, depth)


    // Source relation functionality:

    //member this.GetSourceTermsBy(termID, predicate) =
        


    // SuperClass functionality:

    /// <summary>
    /// Returns all terms that are transitively target-related via an is_a relation to the given term ID.
    /// </summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <returns>A sequence of term IDs representing all is_a target-related terms.</returns>
    member this.GetSuperClassesTransitively(termID) =
        this.GetTargetTermsBy(termID, fun _ _ e -> Set.contains IsA e) |> Seq.map fst

    /// <summary>
    /// Returns all terms that are transitively target-related via an is_a relation to the given term ID.
    /// </summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="onto">The Ontology on which the operation is performed.</param>
    /// <returns>A sequence of term IDs representing all is_a target-related terms.</returns>
    static member getSuperClassesTransitively termID (onto : Ontology) =
        onto.GetSuperClassesTransitively(termID)

    /// <summary>
    /// Returns all terms that are transitively target-related via an is_a relation to the given term ID.
    /// </summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <returns>A sequence of CvTerms representing all is_a target-related terms.</returns>
    member this.GetSuperClassesTransitivelyAsTerms(termID) =
        this.GetTargetTermsBy(termID, fun _ _ e -> Set.contains IsA e) |> Seq.map snd

    /// <summary>
    /// Returns all terms that are transitively target-related via an is_a relation to the given term ID.
    /// </summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="onto">The Ontology on which the operation is performed.</param>
    /// <returns>A sequence of CvTerms representing all is_a target-related terms.</returns>
    static member getSuperClassesTransitivelyAsTerms termID (onto : Ontology) =
        onto.GetSuperClassesTransitivelyAsTerms(termID)


    // SubClass functionality:

    member this.GetSubClassesTransitively(termID) =
        FContext.predecessors this[termID]