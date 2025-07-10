namespace Ontology.NET


open ControlledVocabulary
open Graphoscope

open GraphoscopeAux
open Ontology.NET.OBO


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


    // Xref functionality:

    /// Returns the term IDs of all terms that have an Xref relation to the given term.
    member this.GetXrefs termId =
        let rec loop newTermId outputList =
            let xrefs = 
                FContext.neighbours this[newTermId] 
                |> Seq.choose (
                    fun (targetNodeKey,relationTypes) -> 
                        if Set.contains Xref relationTypes then
                            Some targetNodeKey
                        else None
                )
            xrefs
            |> Seq.collect (
                fun xref ->
                    loop xref (xrefs :: outputList)
            )
            //if Seq.isEmpty xrefs |> not then
            //    Seq.concat [xrefs; (yield! xrefs |> Seq.map loop)]
            //else xrefs
            //seq {
            //    for (targetNodeKey,relationType) in nbs do
            //        if Set.contains Xref relationType then
            //            loop targetNodeKey
            //        else targetNodeKey
            //}
            //nbs
            //|> Seq.choose (
            //    fun (targetNodeKey,relationTypes) ->
            //        if Set.contains Xref relationTypes then
            //            yield! (loop targetNodeKey)
            //            //Some targetNodeKey
            //        else None
            //)
        loop termId []
        //FContext.neighbours this[termId]
        //|> Seq.choose (
        //    fun (targetNodeKey,relationTypes) ->
        //        if Set.contains Xref relationTypes then
        //            Some targetNodeKey
        //        else None
        //)

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

    member this.GetSourceTermsBy(termID, predicate) =
        


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