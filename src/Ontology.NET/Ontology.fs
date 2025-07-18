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


    // parsing functionality:

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


    // basic functionality:

    /// <summary>
    /// Adds a CvTerm to the Ontology.
    /// </summary>
    /// <param name="term">The CvTerm that gets added to the Ontology.</param>
    member this.AddTerm(term : CvTerm) =
        FGraph.addNode term.Accession term this :?> Ontology

    /// <summary>
    /// Returns the CvTerm under the given term ID.
    /// </summary>
    /// <param name="termId">The ID of the CvTerm that shall be returned.</param>
    /// <exception cref="System.Collections.Generic.KeyNotFoundException">Thrown when the given ID has no CvTerm in the Ontology.</exception>
    member this.GetTerm(termId) =
        try FGraph.findNode termId this |> snd with
        | :? KeyNotFoundException -> 
            raise (KeyNotFoundException($"No term with ID present in the Ontology."))

    /// <summary>
    /// Returns the CvTerm under the given term ID if it exists in the Ontology. Else returns None.
    /// </summary>
    /// <param name="termId">The ID of the CvTerm that shall be returned.</param>
    member this.TryGetTerm(termId) =
        try Some (this.GetTerm(termId)) with
        | :? KeyNotFoundException -> None

    /// <summary>
    /// Adds a relation of source term to target term to the Ontology.
    /// </summary>
    /// <param name="sourceTerm">The ID of the term from which the relation originates.</param>
    /// <param name="targetTerm">The ID of the term that is related to the source term.</param>
    /// <param name="relation">The relation between both terms.</param>
    /// <exception cref="System.ArgumentException">Thrown when source term or target term are not presen in the Ontology.</exception>
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
    /// Returns the set of Relations from source to target term.
    /// </summary>
    /// <param name="sourceTerm">ID of the source term (where the relation originates).</param>
    /// <param name="targetTerm">ID of the target term (where the relation points to).</param>
    /// <exception cref="System.ArgumentException">Thrown when there is no relation from source to target term in the Ontology.</exception>
    member this.GetRelation(sourceTerm, targetTerm) =
        if FGraph.containsEdge sourceTerm targetTerm this then
            FGraph.findEdge sourceTerm targetTerm this
            |> fun (_,_,e) -> e
        else raise (System.ArgumentException($"There is no relation from source terms {sourceTerm} to target term {targetTerm} in the Ontology."))
        // TO DO: Replace this with the code below as soon as the `FGraph.tryFindEdge` bug is fixed and a new version with the fix is released.
        //match FGraph.tryFindEdge sourceTerm targetTerm this with
        //| Some (_,_,e) -> e
        //| None -> raise (System.ArgumentException($"There is no relation from source terms {sourceTerm} to target term {targetTerm} in the Ontology."))

    /// <summary>
    /// Removes the given term from the Ontology. Also removes all of its relations.
    /// </summary>
    /// <param name="term">The ID of the term that gets removed.</param>
    member this.RemoveTerm(term) =
        FGraph.removeNode term this :?> Ontology

    /// <summary>
    /// Removes all relations from given source to target term. Relations directed vice versa (from target to source term) are unaffected.
    /// </summary>
    /// <param name="sourceTerm">ID of the source term (where the relation originates).</param>
    /// <param name="targetTerm">ID of the target term (where the relation points to).</param>
    member this.RemoveRelations(sourceTerm, targetTerm) =
        FGraph.removeEdge sourceTerm targetTerm this :?> Ontology

    /// <summary>
    /// Removes the given relation from given source to target term. Relations directed vice versa (from target to source term) are unaffected.
    /// </summary>
    /// <param name="sourceTerm">ID of the source term (where the relation originates).</param>
    /// <param name="targetTerm">ID of the target term (where the relation points to).</param>
    /// <param name="relation">The relation to be removed from the set of relations from source to target term.</param>
    /// <exception cref="System.ArgumentException">Thrown when no relation from source term to target term exists.</expection>
    member this.RemoveRelation(sourceTerm, targetTerm, relation) =
        try 
            FGraph.findEdge sourceTerm targetTerm this
            |> fun (_,_,e) ->
                FGraph.setEdgeData sourceTerm targetTerm (Set.remove relation e) this :?> Ontology
        with _ ->
            raise (System.ArgumentException($"no relation from source term {sourceTerm} to target term {targetTerm}."))
        // TO DO: Replace this with the code below as soon as the `FGraph.tryFindEdge` bug is fixed and a new version with the fix is released.
        //match FGraph.tryFindEdge sourceTerm targetTerm this with
        //| Some (_,_,e) ->
        //    FGraph.setEdgeData sourceTerm targetTerm (Set.remove relation e) this :?> Ontology
        //| None ->
        //    raise (System.ArgumentException($"no edge between source term {sourceTerm} and target term {targetTerm}."))



    // Xref functionality:

    /// <summary>
    /// Returns the term IDs of all terms that have (transitively) an Xref relation to the given term.
    /// </summary>
    /// <param name="termId">The ID of the term by which all Xrefs shall be gotten.</param>
    member this.GetXrefs(termId) =
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

    /// <summary>
    /// Returns the terms (as CvTerms) of all terms that have (transitively) an Xref relation to the given term.
    /// </summary>
    /// <param name="termId">The ID of the term by which all Xrefs shall be gotten.</param>
    member this.GetXrefsAsTerms(termId) =
        let visited = HashSet()
        let stack = Stack()

        stack.Push(termId)
        visited.Add(termId) |> ignore

        seq {
            while stack.Count > 0 do
                let nodeKey = stack.Pop()
                let (a, nd, d) = this[nodeKey]
                if nodeKey <> termId then 
                    yield nd

                for kv in a do
                    if not(visited.Contains(kv.Key)) && Set.contains Xref a[kv.Key] then
                        stack.Push(kv.Key)
                        visited.Add(kv.Key) |> ignore

                for kv in d do
                    if not(visited.Contains(kv.Key)) && Set.contains Xref d[kv.Key] then
                        stack.Push(kv.Key)
                        visited.Add(kv.Key) |> ignore
        }


    // target relation functionality:

    /// <summary>
    /// Returns the target relations of the given term as (target term ID * relations) sequence.
    /// </summary>
    /// <param name="termID">The ID of the term whose target relations shall be returned.</param>
    /// <remarks>Target relations to the given term look like this: "given term -> target term"</remarks>
    member this.GetTargetTermRelations(termID) =
        this[termID]
        |> fun (_,_,s) -> 
            s
            |> Seq.map (fun e -> e.Key, e.Value)

    /// <summary>
    /// Returns all terms that are transitively target-related to the given term ID, following only those relations for which the provided predicate returns true. The traversal is performed depth-first.
    /// </summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="predicate">A function that takes the current term ID, the corresponding CvTerm, and its outgoing relations, and returns a boolean indicating whether traversal should follow that term.</param>
    /// <returns>A sequence of term IDs representing all target-related terms reachable by recursively following valid relations as defined by the predicate.</returns>
    /// <remarks>A target relation is an outgoing relation. E.g. "Term A -> Term B", Term B is the target-related term to Term A.</remarks>
    member this.GetTargetTermsBy(termID, predicate) =
        let visited = HashSet<string>()
        let stack = Stack<string>()

        stack.Push(termID)
        visited.Add(termID) |> ignore

        seq {
            while stack.Count > 0 do
                let nodeKey = stack.Pop()
                let (_, nd, s) = this[nodeKey]
                if nodeKey <> termID then
                    yield nodeKey

                for kv in s do
                    let _, ndSuccessor, _ = this[kv.Key]
                    if not (visited.Contains(kv.Key)) && predicate kv.Key ndSuccessor s[kv.Key] then
                        stack.Push(kv.Key)
                        visited.Add(kv.Key) |> ignore
        }

    /// <summary>
    /// Returns all terms that are transitively target-related to the given term ID, following only those relations for which the provided predicate returns true. The traversal is performed depth-first.
    /// </summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="predicate">A function that takes the current term ID, the corresponding CvTerm, and its outgoing relations, and returns a boolean indicating whether traversal should follow that term.</param>
    /// <returns>A sequence of term IDs representing all target-related terms reachable by recursively following valid relations as defined by the predicate.</returns>
    /// <remarks>A target relation is an outgoing relation. E.g. "Term A -> Term B", Term B is the target-related term to Term A.</remarks>
    member this.GetTargetTermsWithXrefsBy(termID, predicate) =
        let visited = HashSet<string>()
        let stack = Stack<string>()

        stack.Push(termID)
        visited.Add(termID) |> ignore

        seq {
            while stack.Count > 0 do
                let nodeKey = stack.Pop()
                let (_, nd, s) = this[nodeKey]
                if nodeKey <> termID then
                    yield nodeKey

                for kv in s do
                    let _, ndSuccessor, _ = this[kv.Key]
                    if not (visited.Contains(kv.Key)) && predicate kv.Key ndSuccessor s[kv.Key] then
                        stack.Push(kv.Key)
                        visited.Add(kv.Key) |> ignore
                        let xrefs = this.GetXrefs kv.Key
                        xrefs
                        |> Seq.iter (
                            fun xref ->
                                stack.Push(xref)
                                visited.Add(xref) |> ignore
                        )
        }

    /// <summary>
    /// Returns all terms as CvTerms that are transitively target-related to the given term ID, following only those relations for which the provided predicate returns true. The traversal is performed depth-first.
    /// </summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="predicate">A function that takes the current term ID, the corresponding CvTerm, and its outgoing relations, and returns a boolean indicating whether traversal should follow that term.</param>
    /// <returns>A sequence of CvTerms representing all target-related terms reachable by recursively following valid relations as defined by the predicate.</returns>
    /// <remarks>A target relation is an outgoing relation. E.g. "Term A -> Term B", Term B is the target-related term to Term A.</remarks>
    member this.GetTargetTermsAsCvTermsBy(termID, predicate) =
        let visited = HashSet<string>()
        let stack = Stack<string>()

        stack.Push(termID)
        visited.Add(termID) |> ignore

        seq {
            while stack.Count > 0 do
                let nodeKey = stack.Pop()
                let (_, nd, s) = this[nodeKey]
                if nodeKey <> termID then
                    yield nd

                for kv in s do
                    let _, ndSuccessor, _ = this[kv.Key]
                    if not (visited.Contains(kv.Key)) && predicate kv.Key ndSuccessor s[kv.Key] then
                        stack.Push(kv.Key)
                        visited.Add(kv.Key) |> ignore
        }

    /// <summary>
    /// Returns all terms that are transitively target-related to the given term ID, but limits the traversal to the specified depth. The traversal is performed depth-first.</summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="depth">The maximum depth to traverse. A depth of 0 returns only the starting term.</param>
    /// <returns>A sequence of term IDs representing all target-related terms that can be reached within the given depth, where depth corresponds to the number of relation steps (edges) from the starting term.</returns>
    /// <remarks>A target relation is an outgoing relation. E.g. "Term A -> Term B", Term B is the target-related term to Term A.</remarks>
    member this.GetTargetTermsWithDepth(termID, depth) =
        let visited = HashSet<string>()
        let stack = Stack<string * int>()

        stack.Push(termID,0)
        visited.Add(termID) |> ignore

        seq {
            while stack.Count > 0 do
                let nodeKey, currDepth = stack.Pop()
                let (_, nd, s) = this[nodeKey]
                if nodeKey <> termID then
                    yield nodeKey

                if currDepth < depth then
                    for kv in s do
                        if not (visited.Contains(kv.Key)) then
                            stack.Push(kv.Key, currDepth + 1)
                            visited.Add(kv.Key) |> ignore
        }

    /// <summary>
    /// Returns all terms as CvTerms that are transitively target-related to the given term ID, but limits the traversal to the specified depth. The traversal is performed depth-first.</summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="depth">The maximum depth to traverse. A depth of 0 returns only the starting term.</param>
    /// <returns>A sequence of CvTerms representing all target-related terms that can be reached within the given depth, where depth corresponds to the number of relation steps (edges) from the starting term.</returns>
    /// <remarks>A target relation is an outgoing relation. E.g. "Term A -> Term B", Term B is the target-related term to Term A.</remarks>
    member this.GetTargetTermsAsCvTermsWithDepth(termID, depth) =
        let visited = HashSet<string>()
        let stack = Stack<string * int>()

        stack.Push(termID,0)
        visited.Add(termID) |> ignore

        seq {
            while stack.Count > 0 do
                let nodeKey, currDepth = stack.Pop()
                let (_, nd, s) = this[nodeKey]
                if nodeKey <> termID then
                    yield nd

                if currDepth < depth then
                    for kv in s do
                        if not (visited.Contains(kv.Key)) then
                            stack.Push(kv.Key, currDepth + 1)
                            visited.Add(kv.Key) |> ignore
        }

    /// <summary>
    /// Returns all terms that are transitively target-related to the given term ID, following only those relations for which the provided predicate returns true but limits the traversal to the specified depth. The traversal is performed depth-first.</summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="depth">The maximum depth to traverse. A depth of 0 returns only the starting term.</param>
    /// <returns>A sequence of term IDs representing all target-related terms that can be reached within the given depth, where depth corresponds to the number of relation steps (edges) from the starting term.</returns>
    /// <remarks>A target relation is an outgoing relation. E.g. "Term A -> Term B", Term B is the target-related term to Term A.</remarks>
    member this.GetTargetTermsWithDepthBy(termID, depth, predicate) =
        let visited = HashSet<string>()
        let stack = Stack<string * int>()

        stack.Push(termID,0)
        visited.Add(termID) |> ignore

        seq {
            while stack.Count > 0 do
                let nodeKey, currDepth = stack.Pop()
                let (_, nd, s) = this[nodeKey]
                yield nodeKey

                if currDepth < depth then
                    for kv in s do
                        let _, ndSuccessor, _ = this[kv.Key]
                        if not( visited.Contains(kv.Key)) && predicate kv.Key ndSuccessor s[kv.Key] then
                            stack.Push(kv.Key, currDepth + 1)
                            visited.Add(kv.Key) |> ignore
        }

    /// <summary>
    /// Returns all terms as CvTerms that are transitively target-related to the given term ID, following only those relations for which the provided predicate returns true but limits the traversal to the specified depth. The traversal is performed depth-first.</summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="depth">The maximum depth to traverse. A depth of 0 returns only the starting term.</param>
    /// <returns>A sequence of CvTerms representing all target-related terms that can be reached within the given depth, where depth corresponds to the number of relation steps (edges) from the starting term.</returns>
    /// <remarks>A target relation is an outgoing relation. E.g. "Term A -> Term B", Term B is the target-related term to Term A.</remarks>
    member this.GetTargetTermsAsCvTermsWithDepthBy(termID, depth, predicate) =
        let visited = HashSet<string>()
        let stack = Stack<string * int>()

        stack.Push(termID,0)
        visited.Add(termID) |> ignore

        seq {
            while stack.Count > 0 do
                let nodeKey, currDepth = stack.Pop()
                let (_, nd, s) = this[nodeKey]
                yield nd

                if currDepth < depth then
                    for kv in s do
                        let _, ndSuccessor, _ = this[kv.Key]
                        if not( visited.Contains(kv.Key)) && predicate kv.Key ndSuccessor s[kv.Key] then
                            stack.Push(kv.Key, currDepth + 1)
                            visited.Add(kv.Key) |> ignore
        }


    // source relation functionality:

    /// <summary>
    /// Returns the source relations of the given term as (source term ID * relations) sequence.
    /// </summary>
    /// <param name="termID">The ID of the term whose source relations shall be returned.</param>
    /// <remarks>Source relations to the given term look like this: "source term -> given term"</remarks>
    member this.GetSourceTermRelations(termID) =
        this[termID]
        |> fun (p,_,_) -> 
            p
            |> Seq.map (fun e -> e.Key, e.Value)

    //member this.GetSourceTermsBy(termID, predicate) =
        

    /// <summary>
    /// Returns all terms that are transitively source-related to the given term ID, following only those relations for which the provided predicate returns true. The traversal is performed depth-first.
    /// </summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="predicate">A function that takes the current term ID, the corresponding CvTerm, and its outgoing relations, and returns a boolean indicating whether traversal should follow that term.</param>
    /// <returns>A sequence of term IDs representing all source-related terms reachable by recursively following valid relations as defined by the predicate.</returns>
    /// <remarks>A target relation is an outgoing relation. E.g. "Term A -> Term B", Term A is the source-related term to Term B.</remarks>
    member this.GetSourceTermsBy(termID, predicate) =
        let visited = HashSet<string>()
        let stack = Stack<string>()

        stack.Push(termID)
        visited.Add(termID) |> ignore

        seq {
            while stack.Count > 0 do
                let nodeKey = stack.Pop()
                let (p, nd, s) = this[nodeKey]
                if nodeKey <> termID then
                    yield nodeKey

                for kv in p do
                    let _, ndPred, _ = this[kv.Key]
                    if not (visited.Contains(kv.Key)) && predicate kv.Key ndPred s[kv.Key] then
                        stack.Push(kv.Key)
                        visited.Add(kv.Key) |> ignore
        }

    /// <summary>
    /// Returns all terms as CvTerm that are transitively source-related to the given term ID, following only those relations for which the provided predicate returns true. The traversal is performed depth-first.
    /// </summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="predicate">A function that takes the current term ID, the corresponding CvTerm, and its outgoing relations, and returns a boolean indicating whether traversal should follow that term.</param>
    /// <returns>A sequence of term IDs representing all source-related terms reachable by recursively following valid relations as defined by the predicate.</returns>
    /// <remarks>A source relation is an incoming relation. E.g. "Term A -> Term B", Term A is the source-related term to Term B.</remarks>
    member this.GetSourceTermsAsCvTermsBy(termID, predicate) =
        let visited = HashSet<string>()
        let stack = Stack<string>()

        stack.Push(termID)
        visited.Add(termID) |> ignore

        seq {
            while stack.Count > 0 do
                let nodeKey = stack.Pop()
                let (p, nd, s) = this[nodeKey]
                if nodeKey <> termID then
                    yield nd

                for kv in p do
                    let _, ndPred, _ = this[kv.Key]
                    if not (visited.Contains(kv.Key)) && predicate kv.Key ndPred s[kv.Key] then
                        stack.Push(kv.Key)
                        visited.Add(kv.Key) |> ignore
        }

    /// <summary>
    /// Returns all terms that are transitively source-related to the given term ID, but limits the traversal to the specified depth. The traversal is performed depth-first.</summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="depth">The maximum depth to traverse. A depth of 0 returns only the starting term.</param>
    /// <returns>A sequence of term IDs representing all source-related terms that can be reached within the given depth, where depth corresponds to the number of relation steps (edges) from the starting term.</returns>
    /// <remarks>A source relation is an incoming relation. E.g. "Term A -> Term B", Term A is the source-related term to Term B.</remarks>
    member this.GetSourceTermsWithDepth(termID, depth) =
        let visited = HashSet<string>()
        let stack = Stack<string * int>()

        stack.Push(termID,0)
        visited.Add(termID) |> ignore

        seq {
            while stack.Count > 0 do
                let nodeKey, currDepth = stack.Pop()
                let (p, nd, s) = this[nodeKey]
                if nodeKey <> termID then
                    yield nodeKey

                if currDepth < depth then
                    for kv in p do
                        if not (visited.Contains(kv.Key)) then
                            stack.Push(kv.Key, currDepth + 1)
                            visited.Add(kv.Key) |> ignore
        }

    /// <summary>
    /// Returns all terms as CvTerms that are transitively source-related to the given term ID, but limits the traversal to the specified depth. The traversal is performed depth-first.</summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="depth">The maximum depth to traverse. A depth of 0 returns only the starting term.</param>
    /// <returns>A sequence of CvTerms representing all source-related terms that can be reached within the given depth, where depth corresponds to the number of relation steps (edges) from the starting term.</returns>
    /// <remarks>A source relation is an incoming relation. E.g. "Term A -> Term B", Term A is the source-related term to Term B.</remarks>
    member this.GetSourceTermsAsCvTermsWithDepth(termID, depth) =
        let visited = HashSet<string>()
        let stack = Stack<string * int>()

        stack.Push(termID,0)
        visited.Add(termID) |> ignore

        seq {
            while stack.Count > 0 do
                let nodeKey, currDepth = stack.Pop()
                let (p, nd, s) = this[nodeKey]
                if nodeKey <> termID then
                    yield nodeKey

                if currDepth < depth then
                    for kv in p do
                        if not (visited.Contains(kv.Key)) then
                            stack.Push(kv.Key, currDepth + 1)
                            visited.Add(kv.Key) |> ignore
        }

    /// <summary>
    /// Returns all terms that are transitively source-related to the given term ID, following only those relations for which the provided predicate returns true but limits the traversal to the specified depth. The traversal is performed depth-first.</summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="depth">The maximum depth to traverse. A depth of 0 returns only the starting term.</param>
    /// <returns>A sequence of term IDs representing all source-related terms that can be reached within the given depth, where depth corresponds to the number of relation steps (edges) from the starting term.</returns>
    /// <remarks>A source relation is an incoming relation. E.g. "Term A -> Term B", Term A is the source-related term to Term B.</remarks>
    member this.GetSourceTermsWithDepthBy(termID, depth, predicate) =
        let visited = HashSet<string>()
        let stack = Stack<string * int>()

        stack.Push(termID,0)
        visited.Add(termID) |> ignore

        seq {
            while stack.Count > 0 do
                let nodeKey, currDepth = stack.Pop()
                let (p, nd, s) = this[nodeKey]
                yield nodeKey

                if currDepth < depth then
                    for kv in p do
                        let _, ndPred, _ = this[kv.Key]
                        if not( visited.Contains(kv.Key)) && predicate kv.Key ndPred s[kv.Key] then
                            stack.Push(kv.Key, currDepth + 1)
                            visited.Add(kv.Key) |> ignore
        }

    /// <summary>
    /// Returns all terms as CvTerms that are transitively source-related to the given term ID, following only those relations for which the provided predicate returns true but limits the traversal to the specified depth. The traversal is performed depth-first.</summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="depth">The maximum depth to traverse. A depth of 0 returns only the starting term.</param>
    /// <returns>A sequence of CvTerms representing all source-related terms that can be reached within the given depth, where depth corresponds to the number of relation steps (edges) from the starting term.</returns>
    /// <remarks>A source relation is an incoming relation. E.g. "Term A -> Term B", Term A is the source-related term to Term B.</remarks>
    member this.GetSourceTermsAsCvTermsWithDepthBy(termID, depth, predicate) =
        let visited = HashSet<string>()
        let stack = Stack<string * int>()

        stack.Push(termID,0)
        visited.Add(termID) |> ignore

        seq {
            while stack.Count > 0 do
                let nodeKey, currDepth = stack.Pop()
                let (p, nd, s) = this[nodeKey]
                yield nd

                if currDepth < depth then
                    for kv in p do
                        let _, ndPred, _ = this[kv.Key]
                        if not( visited.Contains(kv.Key)) && predicate kv.Key ndPred s[kv.Key] then
                            stack.Push(kv.Key, currDepth + 1)
                            visited.Add(kv.Key) |> ignore
        }


    // SuperClass functionality:

    /// <summary>
    /// Returns all terms that are transitively target-related via an is_a relation to the given term ID.
    /// </summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <returns>A sequence of term IDs representing all is_a target-related terms.</returns>
    member this.GetSuperClassesTransitively(termID) =
        this.GetTargetTermsBy(termID, fun _ _ e -> Set.contains IsA e)

    /// <summary>
    /// Returns all terms that are transitively target-related via an is_a relation to the given term ID.
    /// </summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <returns>A sequence of CvTerms representing all is_a target-related terms.</returns>
    member this.GetSuperClassesTransitivelyAsTerms(termID) =
        this.GetTargetTermsAsCvTermsBy(termID, fun _ _ e -> Set.contains IsA e)


    // SubClass functionality:

    member this.GetSubClassesTransitively(termID) =
        FContext.predecessors this[termID]


    // accompanying static members (to existing object methods):

    /// Returns the term IDs of all terms that have an Xref relation to the given term with the given Ontology.
    static member getXrefs termId (onto : Ontology) =
        onto.GetXrefs termId

    /// Returns the terms (as CvTerms) of all terms that have an Xref relation to the given term with the given Ontology.
    static member getXrefsAsTerms termId (onto : Ontology) =
        onto.GetXrefsAsTerms termId

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
    /// <param name="onto">The Ontology on which the operation is performed.</param>
    /// <returns>A sequence of term IDs and corresponding CvTerms representing all target-related terms that can be reached within the given depth, where depth corresponds to the number of relation steps (edges) from the starting term.</returns>
    /// <remarks>A target relation is an outgoing relation. E.g. "Term A -> Term B", Term B is the target-related term to Term A.</remarks>
    static member getTargetTermsWithDepth termID depth (onto : Ontology) =
        onto.GetTargetTermsWithDepth(termID, depth)

    /// <summary>
    /// Returns all terms that are transitively target-related via an is_a relation to the given term ID.
    /// </summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="onto">The Ontology on which the operation is performed.</param>
    /// <returns>A sequence of CvTerms representing all is_a target-related terms.</returns>
    static member getSuperClassesTransitivelyAsTerms termID (onto : Ontology) =
        onto.GetSuperClassesTransitivelyAsTerms(termID)

    /// <summary>
    /// Returns all terms that are transitively target-related via an is_a relation to the given term ID.
    /// </summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="onto">The Ontology on which the operation is performed.</param>
    /// <returns>A sequence of term IDs representing all is_a target-related terms.</returns>
    static member getSuperClassesTransitively termID (onto : Ontology) =
        onto.GetSuperClassesTransitively(termID)