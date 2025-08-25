namespace Ontology.NET


open System.Collections.Generic

open ControlledVocabulary
open Graphoscope

open GraphoscopeAux
open Ontology.NET.OBO
open type RelationType


module internal OntologyGraphHelpers =

    let setOrAddEdgeObo sourceTerm searchedTermKey (relationType : RelationType) oboOnto graph =
        let cvtTarget = OboOntology.getOrCreateTerm searchedTermKey oboOnto |> OboTerm.toCvTerm
        if FGraph.containsNode cvtTarget.Accession graph then
            if FGraph.containsEdge sourceTerm.Accession cvtTarget.Accession graph then
                let _, _, alreadyExistingEdge = FGraph.findEdge sourceTerm.Accession cvtTarget.Accession graph
                FGraph.setEdgeData sourceTerm.Accession cvtTarget.Accession (Set.add relationType alreadyExistingEdge) graph
            else
                FGraph.addEdge sourceTerm.Accession cvtTarget.Accession (Set (List.singleton relationType)) graph
            |> ignore
            // TO DO: Replace this with the code below as soon as the `FGraph.tryFindEdge` bug is fixed and a new version with the fix is released.
            //match FGraph.tryFindEdge sourceTerm.Accession cvtTarget.Accession graph with
            //| Some (nk1,nk2,alreadyExistingEdge) -> 
            //    FGraph.setEdgeData sourceTerm.Accession cvtTarget.Accession (Set.add relationType alreadyExistingEdge) graph
            //| None -> 
            //    FGraph.addEdge sourceTerm.Accession cvtTarget.Accession (Set (List.singleton relationType)) graph
            //|> ignore
        else 
            let missingTargetTerm = CvTerm.create(searchedTermKey, "<missing>", "<missing>")
            FGraph.addElement sourceTerm.Accession sourceTerm missingTargetTerm.Accession missingTargetTerm (Set (List.singleton relationType)) graph |> ignore

    // CAUTION: fails if one of the terms doesn't exist in the given Ontology!
    let setOrAddEdge sourceTermId targetTermId (relation : RelationType) onto =
        if FGraph.containsEdge sourceTermId targetTermId onto then
            let _, _, currRel = FGraph.findEdge sourceTermId targetTermId onto
            FGraph.setEdgeData sourceTermId targetTermId (Set.add relation currRel) onto
        else
            FGraph.addEdge sourceTermId targetTermId (Set.singleton relation) onto
        // TO DO: Replace this with the code below as soon as the `FGraph.tryFindEdge` bug is fixed and a new version with the fix is released.
        //match FGraph.tryFindEdge sourceTerm targetTerm onto with
        //| Some (_, _, edgeData) ->
        //    FGraph.setEdgeData sourceTerm targetTerm (Set.add relation edgeData) onto
        //| None -> 
        //    FGraph.addEdge sourceTerm targetTerm (Set.singleton relation) onto


open OntologyGraphHelpers


type Ontology() =


    inherit FGraph<string,CvTerm,RelationType Set>()


    // parsing functionality:

    /// <summary>
    /// Takes a given OboOntology and returns the corresponding Ontology, with the term IDs as node keys, the terms as CvTerms as node data and the relations as edges.
    /// </summary>
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

    /// <summary>
    /// Imports all OboOntologies from the headers of the given one, transforms them into Ontologies and merges them.
    /// </summary>
    /// <param name="verbose">Optional. If true, gives verbose information when parsing. Default is false.</param>
    /// <param name="basePath">Required when Import path is relative.</param>
    /// <param name="oboOnto">The OboOntology whose Imports are parsed.</param>
    /// <remarks>Note that the base path does not change which means that for relative paths all OBO ontologies must stem from the same relative directory.</remarks>
    static member fromOboOntologyWithImportsFromHeaders verbose basePath (oboOnto : OboOntology) =
        oboOnto.WithImportsFromHeaders(?verbose = verbose, ?basePath = basePath)
        |> Seq.map Ontology.fromOboOntology
        |> Ontology.mergeAll

    /// <summary>
    /// Imports all OboOntologies transitively from the headers of the given one, transforms them into Ontologies and merges them.
    /// </summary>
    /// <param name="verbose">Optional. If true, gives verbose information when parsing. Default is false.</param>
    /// <param name="basePath">Required when Import path is relative.</param>
    /// <param name="oboOnto">The OboOntology whose Imports are parsed.</param>
    /// <remarks>Note that the base path does not change which means that for relative paths all OBO ontologies must stem from the same relative directory.</remarks>
    static member fromOboOntologyWithImportsFromHeadersTransitively verbose basePath (oboOnto : OboOntology) =
        oboOnto.WithImportsFromHeadersTransitively(?verbose = verbose, ?basePath = basePath)
        |> Seq.map Ontology.fromOboOntology
        |> Ontology.mergeAll

    /// <summary>
    /// Creates an OboOntology from the Ontology. Incorporates the given header tags if present.
    /// </summary>
    /// <param name="headerTags">Optional. The header tags of the resulting OboOntology. Default is empty.</param>
    member this.ToOboOntology(?headerTags) =

        let ht = Option.defaultValue (OboOntologyHeaderTags.createDefault ()) headerTags

        let rec innerLoop (tt : string) (rts : RelationType list) rs ias xs =
            match rts with
            | h :: t ->
                match h with
                | IsA ->        innerLoop tt t rs (tt :: ias) xs
                | Xref ->       innerLoop tt t rs ias (tt :: xs)
                | Term cvt ->   innerLoop tt t ((tt,cvt.Name) :: rs) ias xs
                | Custom c ->   innerLoop tt t ((tt, c) :: rs) ias xs
            | [] -> rs, ias, xs

        let rec outerLoop (input : (string * RelationType Set) list) rs ias xs =
            match input with
            | (tt,rts) :: t -> 
                let newRs, newIas, newXs = innerLoop tt (Seq.toList rts) rs ias xs
                outerLoop t newRs newIas newXs
            | [] -> rs, ias, xs

        let terms = 
            this.GetTerms() 
            |> Seq.map snd
            |> Seq.map (
                fun cvt -> 
                    let relations = this.GetTargetTermRelations(cvt.Accession)
                    let relationshipsRaw, isAs, xrefsRaw = outerLoop (List.ofSeq relations) [] [] []
                    let relationshipsProcessed = relationshipsRaw |> List.map (fun (tt,rsn) -> OboTerm.constructRelationship tt rsn)
                    let xrefsProcessed = xrefsRaw |> List.map DBXref.ofString
                    OboTerm.Create(
                        cvt.Accession, 
                        Name = cvt.Name,
                        Relationships = relationshipsProcessed,
                        IsA = isAs,
                        Xrefs = xrefsProcessed
                    )
            )

        OboOntology.Create(Seq.toList terms, [], ht)

    /// <summary>
    /// Creates an OboOntology from the given Ontology. Incorporates the given header tags if present.
    /// </summary>
    /// <param name="headerTags">Optional. The header tags of the resulting OboOntology. Default is empty.</param>
    /// <param name="onto">The Ontology that shall be used as the basis of the resulting OboOntology.</param>
    static member toOboOntology headerTags (onto : Ontology) =
        onto.ToOboOntology(headerTags)

    /// <summary>
    /// Takes a sequence of triplets and returns the corresponding Ontology.
    /// </summary>
    /// <param name="triplets">The triplets in the form of source term * relation * target term that serve as the informational basis for the ontology that gets created out of them.</param>
    static member fromTriplets (triplets : (CvTerm * RelationType * CvTerm) seq) = 
        let onto = Ontology()

        triplets
        |> Seq.iter (
            fun (term1,rel,term2) ->
                onto.AddTerm(term1) |> ignore
                onto.AddTerm(term2) |> ignore
                onto.AddRelation(term1.Accession, term2.Accession, rel) |> ignore
        )

        onto

    /// <summary>
    /// Returns the Ontology as a collection of Triplets.
    /// </summary>
    /// <returns>A collection of Triplets in the form of SourceTerm * Relation * TargetTerm.</returns>
    member this.ToTriplets() =
        FGraph.toSeq this
        |> Seq.collect (
            fun (nk1,nd1,nk2,nd2,es) -> 
                es
                |> Seq.map (
                    fun e -> nd1, e, nd2
                )
        )


    // basic functionality:

    /// <summary>
    /// Checks if a term exists under the given ID.
    /// </summary>
    /// <param name="termId">The ID of the term whose presence in the Ontology shall be checked.</param>
    member this.ContainsTerm(termId) =
        FGraph.containsNode termId this

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
    /// Returns all terms of the Ontology as a sequence of term ID * term (as CvTerm).
    /// </summary>
    member this.GetTerms() =
        FGraph.getNodes this

    /// <summary>
    /// Updates the term under the given term ID with a given updated term.
    /// </summary>
    /// <param name="termId">The ID of the term which shall be updated.</param>
    /// <param name="updatedTerm">The updated version of the term that shall replace the old version of it.</param>
    member this.UpdateTerm(termId, updatedTerm) =
        if updatedTerm.Accession <> termId then
            raise (System.ArgumentException($"Term ID {termId} is not compatible to updated term ID {updatedTerm.Accession}. Use `.RemoveTerm` and `.AddTerm` to replace an old term with a new one if the term IDs are different."))
        let sourceTerms, oldTerm, targetTerms = this[termId]
        this[termId] <- (sourceTerms, updatedTerm, targetTerms)
        this

    /// <summary>
    /// Removes the given term from the Ontology. Also removes all of its relations.
    /// </summary>
    /// <param name="termId">The ID of the term that gets removed.</param>
    member this.RemoveTerm(termId) =
        FGraph.removeNode termId this :?> Ontology

    /// <summary>
    /// Checks if at least 1 relation from source term to target term exists. 
    /// </summary>
    /// <param name="sourceTermId">The ID of the term from which the relation originates.</param>
    /// <param name="targetTermId">The ID of the term that is related to the source term.</param>
    /// <remarks>Does not check if a relation exists from target to source term.</remarks>
    member this.HasRelations(sourceTermId, targetTermId) =
        FGraph.containsEdge sourceTermId targetTermId this

    /// <summary>
    /// Checks if the given relation from source term to target term exists.
    /// </summary>
    /// <param name="sourceTermId">The ID of the term from which the relation originates.</param>
    /// <param name="targetTermId">The ID of the term that is related to the source term.</param>
    /// <param name="relation">The relation whose presence shall be checked.</param>
    /// <remarks>Does not check if the relation exists from target to source term.</remarks>
    member this.HasRelation(sourceTermId, targetTermId, relation) =
        match this.TryGetRelations(sourceTermId, targetTermId) with
        | None -> 
            false
        | Some r ->
            Set.contains relation r

    /// <summary>
    /// Adds a relation of source term to target term to the Ontology.
    /// </summary>
    /// <param name="sourceTermId">The ID of the term from which the relation originates.</param>
    /// <param name="targetTermId">The ID of the term that is related to the source term.</param>
    /// <param name="relation">The relation between both terms.</param>
    /// <exception cref="System.ArgumentException">Thrown when source term or target term are not presen in the Ontology.</exception>
    member this.AddRelation(sourceTermId, targetTermId, relation) =
        match FGraph.containsNode sourceTermId this, FGraph.containsNode targetTermId this with
        | true, true -> 
            setOrAddEdge sourceTermId targetTermId relation this :?> Ontology
        | false, true ->
            raise (System.ArgumentException($"source term {sourceTermId} does not exist in the Ontology.", sourceTermId))
        | true, false ->
            raise (System.ArgumentException($"target term {targetTermId} does not exist in the Ontology.", targetTermId))
        | false, false ->
            raise (System.ArgumentException($"terms {sourceTermId} and {targetTermId} do not exist in the Ontology."))

    /// <summary>
    /// Returns the set of Relations from source to target term.
    /// </summary>
    /// <param name="sourceTermId">ID of the source term (where the relation originates).</param>
    /// <param name="targetTermId">ID of the target term (where the relation points to).</param>
    /// <exception cref="System.ArgumentException">Thrown when there is no relation from source to target term in the Ontology.</exception>
    member this.GetRelations(sourceTermId, targetTermId) =
        if FGraph.containsEdge sourceTermId targetTermId this then
            FGraph.findEdge sourceTermId targetTermId this
            |> fun (_,_,e) -> e
        else raise (System.ArgumentException($"There is no relation from source terms {sourceTermId} to target term {targetTermId} in the Ontology."))
        // TO DO: Replace this with the code below as soon as the `FGraph.tryFindEdge` bug is fixed and a new version with the fix is released.
        //match FGraph.tryFindEdge sourceTermId targetTermId this with
        //| Some (_,_,e) -> e
        //| None -> raise (System.ArgumentException($"There is no relation from source terms {sourceTermId} to target term {targetTermId} in the Ontology."))

    /// <summary>
    /// Returns the set of Relations from source to target term if they exist. Else returns None.
    /// </summary>
    /// <param name="sourceTermId">ID of the source term (where the relation originates).</param>
    /// <param name="targetTermId">ID of the target term (where the relation points to).</param>
    member this.TryGetRelations(sourceTermId, targetTermId) =
        if FGraph.containsEdge sourceTermId targetTermId this then
            Some (FGraph.findEdge sourceTermId targetTermId this |> fun (_,_,e) -> e)
        else None
        // TO DO: Replace this with the code below as soon as the `FGraph.tryFindEdge` bug is fixed and a new version with the fix is released.
        //FGraph.tryFindEdge sourceTermId targetTermId this
        //|> Option.map (fun (_,_,e) -> e)

    /// <summary>
    /// Returns all relations of the Ontology as a sequence of source term ID * target term ID * relations.
    /// </summary>
    member this.GetAllRelations() =
        FGraph.toEdgeSeq this

    /// <summary>
    /// Removes all relations from given source to target term. Relations directed vice versa (from target to source term) are unaffected.
    /// </summary>
    /// <param name="sourceTermId">ID of the source term (where the relation originates).</param>
    /// <param name="targetTerm">ID of the target term (where the relation points to).</param>
    member this.RemoveRelations(sourceTermId, targetTermId) =
        FGraph.removeEdge sourceTermId targetTermId this :?> Ontology

    /// <summary>
    /// Removes the given relation from given source to target term. Relations directed vice versa (from target to source term) are unaffected.
    /// </summary>
    /// <param name="sourceTermId">ID of the source term (where the relation originates).</param>
    /// <param name="targetTerm">ID of the target term (where the relation points to).</param>
    /// <param name="relation">The relation to be removed from the set of relations from source to target term.</param>
    /// <exception cref="System.ArgumentException">Thrown when no relation from source term to target term exists.</expection>
    member this.RemoveRelation(sourceTermId, targetTermId, relation) =
        try 
            FGraph.findEdge sourceTermId targetTermId this
            |> fun (_,_,e) ->
                FGraph.setEdgeData sourceTermId targetTermId (Set.remove relation e) this :?> Ontology
        with _ ->
            raise (System.ArgumentException($"no relation from source term {sourceTermId} to target term {targetTermId}."))
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


    // target relation functionality:

    /// <summary>
    /// Returns the target relations of the given term as (target term ID * relations) sequence.
    /// </summary>
    /// <param name="termId">The ID of the term whose target relations shall be returned.</param>
    /// <remarks>Target relations to the given term look like this: "given term -> target term"</remarks>
    member this.GetTargetTermRelations(termId) =
        this[termId]
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
    member this.GetTargetTermsBy(termId, predicate) =
        let visited = HashSet<string>()
        let stack = Stack<string>()

        stack.Push(termId)
        visited.Add(termId) |> ignore

        seq {
            while stack.Count > 0 do
                let nodeKey = stack.Pop()
                let (_, nd, s) = this[nodeKey]
                if nodeKey <> termId then
                    yield nodeKey

                for kv in s do
                    let _, ndSuccessor, _ = this[kv.Key]
                    if not (visited.Contains(kv.Key)) && predicate kv.Key ndSuccessor s[kv.Key] then
                        stack.Push(kv.Key)
                        visited.Add(kv.Key) |> ignore
        }

    /// <summary>
    /// Returns all terms and their Xref-related terms that are transitively target-related to the given term ID, following only those relations for which the provided predicate returns true. The traversal is performed depth-first.
    /// </summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="predicate">A function that takes the current term ID, the corresponding CvTerm, and its outgoing relations, and returns a boolean indicating whether traversal should follow that term.</param>
    /// <returns>A sequence of term IDs representing all target-related terms and their Xref-related terms reachable by recursively following valid relations as defined by the predicate.</returns>
    /// <remarks>A target relation is an outgoing relation. E.g. "Term A -> Term B", Term B is the target-related term to Term A.</remarks>
    member this.GetTargetTermsWithXrefsBy(termId, predicate) =
        let visited = HashSet<string>()
        let stack = Stack<string>()

        stack.Push(termId)
        visited.Add(termId) |> ignore

        seq {
            while stack.Count > 0 do
                let nodeKey = stack.Pop()
                let (_, nd, s) = this[nodeKey]
                if nodeKey <> termId then
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
    /// Returns all terms that are transitively target-related to the given term ID, but limits the traversal to the specified depth. The traversal is performed depth-first.</summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="depth">The maximum depth to traverse.</param>
    /// <returns>A sequence of term IDs representing all target-related terms that can be reached within the given depth, where depth corresponds to the number of relation steps (edges) from the starting term.</returns>
    /// <remarks>A target relation is an outgoing relation. E.g. "Term A -> Term B", Term B is the target-related term to Term A.</remarks>
    member this.GetTargetTermsWithDepth(termId, depth) =
        let visited = HashSet<string>()
        let stack = Stack<string * int>()

        stack.Push(termId,0)
        visited.Add(termId) |> ignore

        seq {
            while stack.Count > 0 do
                let nodeKey, currDepth = stack.Pop()
                let (_, nd, s) = this[nodeKey]
                if nodeKey <> termId then
                    yield nodeKey

                if currDepth < depth then
                    for kv in s do
                        if not (visited.Contains(kv.Key)) then
                            stack.Push(kv.Key, currDepth + 1)
                            visited.Add(kv.Key) |> ignore
        }

    /// <summary>
    /// Returns all terms and their Xref-related terms that are transitively target-related to the given term ID, but limits the traversal to the specified depth. The traversal is performed depth-first.</summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="depth">The maximum depth to traverse.</param>
    /// <returns>A sequence of term IDs representing all target-related terms and their Xref-related terms that can be reached within the given depth, where depth corresponds to the number of relation steps (edges) from the starting term.</returns>
    /// <remarks>A target relation is an outgoing relation. E.g. "Term A -> Term B", Term B is the target-related term to Term A.</remarks>
    member this.GetTargetTermsWithXrefsWithDepth(termId, depth) =
        let visited = HashSet<string>()
        let stack = Stack<string * int>()

        stack.Push(termId,0)
        visited.Add(termId) |> ignore

        seq {
            while stack.Count > 0 do
                let nodeKey, currDepth = stack.Pop()
                let (_, nd, s) = this[nodeKey]
                if nodeKey <> termId then
                    yield nodeKey

                if currDepth < depth then
                    for kv in s do
                        if not (visited.Contains(kv.Key)) then
                            stack.Push(kv.Key, currDepth + 1)
                            visited.Add(kv.Key) |> ignore
                            let xrefs = this.GetXrefs kv.Key
                            xrefs
                            |> Seq.iter (
                                fun xref ->
                                    stack.Push(xref, currDepth)
                                    visited.Add(xref) |> ignore
                            )
        }

    /// <summary>
    /// Returns all terms that are transitively target-related to the given term ID, following only those relations for which the provided predicate returns true but limits the traversal to the specified depth. The traversal is performed depth-first.</summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="depth">The maximum depth to traverse.</param>
    /// <returns>A sequence of term IDs representing all target-related terms that can be reached within the given depth, where depth corresponds to the number of relation steps (edges) from the starting term.</returns>
    /// <remarks>A target relation is an outgoing relation. E.g. "Term A -> Term B", Term B is the target-related term to Term A.</remarks>
    member this.GetTargetTermsWithDepthBy(termId, depth, predicate) =
        let visited = HashSet<string>()
        let stack = Stack<string * int>()

        stack.Push(termId,0)
        visited.Add(termId) |> ignore

        seq {
            while stack.Count > 0 do
                let nodeKey, currDepth = stack.Pop()
                let (_, nd, s) = this[nodeKey]
                if nodeKey <> termId then
                    yield nodeKey

                if currDepth < depth then
                    for kv in s do
                        let _, ndSuccessor, _ = this[kv.Key]
                        if not( visited.Contains(kv.Key)) && predicate kv.Key ndSuccessor s[kv.Key] then
                            stack.Push(kv.Key, currDepth + 1)
                            visited.Add(kv.Key) |> ignore
        }

    /// <summary>
    /// Returns all terms that and their Xref-related terms are transitively target-related to the given term ID, following only those relations for which the provided predicate returns true but limits the traversal to the specified depth. The traversal is performed depth-first.</summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="depth">The maximum depth to traverse.</param>
    /// <returns>A sequence of term IDs representing all target-related terms and their Xref-related terms that can be reached within the given depth, where depth corresponds to the number of relation steps (edges) from the starting term.</returns>
    /// <remarks>A target relation is an outgoing relation. E.g. "Term A -> Term B", Term B is the target-related term to Term A.</remarks>
    member this.GetTargetTermsWithXrefsWithDepthBy(termId, depth, predicate) =
        let visited = HashSet<string>()
        let stack = Stack<string * int>()

        stack.Push(termId,0)
        visited.Add(termId) |> ignore

        seq {
            while stack.Count > 0 do
                let nodeKey, currDepth = stack.Pop()
                let (_, nd, s) = this[nodeKey]
                if nodeKey <> termId then
                    yield nodeKey

                if currDepth < depth then
                    for kv in s do
                        let _, ndSuccessor, _ = this[kv.Key]
                        if not( visited.Contains(kv.Key)) && predicate kv.Key ndSuccessor s[kv.Key] then
                            stack.Push(kv.Key, currDepth + 1)
                            visited.Add(kv.Key) |> ignore
                            let xrefs = this.GetXrefs kv.Key
                            xrefs
                            |> Seq.iter (
                                fun xref ->
                                    stack.Push(xref, currDepth)
                                    visited.Add(xref) |> ignore
                            )
        }


    // source relation functionality:

    /// <summary>
    /// Returns the source relations of the given term as (source term ID * relations) sequence.
    /// </summary>
    /// <param name="termID">The ID of the term whose source relations shall be returned.</param>
    /// <remarks>Source relations to the given term look like this: "source term -> given term"</remarks>
    member this.GetSourceTermRelations(termId) =
        this[termId]
        |> fun (p,_,_) -> 
            p
            |> Seq.map (fun e -> e.Key, e.Value)

    /// <summary>
    /// Returns all terms that are transitively source-related to the given term ID, following only those relations for which the provided predicate returns true. The traversal is performed depth-first.
    /// </summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="predicate">A function that takes the current term ID, the corresponding CvTerm, and its outgoing relations, and returns a boolean indicating whether traversal should follow that term.</param>
    /// <returns>A sequence of term IDs representing all source-related terms reachable by recursively following valid relations as defined by the predicate.</returns>
    /// <remarks>A target relation is an outgoing relation. E.g. "Term A -> Term B", Term A is the source-related term to Term B.</remarks>
    member this.GetSourceTermsBy(termId, predicate) =
        let visited = HashSet<string>()
        let stack = Stack<string>()

        stack.Push(termId)
        visited.Add(termId) |> ignore

        seq {
            while stack.Count > 0 do
                let nodeKey = stack.Pop()
                let (p, nd, s) = this[nodeKey]
                if nodeKey <> termId then
                    yield nodeKey

                for kv in p do
                    let _, ndPred, _ = this[kv.Key]
                    if not (visited.Contains(kv.Key)) && predicate kv.Key ndPred p[kv.Key] then
                        stack.Push(kv.Key)
                        visited.Add(kv.Key) |> ignore
        }

    /// <summary>
    /// Returns all terms and their Xref-related terms that are transitively source-related to the given term ID, following only those relations for which the provided predicate returns true. The traversal is performed depth-first.
    /// </summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="predicate">A function that takes the current term ID, the corresponding CvTerm, and its outgoing relations, and returns a boolean indicating whether traversal should follow that term.</param>
    /// <returns>A sequence of term IDs representing all source-related terms and their Xref-related terms reachable by recursively following valid relations as defined by the predicate.</returns>
    /// <remarks>A target relation is an outgoing relation. E.g. "Term A -> Term B", Term A is the source-related term to Term B.</remarks>
    member this.GetSourceTermsWithXrefsBy(termId, predicate) =
        let visited = HashSet<string>()
        let stack = Stack<string>()

        stack.Push(termId)
        visited.Add(termId) |> ignore

        seq {
            while stack.Count > 0 do
                let nodeKey = stack.Pop()
                let (p, nd, s) = this[nodeKey]
                if nodeKey <> termId then
                    yield nodeKey

                for kv in p do
                    let _, ndPred, _ = this[kv.Key]
                    if not (visited.Contains(kv.Key)) && predicate kv.Key ndPred p[kv.Key] then
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
    /// Returns all terms that are transitively source-related to the given term ID, but limits the traversal to the specified depth. The traversal is performed depth-first.</summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="depth">The maximum depth to traverse. A depth of 0 returns only the starting term.</param>
    /// <returns>A sequence of term IDs representing all source-related terms that can be reached within the given depth, where depth corresponds to the number of relation steps (edges) from the starting term.</returns>
    /// <remarks>A source relation is an incoming relation. E.g. "Term A -> Term B", Term A is the source-related term to Term B.</remarks>
    member this.GetSourceTermsWithDepth(termId, depth) =
        let visited = HashSet<string>()
        let stack = Stack<string * int>()

        stack.Push(termId,0)
        visited.Add(termId) |> ignore

        seq {
            while stack.Count > 0 do
                let nodeKey, currDepth = stack.Pop()
                let (p, nd, s) = this[nodeKey]
                if nodeKey <> termId then
                    yield nodeKey

                if currDepth < depth then
                    for kv in p do
                        if not (visited.Contains(kv.Key)) then
                            stack.Push(kv.Key, currDepth + 1)
                            visited.Add(kv.Key) |> ignore
        }

    /// <summary>
    /// Returns all terms and their Xref-related terms that are transitively source-related to the given term ID, but limits the traversal to the specified depth. The traversal is performed depth-first.</summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="depth">The maximum depth to traverse.</param>
    /// <returns>A sequence of term IDs representing all source-related terms and their Xref-related terms that can be reached within the given depth, where depth corresponds to the number of relation steps (edges) from the starting term.</returns>
    /// <remarks>A source relation is an incoming relation. E.g. "Term A -> Term B", Term A is the source-related term to Term B.</remarks>
    member this.GetSourceTermsWithXrefsWithDepth(termId, depth) =
        let visited = HashSet<string>()
        let stack = Stack<string * int>()

        stack.Push(termId,0)
        visited.Add(termId) |> ignore

        seq {
            while stack.Count > 0 do
                let nodeKey, currDepth = stack.Pop()
                let (p, nd, s) = this[nodeKey]
                if nodeKey <> termId then
                    yield nodeKey

                if currDepth < depth then
                    for kv in p do
                        if not (visited.Contains(kv.Key)) then
                            stack.Push(kv.Key, currDepth + 1)
                            visited.Add(kv.Key) |> ignore
                            let xrefs = this.GetXrefs kv.Key
                            xrefs
                            |> Seq.iter (
                                fun xref ->
                                    stack.Push(xref, currDepth)
                                    visited.Add(xref) |> ignore
                            )
        }

    /// <summary>
    /// Returns all terms that are transitively source-related to the given term ID, following only those relations for which the provided predicate returns true but limits the traversal to the specified depth. The traversal is performed depth-first.</summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="depth">The maximum depth to traverse.</param>
    /// <returns>A sequence of term IDs representing all source-related terms that can be reached within the given depth, where depth corresponds to the number of relation steps (edges) from the starting term.</returns>
    /// <remarks>A source relation is an incoming relation. E.g. "Term A -> Term B", Term A is the source-related term to Term B.</remarks>
    member this.GetSourceTermsWithDepthBy(termId, depth, predicate) =
        let visited = HashSet<string>()
        let stack = Stack<string * int>()

        stack.Push(termId,0)
        visited.Add(termId) |> ignore

        seq {
            while stack.Count > 0 do
                let nodeKey, currDepth = stack.Pop()
                let (p, nd, s) = this[nodeKey]
                if nodeKey <> termId then
                    yield nodeKey

                if currDepth < depth then
                    for kv in p do
                        let _, ndPred, _ = this[kv.Key]
                        if not( visited.Contains(kv.Key)) && predicate kv.Key ndPred p[kv.Key] then
                            stack.Push(kv.Key, currDepth + 1)
                            visited.Add(kv.Key) |> ignore
        }

    /// <summary>
    /// Returns all terms and their Xref-related terms that are transitively source-related to the given term ID, following only those relations for which the provided predicate returns true but limits the traversal to the specified depth. The traversal is performed depth-first.</summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="depth">The maximum depth to traverse.</param>
    /// <returns>A sequence of term IDs representing all source-related terms and their Xref-related terms that can be reached within the given depth, where depth corresponds to the number of relation steps (edges) from the starting term.</returns>
    /// <remarks>A source relation is an incoming relation. E.g. "Term A -> Term B", Term A is the source-related term to Term B.</remarks>
    member this.GetSourceTermsWithXrefsWithDepthBy(termId, depth, predicate) =
        let visited = HashSet<string>()
        let stack = Stack<string * int>()

        stack.Push(termId,0)
        visited.Add(termId) |> ignore

        seq {
            while stack.Count > 0 do
                let nodeKey, currDepth = stack.Pop()
                let (p, nd, s) = this[nodeKey]
                if nodeKey <> termId then
                    yield nodeKey

                if currDepth < depth then
                    for kv in p do
                        let _, ndPred, _ = this[kv.Key]
                        if not( visited.Contains(kv.Key)) && predicate kv.Key ndPred p[kv.Key] then
                            stack.Push(kv.Key, currDepth + 1)
                            visited.Add(kv.Key) |> ignore
                            let xrefs = this.GetXrefs kv.Key
                            xrefs
                            |> Seq.iter (
                                fun xref ->
                                    stack.Push(xref, currDepth)
                                    visited.Add(xref) |> ignore
                            )
        }


    // SuperClass functionality:

    /// <summary>
    /// Returns all terms that are target-related via an is_a relation to the given term ID.
    /// </summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <returns>A sequence of term IDs representing all is_a target-related terms.</returns>
    /// <remarks>A source relation is an incoming relation. E.g. "Term A -> Term B", Term B is the target-related term to Term A. If "->" is an is_a relation, Term B is the superclass of Term A.</remarks>
    member this.GetSuperClasses(termId) =
        FContext.successors this[termId]
        |> Seq.choose (
            fun (tid,r) ->
                if Set.contains IsA r then
                    Some tid
                else None
        )

    /// <summary>
    /// Returns all terms that are transitively target-related via an is_a relation to the given term ID.
    /// </summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <returns>A sequence of term IDs representing all transitively is_a target-related terms.</returns>
    /// <remarks>A source relation is an incoming relation. E.g. "Term A -> Term B", Term B is the target-related term to Term A. If "->" is an is_a relation, Term B is the superclass of Term A.</remarks>
    member this.GetSuperClassesTransitively(termId) =
        this.GetTargetTermsBy(termId, fun _ _ e -> Set.contains IsA e)

    /// <summary>
    /// Returns all terms and their xref-related terms that are transitively target-related via an is_a relation to the given term ID.
    /// </summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <returns>A sequence of term IDs representing all transitively is_a target-related terms and their xref-related terms.</returns>
    /// <remarks>A source relation is an incoming relation. E.g. "Term A -> Term B", Term B is the target-related term to Term A. If "->" is an is_a relation, Term B is the superclass of Term A.</remarks>
    member this.GetSuperClassesWithXrefsTransitively(termId) =
        this.GetTargetTermsWithXrefsBy(termId, fun _ _ e -> Set.contains IsA e)

    /// <summary>
    /// Returns all terms that are transitively target-related via an is_a relation to the given term ID but limits the traversal to the specified depth.
    /// </summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="depth">The maximum depth to traverse.</param>
    /// <returns>A sequence of term IDs representing all transitively is_a target-related terms that can be reached within the given depth.</returns>
    /// <remarks>A source relation is an incoming relation. E.g. "Term A -> Term B", Term B is the target-related term to Term A. If "->" is an is_a relation, Term B is the superclass of Term A.</remarks>
    member this.GetSuperClassesWithDepthTransitively(termId, depth) =
        this.GetTargetTermsWithDepthBy(termId, depth, fun _ _ e -> Set.contains IsA e)

    /// <summary>
    /// Returns all terms and their xref-related terms that are transitively target-related via an is_a relation to the given term ID but limits the traversal to the specified depth.
    /// </summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="depth">The maximum depth to traverse.</param>
    /// <returns>A sequence of term IDs representing all transitively is_a target-related terms and their xref-related terms that can be reached within the given depth.</returns>
    /// <remarks>A source relation is an incoming relation. E.g. "Term A -> Term B", Term B is the target-related term to Term A. If "->" is an is_a relation, Term B is the superclass of Term A.</remarks>
    member this.GetSuperClassesWithXrefsWithDepthTransitively(termId, depth) =
        this.GetTargetTermsWithXrefsWithDepthBy(termId, depth, fun _ _ e -> Set.contains IsA e)


    // SubClass functionality:

    /// <summary>
    /// Returns all terms that are transitively source-related via an is_a relation to the given term ID.
    /// </summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <returns>A sequence of term IDs representing all transitively is_a source-related terms.</returns>
    /// <remarks>A source relation is an incoming relation. E.g. "Term A -> Term B", Term A is the source-related term to Term B. If "->" is an is_a relation, Term A is the subclass of Term B.</remarks>
    member this.GetSubClassesTransitively(termId) =
        this.GetSourceTermsBy(termId, fun _ _ e -> Set.contains IsA e)

    /// <summary>
    /// Returns all terms and their xrefs-related terms that are transitively source-related via an is_a relation to the given term ID.
    /// </summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <returns>A sequence of term IDs representing all transitively is_a source-related terms and their xref-related terms.</returns>
    /// <remarks>A source relation is an incoming relation. E.g. "Term A -> Term B", Term A is the source-related term to Term B. If "->" is an is_a relation, Term A is the subclass of Term B.</remarks>
    member this.GetSubClassesWithXrefsTransitively(termId) =
        this.GetSourceTermsWithXrefsBy(termId, fun _ _ e -> Set.contains IsA e)

    /// <summary>
    /// Returns all terms that are transitively source-related via an is_a relation to the given term ID but limits the traversal to the specified depth.
    /// </summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="depth">The maximum depth to traverse.</param>
    /// <returns>A sequence of term IDs representing all transitively is_a source-related terms that can be reached within the given depth.</returns>
    /// <remarks>A source relation is an incoming relation. E.g. "Term A -> Term B", Term A is the source-related term to Term B. If "->" is an is_a relation, Term A is the subclass of Term B.</remarks>
    member this.GetSubClassesWithDepthTransitively(termId, depth) =
        this.GetSourceTermsWithDepthBy(termId, depth, fun _ _ e -> Set.contains IsA e)

    /// <summary>
    /// Returns all terms that and their xrefs-related terms are transitively source-related via an is_a relation to the given term ID but limits the traversal to the specified depth.
    /// </summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="depth">The maximum depth to traverse.</param>
    /// <returns>A sequence of term IDs representing all transitively is_a source-related terms and their xref-related terms that can be reached within the given depth.</returns>
    /// <remarks>A source relation is an incoming relation. E.g. "Term A -> Term B", Term A is the source-related term to Term B. If "->" is an is_a relation, Term A is the subclass of Term B.</remarks>
    member this.GetSubClassesWithDepthWithXrefsTransitively(termId, depth) =
        this.GetSourceTermsWithXrefsWithDepthBy(termId, depth, fun _ _ e -> Set.contains IsA e)


    // merge functionality:

    /// <summary>
    /// Merges this Ontology with the given one. If this Ontology has xref terms with missing information that are present in the given Ontology, updates those xref terms accordingly.
    /// </summary>
    /// <param name="onto">The Ontology that gets merged into this one.</param>
    member this.MergeWith(onto : Ontology) =
        let newTerms = onto.GetTerms()
        let newRelations = onto.GetAllRelations()
        newTerms
        |> Seq.iter (
            fun (termId,cvTerm) ->
                match this.TryGetTerm(termId) with
                | None ->
                    this.AddTerm(cvTerm)
                    |> ignore
                | Some oldCvTerm ->
                    if oldCvTerm.Name = "<missing>" then
                        this.UpdateTerm(termId, cvTerm)
                        |> ignore
        )
        newRelations
        |> Seq.iter (
            fun (sourceTermId,targetTermId,relations) ->
                relations
                |> Seq.iter (
                    fun relation ->
                        this.AddRelation(sourceTermId, targetTermId, relation)
                        |> ignore
                )
        )
        this


    // additional static methods:

    /// <summary>
    /// Merges 2 given Ontologies. If ther first Ontology has xref terms with missing information that are present in the second Ontology, updates those xref terms accordingly.
    /// </summary>
    /// <param name="onto1">The Ontology that gets the second one merged into it.</param>
    /// <param name="onto2">The Ontology that gets merged into the first one.</param>
    static member merge (onto1 : Ontology) (onto2 : Ontology) =
        onto1.MergeWith(onto2)

    /// <summary>
    /// Merges all given Ontologies into one.
    /// </summary>
    /// <param name="ontos">A collection of Ontologies that shall be merged into one.</param>
    static member mergeAll (ontos : Ontology seq) =
        ontos
        |> Seq.reduce (Ontology.merge)


    // accompanying static methods (to existing instance methods):

    /// <summary>
    /// Checks if a term exists under the given ID.
    /// </summary>
    /// <param name="termId">The ID of the term whose presence in the Ontology shall be checked.</param>
    /// <param name="onto">The Ontology where the presence of the term shall be checked.</param>
    static member containsTerm termId (onto : Ontology) =
        onto.ContainsTerm(termId)

    /// <summary>
    /// Adds a CvTerm to the Ontology.
    /// </summary>
    /// <param name="term">The CvTerm that gets added to the Ontology.</param>
    /// <param name="onto">The Ontology to which the term shall be added.</param>
    static member addTerm term (onto : Ontology) =
        onto.AddTerm(term)

    /// <summary>
    /// Returns the CvTerm under the given term ID.
    /// </summary>
    /// <param name="termId">The ID of the CvTerm that shall be returned.</param>
    /// <param name="onto">The Ontology from which the term shall be retrieved.</param>
    static member getTerm termId (onto : Ontology) =
        onto.GetTerm(termId)

    /// <summary>
    /// Returns the CvTerm under the given term ID if it exists in the Ontology. Else returns None.
    /// </summary>
    /// <param name="termId">The ID of the CvTerm that shall be returned.</param>
    /// <param name="onto">The Ontology from which the term shall be retrieved.</param>
    static member tryGetTerm termId (onto : Ontology) =
        onto.TryGetTerm(termId)

    /// <summary>
    /// Returns all terms of the Ontology.
    /// </summary>
    /// <param name="onto">The Ontology from which the terms shall be retrieved.</param>
    static member getTerms (onto : Ontology) =
        onto.GetTerms()

    /// <summary>
    /// Updates the term under the given term ID with a given updated term.
    /// </summary>
    /// <param name="termId">The ID of the term which shall be updated.</param>
    /// <param name="updatedTerm">The updated version of the term that shall replace the old version of it.</param>
    /// <param name="onto">The Ontology on which the term shall be updated.</param>
    static member updateTerm termId updatedTerm (onto : Ontology) =
        onto.UpdateTerm(termId, updatedTerm)

    /// <summary>
    /// Removes the given term from the Ontology. Also removes all of its relations.
    /// </summary>
    /// <param name="term">The ID of the term that gets removed.</param>
    /// <param name="onto">The Ontology from which the term shall be removed.</param>
    static member removeTerm term (onto : Ontology) =
        onto.RemoveTerm(term)

    /// <summary>
    /// Checks if at least 1 relation from source term to target term exists.
    /// </summary>
    /// <param name="sourceTermId">The ID of the term from which the relation originates.</param>
    /// <param name="targetTermId">The ID of the term that is related to the source term.</param>
    /// <param name="onto">The Ontology in which the relation shall be searched for.</param>
    /// <remarks>Does not check if a relation exists from target to source term.</remarks>
    static member hasRelations sourceTermId targetTermId (onto : Ontology) =
        onto.HasRelations(sourceTermId, targetTermId)

    /// <summary>
    /// Checks if the given relation from source term to target term exists.
    /// </summary>
    /// <param name="sourceTermId">The ID of the term from which the relation originates.</param>
    /// <param name="targetTermId">The ID of the term that is related to the source term.</param>
    /// <param name="relation">The relation whose presence shall be checked.</param>
    /// <param name="onto">The Ontology in which the relation shall be searched for.</param>
    /// <remarks>Does not check if the relation exists from target to source term.</remarks>
    static member hasRelation sourceTermId targetTermId relation (onto : Ontology) =
        onto.HasRelation(sourceTermId, targetTermId, relation)

    /// <summary>
    /// Adds a relation of source term to target term to the Ontology.
    /// </summary>
    /// <param name="sourceTermId">The ID of the term from which the relation originates.</param>
    /// <param name="targetTermId">The ID of the term that is related to the source term.</param>
    /// <param name="relation">The relation between both terms.</param>
    /// <param name="onto">The Ontology in which the relation shall be added.</param>
    static member addRelation sourceTermId targetTermId relation (onto : Ontology) =
        onto.AddRelation(sourceTermId, targetTermId, relation)

    /// <summary>
    /// Returns the set of Relations from source to target term.
    /// </summary>
    /// <param name="sourceTermId">ID of the source term (where the relation originates).</param>
    /// <param name="targetTermId">ID of the target term (where the relation points to).</param>
    /// <param name="onto">The Ontology in which to look for the relation.</param>
    static member getRelations sourceTermId targetTermId (onto : Ontology) =
        onto.GetRelations(sourceTermId, targetTermId)

    /// <summary>
    /// Returns the set of Relations from source to target term if they exist. Else returns None.
    /// </summary>
    /// <param name="sourceTermId">ID of the source term (where the relation originates).</param>
    /// <param name="targetTermId">ID of the target term (where the relation points to).</param>
    /// <param name="onto">The Ontology in which to look for the relation.</param>
    static member tryGetRelations sourceTermId targetTermId (onto : Ontology) =
        onto.TryGetRelations(sourceTermId, targetTermId)

    /// <summary>
    /// Returns all relations of the Ontology as a sequence of source term ID * target term ID * relations.
    /// </summary>
    /// <param name="onto">The Ontology in which to look for the relation.</param>
    static member getAllRelations (onto : Ontology) =
        onto.GetAllRelations()

    /// <summary>
    /// Removes all relations from given source to target term. Relations directed vice versa (from target to source term) are unaffected.
    /// </summary>
    /// <param name="sourceTermId">ID of the source term (where the relation originates).</param>
    /// <param name="targetTermId">ID of the target term (where the relation points to).</param>
    /// <param name="onto">The Ontology from which the relations shall be removed.</param>
    static member removeRelations sourceTermId targetTermId (onto : Ontology) =
        onto.RemoveRelations(sourceTermId, targetTermId)

    /// <summary>
    /// Removes the given relation from given source to target term. Relations directed vice versa (from target to source term) are unaffected.
    /// </summary>
    /// <param name="sourceTermId">ID of the source term (where the relation originates).</param>
    /// <param name="targetTermId">ID of the target term (where the relation points to).</param>
    /// <param name="relation">The relation to be removed from the set of relations from source to target term.</param>
    /// <param name="onto">The Ontology from which the relation shall be removed.</param>
    static member removeRelation sourceTermId targetTermId relation (onto : Ontology) =
        onto.RemoveRelation(sourceTermId, targetTermId, relation)

    /// <summary>
    /// Returns the term IDs of all terms that have an Xref relation to the given term with the given Ontology.
    /// </summary>
    /// <param name="termId">The ID of the term whose Xrefs shall be returned.</param>
    /// <param name="onto">The Ontology in which the term is located.</param>
    static member getXrefs termId (onto : Ontology) =
        onto.GetXrefs termId

    /// <summary>
    /// Returns the target relations of the given term as (target term ID * relations) sequence.
    /// </summary>
    /// <param name="termID">The ID of the term whose target relations shall be returned.</param>
    /// <param name="onto">The Ontology on which the operation is performed.</param>
    static member getTargetTermRelations termId (onto : Ontology) =
        onto.GetTargetTermRelations(termId)

    /// <summary>
    /// Returns all terms that are transitively target-related to the given term ID, following only those relations for which the provided predicate returns true. The traversal is performed depth-first.
    /// </summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="predicate">A function that takes the current term ID, the corresponding CvTerm, and its outgoing relations, and returns a boolean indicating whether traversal should follow that term.</param>
    /// <param name="onto">The Ontology on which the operation is performed.</param>
    /// <returns>A sequence of term IDs and corresponding CvTerms representing all target-related terms reachable by recursively following valid relations as defined by the predicate.</returns>
    /// <remarks>A target relation is an outgoing relation. E.g. "Term A -> Term B", Term B is the target-related term to Term A.</remarks>
    static member getTargetTermsBy termId predicate (onto : Ontology) =
        onto.GetTargetTermsBy(termId, predicate)

    /// <summary>
    /// Returns all terms and their Xref-related terms that are transitively target-related to the given term ID, following only those relations for which the provided predicate returns true. The traversal is performed depth-first.
    /// </summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="predicate">A function that decides whether to traverse a relation.</param>
    /// <param name="onto">The Ontology on which the operation is performed.</param>
    static member getTargetTermsWithXrefsBy termId predicate (onto : Ontology) =
        onto.GetTargetTermsWithXrefsBy(termId, predicate)

    /// <summary>
    /// Returns all terms that are transitively target-related to the given term ID, but limits the traversal to the specified depth. The traversal is performed depth-first.</summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="depth">The maximum depth to traverse.</param>
    /// <param name="onto">The Ontology on which the operation is performed.</param>
    /// <returns>A sequence of term IDs and corresponding CvTerms representing all target-related terms that can be reached within the given depth, where depth corresponds to the number of relation steps (edges) from the starting term.</returns>
    /// <remarks>A target relation is an outgoing relation. E.g. "Term A -> Term B", Term B is the target-related term to Term A.</remarks>
    static member getTargetTermsWithDepth termId depth (onto : Ontology) =
        onto.GetTargetTermsWithDepth(termId, depth)

    /// <summary>
    /// Returns all terms and their Xref-related terms that are transitively target-related to the given term ID, but limits the traversal to the specified depth. The traversal is performed depth-first.
    /// </summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="depth">The maximum depth to traverse.</param>
    /// <param name="onto">The Ontology on which the operation is performed.</param>
    static member getTargetTermsWithXrefsWithDepth termId depth (onto : Ontology) =
        onto.GetTargetTermsWithXrefsWithDepth(termId, depth)

    /// <summary>
    /// Returns all terms that are transitively target-related to the given term ID, following only those relations for which the provided predicate returns true but limits the traversal to the specified depth.
    /// </summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="depth">The maximum depth to traverse.</param>
    /// <param name="predicate">A function that decides whether to traverse a relation.</param>
    /// <param name="onto">The Ontology on which the operation is performed.</param>
    static member getTargetTermsWithDepthBy termId depth predicate (onto : Ontology) =
        onto.GetTargetTermsWithDepthBy(termId, depth, predicate)

    /// <summary>
    /// Returns all terms that and their Xref-related terms are transitively target-related to the given term ID, following only those relations for which the provided predicate returns true but limits the traversal to the specified depth.
    /// </summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="depth">The maximum depth to traverse.</param>
    /// <param name="predicate">A function that decides whether to traverse a relation.</param>
    /// <param name="onto">The Ontology on which the operation is performed.</param>
    static member getTargetTermsWithXrefsWithDepthBy termId depth predicate (onto : Ontology) =
        onto.GetTargetTermsWithXrefsWithDepthBy(termId, depth, predicate)

    /// <summary>
    /// Returns the source relations of the given term as (source term ID * relations) sequence.
    /// </summary>
    /// <param name="termID">The ID of the term whose source relations shall be returned.</param>
    /// <param name="onto">The Ontology on which the operation is performed.</param>
    static member getSourceTermRelations termId (onto : Ontology) =
        onto.GetSourceTermRelations(termId)

    /// <summary>
    /// Returns all terms that are transitively source-related to the given term ID, following only those relations for which the provided predicate returns true. The traversal is performed depth-first.
    /// </summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="predicate">A function that decides whether to traverse a relation.</param>
    /// <param name="onto">The Ontology on which the operation is performed.</param>
    static member getSourceTermsBy termId predicate (onto : Ontology) =
        onto.GetSourceTermsBy(termId, predicate)

    /// <summary>
    /// Returns all terms and their Xref-related terms that are transitively source-related to the given term ID, following only those relations for which the provided predicate returns true. The traversal is performed depth-first.
    /// </summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="predicate">A function that decides whether to traverse a relation.</param>
    /// <param name="onto">The Ontology on which the operation is performed.</param>
    static member getSourceTermsWithXrefsBy termId predicate (onto : Ontology) =
        onto.GetSourceTermsWithXrefsBy(termId, predicate)

    /// <summary>
    /// Returns all terms that are transitively source-related to the given term ID, but limits the traversal to the specified depth.
    /// </summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="depth">The maximum depth to traverse.</param>
    /// <param name="onto">The Ontology on which the operation is performed.</param>
    static member getSourceTermsWithDepth termId depth (onto : Ontology) =
        onto.GetSourceTermsWithDepth(termId, depth)

    /// <summary>
    /// Returns all terms and their Xref-related terms that are transitively source-related to the given term ID, but limits the traversal to the specified depth.
    /// </summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="depth">The maximum depth to traverse.</param>
    /// <param name="onto">The Ontology on which the operation is performed.</param>
    static member getSourceTermsWithXrefsWithDepth termId depth (onto : Ontology) =
        onto.GetSourceTermsWithXrefsWithDepth(termId, depth)

    /// <summary>
    /// Returns all terms that are transitively source-related to the given term ID, following only those relations for which the provided predicate returns true but limits the traversal to the specified depth.
    /// </summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="depth">The maximum depth to traverse.</param>
    /// <param name="predicate">A function that decides whether to traverse a relation.</param>
    /// <param name="onto">The Ontology on which the operation is performed.</param>
    static member getSourceTermsWithDepthBy termId depth predicate (onto : Ontology) =
        onto.GetSourceTermsWithDepthBy(termId, depth, predicate)

    /// <summary>
    /// Returns all terms and their Xref-related terms that are transitively source-related to the given term ID, following only those relations for which the provided predicate returns true but limits the traversal to the specified depth.
    /// </summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="depth">The maximum depth to traverse.</param>
    /// <param name="predicate">A function that decides whether to traverse a relation.</param>
    /// <param name="onto">The Ontology on which the operation is performed.</param>
    static member getSourceTermsWithXrefsWithDepthBy termId depth predicate (onto : Ontology) =
        onto.GetSourceTermsWithXrefsWithDepthBy(termId, depth, predicate)

    /// <summary>
    /// Returns all terms that are transitively target-related via an is_a relation to the given term ID.
    /// </summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="onto">The Ontology on which the operation is performed.</param>
    /// <param name="onto">The Ontology on which the operation is performed.</param>
    /// <returns>A sequence of term IDs representing all is_a target-related terms.</returns>
    static member getSuperClassesTransitively termId (onto : Ontology) =
        onto.GetSuperClassesTransitively(termId)

    /// <summary>
    /// Returns all terms and their xref-related terms that are transitively target-related via an is_a relation to the given term ID.
    /// </summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="onto">The Ontology on which the operation is performed.</param>
    /// <returns>A sequence of term IDs representing all transitively is_a target-related terms and their xref-related terms.</returns>
    /// <remarks>A source relation is an incoming relation. E.g. "Term A -> Term B", Term B is the target-related term to Term A. If "->" is an is_a relation, Term B is the superclass of Term A.</remarks>
    static member getSuperClassesWithXrefsTransitively termID (onto : Ontology) =
        onto.GetSuperClassesWithXrefsTransitively(termID)

    /// <summary>
    /// Returns all terms that are transitively target-related via an is_a relation to the given term ID but limits the traversal to the specified depth.
    /// </summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="depth">The maximum depth to traverse.</param>
    /// <param name="onto">The Ontology on which the operation is performed.</param>
    /// <returns>A sequence of term IDs representing all transitively is_a target-related terms that can be reached within the given depth.</returns>
    /// <remarks>A source relation is an incoming relation. E.g. "Term A -> Term B", Term B is the target-related term to Term A. If "->" is an is_a relation, Term B is the superclass of Term A.</remarks>
    static member getSuperClassesWithDepthTransitively termID depth (onto : Ontology) =
        onto.GetSuperClassesWithDepthTransitively(termID, depth)

    /// <summary>
    /// Returns all terms and their xref-related terms that are transitively target-related via an is_a relation to the given term ID but limits the traversal to the specified depth.
    /// </summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="depth">The maximum depth to traverse.</param>
    /// <param name="onto">The Ontology on which the operation is performed.</param>
    /// <returns>A sequence of term IDs representing all transitively is_a target-related terms and their xref-related terms that can be reached within the given depth.</returns>
    /// <remarks>A source relation is an incoming relation. E.g. "Term A -> Term B", Term B is the target-related term to Term A. If "->" is an is_a relation, Term B is the superclass of Term A.</remarks>
    static member getSuperClassesWithXrefsWithDepthTransitively termID depth (onto : Ontology) =
        onto.GetSuperClassesWithXrefsWithDepthTransitively(termID, depth)

    /// <summary>
    /// Returns all terms that are transitively source-related via an is_a relation to the given term ID.
    /// </summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="onto">The Ontology on which the operation is performed.</param>
    /// <returns>A sequence of term IDs representing all transitively is_a source-related terms.</returns>
    /// <remarks>A source relation is an incoming relation. E.g. "Term A -> Term B", Term A is the source-related term to Term B. If "->" is an is_a relation, Term A is the subclass of Term B.</remarks>
    static member getSubClassesTransitively termId (onto : Ontology) =
        onto.GetSubClassesTransitively(termId)

    /// <summary>
    /// Returns all terms and their xrefs-related terms that are transitively source-related via an is_a relation to the given term ID.
    /// </summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="onto">The Ontology on which the operation is performed.</param>
    /// <returns>A sequence of term IDs representing all is_a source-related terms and their xref-related terms.</returns>
    /// <remarks>A source relation is an incoming relation. E.g. "Term A -> Term B", Term A is the source-related term to Term B. If "->" is an is_a relation, Term A is the subclass of Term B.</remarks>
    static member getSubClassesWithXrefsTransitively termID (onto : Ontology) =
        onto.GetSubClassesWithXrefsTransitively(termID)

    /// <summary>
    /// Returns all terms that are transitively source-related via an is_a relation to the given term ID but limits the traversal to the specified depth.
    /// </summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="depth">The maximum depth to traverse.</param>
    /// <param name="onto">The Ontology on which the operation is performed.</param>
    /// <returns>A sequence of term IDs representing all transitively is_a source-related terms that can be reached within the given depth.</returns>
    /// <remarks>A source relation is an incoming relation. E.g. "Term A -> Term B", Term A is the source-related term to Term B. If "->" is an is_a relation, Term A is the subclass of Term B.</remarks>
    static member getSubClassesWithDepthTransitively termID depth (onto : Ontology) =
        onto.GetSubClassesWithDepthTransitively(termID, depth)

    /// <summary>
    /// Returns all terms that and their xrefs-related terms are transitively source-related via an is_a relation to the given term ID but limits the traversal to the specified depth.
    /// </summary>
    /// <param name="termID">The ID of the starting term.</param>
    /// <param name="depth">The maximum depth to traverse.</param>
    /// <param name="onto">The Ontology on which the operation is performed.</param>
    /// <returns>A sequence of term IDs representing all transitively is_a source-related terms and their xref-related terms that can be reached within the given depth.</returns>
    /// <remarks>A source relation is an incoming relation. E.g. "Term A -> Term B", Term A is the source-related term to Term B. If "->" is an is_a relation, Term A is the subclass of Term B.</remarks>
    static member getSubClassesWithDepthWithXrefsTransitively termID depth (onto : Ontology) =
        onto.GetSubClassesWithDepthWithXrefsTransitively(termID, depth)

    /// <summary>
    /// Returns the given Ontology as a collection of Triplets.
    /// </summary>
    /// <returns>A collection of Triplets in the form of SourceTerm * Relation * TargetTerm.</returns>
    static member toTriplets (onto : Ontology) =
        onto.ToTriplets()