namespace Ontology.NET.OBO


open ARCtrl


module OboOntology =
    /// Finds an OBO term by "TermSourceRef:TermAccessionNumber" style ID and returns it as an ISA OntologyAnnotation type.
    let tryGetOntologyAnnotation (id : string) (oboOnto : OboOntology) = 
        oboOnto.Terms
        |> List.tryPick (fun t ->
            if t.Id = id then Some (OboTerm.toOntologyAnnotation t) else None
        )

    /// Finds an OBO term by "TermSourceRef:TermAccessionNumber" style ID and returns it as an ISA OntologyAnnotation type.
    let getOntologyAnnotation (id : string) (oboOnto : OboOntology) = 
        oboOnto.Terms
        |> List.pick (fun t ->
            if t.Id = id then Some (OboTerm.toOntologyAnnotation t) else None
        )

/// Finds an OBO term by its free text name and returns it as ISA OntologyAnnotation type if it exists in the given OboOntology. Else returns None.
    let tryGetOntologyAnnotationByName (name : string) (oboOnto : OboOntology) = 
        oboOnto.Terms
        |> List.tryPick (fun t ->
            if t.Name = name then Some (OboTerm.toOntologyAnnotation t) else None
        )

    /// Finds an OBO term by its free text name in the given OboOntology and returns it as ISA OntologyAnnotation type.
    let getOntologyAnnotationByName (name : string) (oboOnto : OboOntology) = 
        oboOnto.Terms
        |> List.pick (fun t ->
            if t.Name = name then Some (OboTerm.toOntologyAnnotation t) else None
        )

    /// For a given OntologyAnnotation term, finds all equivalent terms that are connected via XRefs in the given OboOntology.
    ///
    /// Depth can be used to restrict the number of iterations by which neighbours of neighbours are checked.
    let getEquivalentOntologyAnnotations (term : OntologyAnnotation) (depth : int option) (oboOnto : OboOntology) =

        let rec loop dugDepth (equivalents : OntologyAnnotation list) (lastLoop : OntologyAnnotation list) =
            if equivalents.Length = lastLoop.Length then equivalents
            elif depth.IsSome && depth.Value < dugDepth then equivalents
            else
                let newEquivalents = 
                    equivalents
                    |> List.collect (fun t ->
                        let forward = 
                            match oboOnto.TryGetTerm t.TermAccessionShort with
                            | Some term ->
                                term.Xrefs
                                |> List.map (fun xref ->
                                    let id = OntologyAnnotation.fromTermAnnotation(xref.Name).TermAccessionShort
                                    match tryGetOntologyAnnotation id oboOnto with
                                    | Some oa ->
                                        oa
                                    | None -> 
                                        OntologyAnnotation(tan = xref.Name)
                                )
                            | None ->
                                []
                        let backward = 
                            oboOnto.Terms
                            |> List.filter (fun term ->
                                term.Xrefs
                                |> List.exists (fun xref ->
                                    t.Equals(xref.Name)
                                )
                            )
                            |> List.map (fun ot -> OboTerm.toOntologyAnnotation ot)
                        forward @ backward
                    )
                loop (dugDepth + 1) (equivalents @ newEquivalents |> List.distinct) equivalents
        loop 1 [term] []
        |> List.filter ((<>) term)

    /// For a given OntologyAnnotation term, finds all equivalent terms that are connected via XRefs in the given OboOntology.
    ///
    /// Depth can be used to restrict the number of iterations by which neighbours of neighbours are checked.
    let getEquivalentOntologyAnnotationsByName (termId : string) (depth : int option) (oboOnto : OboOntology) =
        match depth with 
        | Some _ ->
            OntologyAnnotation.fromTermAnnotation termId         
            |> fun oa -> getEquivalentOntologyAnnotations oa depth oboOnto
        | None -> 
            getEquivalentOntologyAnnotations (OntologyAnnotation.fromTermAnnotation termId) None oboOnto

    /// For a given OntologyAnnotation term, finds all terms to which this term points in a "isA" relationship in the given OboOntology.
    ///
    /// Depth can be used to restrict the number of iterations by which neighbours of neighbours are checked.
    let getParentOntologyAnnotations (term : OntologyAnnotation) (depth : int option) (oboOnto : OboOntology) =
        let rec loop dugDepth (equivalents : OntologyAnnotation list) (lastLoop : OntologyAnnotation list) =
            if equivalents.Length = lastLoop.Length then equivalents
            elif depth.IsSome && depth.Value < dugDepth then equivalents
            else
                let newEquivalents = 
                    equivalents
                    |> List.collect (fun t ->
                        match oboOnto.TryGetTerm t.TermAccessionShort with
                        | Some term ->
                            term.IsA
                            |> List.map (fun isA ->
                                match tryGetOntologyAnnotation isA oboOnto with
                                | Some oa ->
                                    oa
                                | None -> 
                                    OntologyAnnotation(tan = isA)
                            )
                        | None ->
                            []
                    )
                loop (dugDepth + 1) (equivalents @ newEquivalents |> List.distinct) equivalents
        loop 1 [term] []
        |> List.filter ((<>) term)

    /// For a given OntologyAnnotation term, find all terms to which this term points in a "isA" relationship in a given OboOntology.
    ///
    /// Depth can be used to restrict the number of iterations by which neighbours of neighbours are checked.
    let getParentOntologyAnnotationsByName (termId : string) (depth) (oboOnto : OboOntology) =
        match depth with 
        | Some _ ->
            OntologyAnnotation.fromTermAnnotation termId
            |> fun oa -> getParentOntologyAnnotations oa depth oboOnto
        | None -> 
            getParentOntologyAnnotations (OntologyAnnotation.fromTermAnnotation termId) None oboOnto

    /// For a given OntologyAnnotation term, finds all terms which point to this term "isA" relationship in a given OboOntology.
    ///
    /// Depth can be used to restrict the number of iterations by which neighbours of neighbours are checked.
    let getChildOntologyAnnotations (term : OntologyAnnotation) depth (oboOnto : OboOntology) =
        let rec loop dugDepth (equivalents : OntologyAnnotation list) (lastLoop : OntologyAnnotation list) =
            if equivalents.Length = lastLoop.Length then equivalents
            elif Option.isSome depth && depth.Value < dugDepth then equivalents
            else
                let newEquivalents = 
                    equivalents
                    |> List.collect (fun t ->
                        oboOnto.Terms
                        |> List.choose (fun pt -> 
                            let isChild = 
                                pt.IsA
                                |> List.exists (fun isA -> t.TermAccessionShort = isA)
                            if isChild then
                                Some (OboTerm.toOntologyAnnotation(pt))
                            else
                                None

                        )
                    )
                loop (dugDepth + 1) (equivalents @ newEquivalents |> List.distinct) equivalents
        loop 1 [term] []
        |> List.filter ((<>) term)

    /// For a given OntologyAnnotation term, finds all terms which point to this term "isA" relationship in a given OboOntology.
    ///
    /// Depth can be used to restrict the number of iterations by which neighbours of neighbours are checked.
    let getChildOntologyAnnotationsByName (termId : string) (depth) (oboOnto : OboOntology) =
        match depth with 
        | Some _ ->
            OntologyAnnotation.fromTermAnnotation termId
            |> fun oa -> getChildOntologyAnnotations oa depth oboOnto
        | None -> 
            getChildOntologyAnnotations (OntologyAnnotation.fromTermAnnotation termId) None oboOnto