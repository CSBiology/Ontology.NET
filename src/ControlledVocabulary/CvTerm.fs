namespace ControlledVocabulary


open FSharpAux


/// Represents a term from a controlled vocabulary (CV) in the form of: id|accession ; name|value ; refUri.
// ?Maybe [<Struct>]
[<Struct>]
[<CustomEquality; CustomComparison>]
type CvTerm = {
    Accession: string
    Name: string
    RefUri: string
} with

    /// <summary>
    /// Checks if the given accession is an URI.
    /// </summary>
    /// <param name="accession">The input accession.</param>
    static member checkForUri accession =
        let rx = System.Text.RegularExpressions.Regex("^https?:\/\/[a-zA-Z0-9\/.]+\/[a-zA-Z]+_[0-9]+\/?$")
        rx.Match(accession).Success

    /// <summary>
    /// Takes an URI and returns the respective TAN.
    /// </summary>
    /// <param name="uri">The input URI.</param>
    static member uriToTan (uri : string) : string =
        let posLastSlash = String.findIndexBack '/' uri
        uri[posLastSlash + 1 ..]
        |> String.replace "_" ":"

    /// <summary>
    /// Returns the corresponding Term Source Ref from the given Term Number Accession.
    /// </summary>
    /// <param name="accession">The input accession.</param>
    static member refOfAccession accession =
        let m = System.Text.RegularExpressions.Regex.Match(accession, @"^(?<TermSourceRef>[A-Za-z0-9]+):(\d+)$")
        m.Groups["TermSourceRef"].Value

    /// <summary>
    /// Creates a CvTerm from a given accession, name and reference.
    /// </summary>
    /// <param name="accession">The accession of the term.</param>
    /// <param name="name">The name of the term.</param>
    /// <param name="ref">The term source reference of the term.</param>
    static member create(
        accession: string,
        name: string,
        ref : string
    ) = 
        {Accession = accession; Name = name; RefUri = ref}

    /// <summary>
    /// Creates a CvTerm from a given name. Accession and reference are empty.
    /// </summary>
    /// <param name="name">The name of the term.</param>
    static member create(
        name: string
    ) = 
        CvTerm.create(
            name = name,
            accession = "",
            ref = ""
        )

    /// Serves as the default hash function.
    override this.GetHashCode() =
        //this.Accession.GetHashCode()
        hash this.Accession

    /// Determines whether the specified object is equals to the current object.
    override this.Equals o =
        match o with
        | :? CvTerm as cvt -> this.Accession = cvt.Accession
        | _ -> false

    interface System.IComparable<CvTerm> with
        member this.CompareTo cvt =
            compare this.Accession cvt.Accession

    interface System.IComparable with
        member this.CompareTo o =
            match o with
            | :? CvTerm as cvt -> compare this.Accession cvt.Accession
            | _ -> raise (System.ArgumentException("Object must be of type CvTerm"))

/// Represents a unit term from the unit ontology 
/// in the form of: id|accession * name * refUri
// ?Maybe [<Struct>]
type CvUnit = 
    CvTerm