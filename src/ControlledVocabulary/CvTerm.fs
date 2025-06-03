namespace ControlledVocabulary


/// Represents a term from a controlled vocabulary (Cv)
/// in the form of: id|accession ; name|value ; refUri
// ?Maybe [<Struct>]
[<Struct>]
[<CustomEquality; CustomComparison>]
type CvTerm = 
    {
        Accession: string
        Name: string
        RefUri: string
    } with

        static member create(
            accession: string,
            name: string,
            ref : string
        ) = 
            {Accession = accession; Name = name; RefUri = ref}

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

/// Represents a unit term from the unit ontology 
/// in the form of: id|accession * name * refUri
// ?Maybe [<Struct>]
type CvUnit = 
    CvTerm