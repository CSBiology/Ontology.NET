namespace Ontology.NET.OBO


open Ontology.NET


[<AutoOpen>]
module Extensions =

    type OboOntology with

        /// <summary>
        /// Returns the corresponding Ontology from the OboOntology, with the term IDs as node keys, the terms as CvTerms as node data and the relations as edges.
        /// </summary>
        /// <remarks>If a relation points to a term that is not present in the given OboOntology, initializes them as new CvTerms but with name and ref = "&lt;missing&gt;".</remarks>
        member this.ToOntology() =
            Ontology.fromOboOntology this

        /// <summary>
        /// Returns the corresponding Ontology from the given OboOntology, with the term IDs as node keys, the terms as CvTerms as node data and the relations as edges.
        /// </summary>
        /// <param name="oboOnto">The OboOntology that serves as the basis for the respective Ontology.</param>
        /// <remarks>If a relation points to a term that is not present in the given OboOntology, initializes them as new CvTerms but with name and ref = "&lt;missing&gt;".</remarks>
        static member toOntology (oboOnto : OboOntology) =
            oboOnto.ToOntology()