namespace ControlledVocabulary


open System.Collections.Generic


/// Represents a structured value, annotated by a user defined name.
[<StructuredFormatDisplay("{DisplayText}")>]
type UserParam(name : string, paramValue : ParamValue, attributes : IDictionary<string,IParam>) =

    inherit CvAttributeCollection(attributes)        

    member this.Accession   = name
    member this.Name        = name
    member this.RefUri      = "UserTerm"
    member this.Value       = paramValue
    member this.WithValue(v : ParamValue) = UserParam(name,v,attributes)
    member this.HasAttributes 
        with get() = this.Attributes |> Seq.isEmpty |> not

    interface IParam with 
        member this.Accession                   = this.Accession
        member this.Name                        = this.Name     
        member this.RefUri                      = this.RefUri   
        member this.Value                       = this.Value    
        member this.WithValue(v : ParamValue)   = this.WithValue(v)
        member this.HasAttributes               = this.HasAttributes

    new (name,pv,attributes : seq<IParam>) = 
        let dict = CvAttributeCollection(attributes)
        UserParam (name,pv,dict)
    new (name,pv) = 
        UserParam (name,pv,Seq.empty)

    /// Serves as the default hash function.
    override this.GetHashCode() =
        hash (name, paramValue, attributes)

    /// Determines whether the specified object is equals to the current object.
    override this.Equals(o) =
        match o with
        | :? CvTerm as cvt -> Param.equalsTerm cvt this
        | :? UserParam as up -> 
            up.Name         = this.Name &&
            up.Value        = this.Value &&
            up.Attributes   = this.Attributes
        | :? IParam as p ->
            p.Name  = this.Name &&
            p.Value = this.Value
        | :? ICvBase as cvb -> CvBase.equals cvb this
        | :? IParamBase as pb -> pb.Value = this.Value
        | _ -> false


    //---------------------- IParam implementations ----------------------//

    /// Returns the value of the UserParam as a ParamValue.
    static member getParamValue (up: UserParam) = 
        Param.getParamValue up

    /// Returns the value of the UserParam as IConvertible.
    static member getValue (up: UserParam) = 
        Param.getValue up

    /// Returns the value of the UserParam as string.
    static member getValueAsString (up: UserParam) = 
        Param.getValueAsString up
        
    /// Returns the value of the UserParam as int if possible, else fails.
    static member getValueAsInt (up: UserParam) = 
        Param.getValueAsInt up

    /// Returns the value of the UserParam as a CvTerm.
    static member getValueAsTerm (up: UserParam) = 
        Param.getValueAsTerm up

    /// Returns the value's accession of the UserParam if it exists. Else returns None.
    static member tryGetValueAccession (up: UserParam) = 
        Param.tryGetValueAccession up

    /// Returns the value's reference of the UserParam if it exists. Else returns None.
    static member tryGetValueRef (up: UserParam) = 
        Param.tryGetValueRef up

    /// Returns the value's CvUnit of the UserParam if it exists. Else returns None.
    static member tryGetCvUnit (up: UserParam) : CvUnit option = 
        Param.tryGetCvUnit up

    /// Returns the value's CvUnit's value of the UserParam if it exists. Else returns None.
    static member tryGetCvUnitValue (up: UserParam) = 
        Param.tryGetCvUnitValue up

    /// Returns the value's CvUnit's term name of the UserParam if it exists. Else returns None.
    static member tryGetCvUnitTermName (up: UserParam) = 
        Param.tryGetCvUnitTermName up

    /// Returns the value's CvUnit's term accession of the UserParam if it exists. Else returns None.
    static member tryGetCvUnitTermAccession (up: UserParam) = 
        Param.tryGetCvUnitTermAccession up

    /// Returns the value's CvUnit's term reference of the UserParam if it exists. Else returns None.
    static member tryGetCvUnitTermRef (up: UserParam) = 
        Param.tryGetCvUnitTermRef up

    /// Maps the value of the UserParam by applying the given function.
    static member mapValue (f : ParamValue -> ParamValue) (up: UserParam) = 
        Param.mapValue f up :?> CvParam

    /// Tries mapping the value of the UserParam by applying the given function. Returns Some if it succeeds, else returns None.
    static member tryMapValue (f : ParamValue -> ParamValue option) (up: UserParam) = 
        Param.tryMapValue f up 
        |> Option.map (fun v -> v :?> UserParam)

    /// Tries to add the given name to the UserParam. Returns Some if possible, else returns None.
    static member tryAddName (name : string) (up: UserParam) = 
        Param.tryAddName name up
        |> Option.map (fun v -> v :?> UserParam)

    /// Tries to add the given accession to the UserParam. Returns Some if possible, else returns None.
    static member tryAddAccession (acc : string) (up: UserParam) = 
        Param.tryAddAccession acc up
        |> Option.map (fun v -> v :?> UserParam)

    /// Tries to add the given reference to the UserParam. Returns Some if possible, else returns None.
    static member tryAddReference (ref : string) (up: UserParam) = 
        Param.tryAddReference ref up
        |> Option.map (fun v -> v :?> UserParam)

    /// Tries to add the given CvUnit to the UserParam. Returns Some if possible, else returns None.
    static member tryAddUnit (unit : CvUnit) (up: UserParam) = 
        Param.tryAddUnit unit up
        |> Option.map (fun v -> v :?> UserParam)

    /// Returns the ID of the UserParam.
    static member getCvAccession (up: UserParam) = 
        Param.getCvAccession up

    /// Returns the name of the UserParam.
    static member getCvName (up: UserParam) = 
        Param.getCvName up

    /// Returns the reference of the UserParam.
    static member getCvRef (up: UserParam) = 
        Param.getCvRef up

    /// Returns the full CvTerm of the UserParam.
    static member getTerm (up: UserParam) = 
        Param.getTerm up

    /// Returns true if the given term matches the term of the UserParam.
    static member equalsTerm (term : CvTerm) (up: UserParam) = 
        Param.equalsTerm term up

    /// Returns true if the terms of the given UserParams match.
    static member equals (up1: UserParam) (up2: UserParam) = 
        Param.equals up1 up2

    /// Returns true if the names of the given UserParams match.
    static member equalsName (up1: UserParam) (up2: UserParam) = 
        Param.equalsName up1 up2


    //---------------------- UserParam specific implementations ----------------------//

    static member toCvParam (up: UserParam) = CvParam(CvTerm.create(up.Accession, up.Name, up.RefUri), up.Value, up.Attributes)

    override this.ToString() = 
        $"Name: {this.Name}\n\tValue: {this.Value}\n\tQualifiers: {this.Keys |> Seq.toList}"

    member this.DisplayText = 
        this.ToString()


[<AutoOpen>]
module UserParamExtensions = 

    type CvParam with
        /// Returns the UserParam of the given CvParam.
        static member toUserParam (cvp: CvParam) =
            UserParam(cvp.Name, cvp.Value, cvp.Attributes)

    type ParamBase with
        /// Returns Some UserParam if the given IParamBase can be downcast, else returns None.
        static member tryUserParam (cv : IParamBase) =
            match cv with
            | :? UserParam as param -> Some param
            | _ -> None

    type Param with
        /// Returns Some UserParam if the given IParam can be downcast, else returns None.
        static member tryUserParam (cv : IParam) =
            match cv with
            | :? UserParam as param -> Some param
            | _ -> None

        /// Returns the CvParam of the given IParam.
        static member toCvParam (cv : IParam) =
            match cv with
            | :? UserParam as up -> up |> UserParam.toCvParam
            | :? CvParam as cvp -> cvp
            | _ -> failwith "no conversion to CvParam available for this type"
            
        /// Returns the UserParam of the given IParam.
        static member toUserParam (cv : IParam) =
            match cv with
            | :? UserParam as up -> up 
            | :? CvParam as cvp -> cvp |> CvParam.toUserParam
            | _ -> failwith "no conversion to CvParam available for this type"

    type CvBase with
        /// Returns Some UserParam if the given ICvBase can be downcast, else returns None.
        static member tryUserParam (cv : ICvBase) =
            match cv with
            | :? UserParam as param -> Some param
            | _ -> None