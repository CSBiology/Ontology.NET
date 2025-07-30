namespace ControlledVocabulary


/// Interface ensures the properties necessary for Param.
type IParam =

    inherit ICvBase
    inherit IParamBase


/// Functions for working Param objects.
type Param =

    //---------------------- IParamBase implementations ----------------------//

    /// Returns the value of the Param as a ParamValue.
    static member getParamValue (param: IParam) = 
        ParamBase.getParamValue param

    /// Returns the value of the Param as IConvertible.
    static member getValue (param: IParam) = 
        ParamBase.getValue param

    /// Returns the value of the Param as string.
    static member getValueAsString (param: IParam) = 
        ParamBase.getValueAsString param

    /// Returns the value of the Param as int if possible, else fails.
    static member getValueAsInt (param: IParam) = 
        ParamBase.getValueAsInt param

    /// Returns the value of the Param as a CvTerm.
    static member getValueAsTerm (param: IParam) = 
        ParamBase.getValueAsTerm param

    /// Returns the accession of the Param's value if it exists. Else returns None.
    static member tryGetValueAccession (param: IParam) = 
        ParamBase.tryGetValueAccession param

    /// Returns the reference of the Param's value if it exists. Else returns None.
    static member tryGetValueRef (param: IParam) = 
        ParamBase.tryGetValueRef param

    /// Returns the CvUnit of the Param if it exists. Else returns None.
    static member tryGetCvUnit (param: IParam) : CvUnit option = 
        ParamBase.tryGetCvUnit param

    /// Returns the CvUnit's value of the Param if it exists. Else returns None.
    static member tryGetCvUnitValue (param: IParam) = 
        ParamBase.tryGetCvUnitValue param

    /// Returns the CvUnit's term name of the Param if it exists. Else returns None.
    static member tryGetCvUnitTermName (param: IParam) = 
        ParamBase.tryGetCvUnitTermName param

    /// Returns the CvUnit's term accession of the Param if it exists. Else returns None.
    static member tryGetCvUnitTermAccession (param: IParam) = 
        ParamBase.tryGetCvUnitTermAccession param

    /// Returns the CvUnit's term ref of the Param if it exists. Else returns None.
    static member tryGetCvUnitTermRef (param: IParam) = 
        ParamBase.tryGetCvUnitTermRef param

    /// Maps the value of the Param by appyling the given function.
    static member mapValue (f : ParamValue -> ParamValue) (param : IParam) = 
        ParamBase.mapValue f param :?> IParam

    /// Tries to map the value of the Param by applying the given function and returns Some if successful. Else returns None.
    static member tryMapValue (f : ParamValue -> ParamValue option) (param : IParamBase) = 
        ParamBase.tryMapValue f param 
        |> Option.map (fun v -> v :?> IParam)

    /// Tries to add a name to the Param and returns Some if successful. Else returns None.
    static member tryAddName (name : string) (param : IParamBase) = 
        ParamBase.tryAddName name param
        |> Option.map (fun v -> v :?> IParam)

    /// Tries to add an accession to the Param and returns Some if successful. Else returns None.
    static member tryAddAccession (acc : string) (param : IParamBase) = 
        ParamBase.tryAddAccession acc param
        |> Option.map (fun v -> v :?> IParam)

    /// Tries to add a reference to the Param and returns Some if successful. Else returns None.
    static member tryAddReference (ref : string) (param : IParamBase) = 
        ParamBase.tryAddReference ref param
        |> Option.map (fun v -> v :?> IParam)

    /// Tries to add a unit to the Param and returns Some if successful. Else returns None.
    static member tryAddUnit (unit : CvUnit) (param : IParamBase) = 
        ParamBase.tryAddUnit unit param
        |> Option.map (fun v -> v :?> IParam)

    //------------------------ ICvBase implementations -----------------------//
    
    /// Returns the ID of the CV item.
    static member getCvAccession (param: IParam) = 
        CvBase.getCvAccession param

    /// Returns the name of the CV item.
    static member getCvName (param: IParam) = 
        CvBase.getCvName param

    /// Returns the reference of the CV item.
    static member getCvRef (param: IParam) = 
        CvBase.getCvRef param

    /// Returns the full term of the CV item.
    static member getTerm (param: IParam) = 
        CvBase.getTerm param

    /// Returns true if the given term matches the term of the CV item.
    static member equalsTerm (term : CvTerm) (param: IParam) = 
        CvBase.equalsTerm term param

    /// Returns true if the terms of the given Param items match.
    static member equals (param1 : IParam) (param2 : IParam) = 
        CvBase.equals param1 param2

    /// Returns true if the names of the given Param items match.
    static member equalsName (param1 : IParam) (param2 : IParam) = 
        CvBase.equalsName param1 param2

    /// Returns Some Value of type 'T if the given Param item can be downcast, else returns None.
    static member inline tryAs<'T when 'T :> IParam> (param: IParam) = 
        CvBase.tryAs<'T> param

    /// Returns true if the given Param item can be downcast.
    static member inline is<'T when 'T :> IParam> (param : IParam) = 
        CvBase.is<'T> param

    //-------------------- IParam specific implementations -------------------//


[<AutoOpen>]
module IParamExtensions =

    type CvBase with
        /// Returns Some Param if the given param item can be downcast, else returns None.
        static member tryParam (cv : ICvBase) =
            match cv with
            | :? IParam as param -> Some param
            | _ -> None

    type ParamBase with
        /// Returns Some Param, if the given value item can be downcast, else returns None.
        static member tryParam (cv : IParamBase) =
            match cv with
            | :? IParam as param -> Some param
            | _ -> None