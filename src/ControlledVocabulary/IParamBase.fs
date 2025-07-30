namespace ControlledVocabulary


open System 


/// Interface ensures the value as ParamValue<'T>.
type IParamBase =
    abstract member Value : ParamValue
    abstract member WithValue : ParamValue -> IParamBase


/// Functions for working with IParamBase objects.
type ParamBase = 

    /// <summary>Returns the value of the parameter as a ParamValue.</summary>
    static member getParamValue (param:IParamBase) =
        param.Value

    /// <summary>Returns the inner value of the parameter as IConvertible.</summary>
    static member getValue (param:IParamBase) =
        ParamValue.getValue param.Value

    /// <summary>Returns the inner value of the parameter as string.</summary>
    static member getValueAsString (param:IParamBase) =
        ParamValue.getValueAsString param.Value

    /// <summary>Returns the inner value of the parameter as int. Throws if conversion fails.</summary>
    static member getValueAsInt (param:IParamBase) =
        ParamValue.getValueAsInt param.Value

    /// <summary>Returns the inner value of the parameter as CvTerm.</summary>
    static member getValueAsTerm (param:IParamBase) =
        ParamValue.getValueAsTerm param.Value

    /// <summary>Returns the accession if the parameter holds a CvValue.</summary>
    static member tryGetValueAccession (param : #IParamBase) =
        match param.Value with
        | CvValue                   cv      -> Some cv.Accession
        | Value                      _      -> None
        | WithCvUnitAccession        _      -> None

    /// <summary>Returns the reference URI if the parameter holds a CvValue.</summary>
    static member tryGetValueRef (param : #IParamBase) =
        match param.Value with
        | CvValue                    cv -> Some cv.RefUri
        | Value                      _  -> None
        | WithCvUnitAccession        _  -> None

    /// <summary>Returns the CvUnit if the parameter holds a WithCvUnitAccession value.</summary>
    static member tryGetCvUnit (param : #IParamBase) : CvUnit option =
        match param.Value with
        | Value                  _  -> None
        | CvValue                _  -> None
        | WithCvUnitAccession (_,u) -> Some u

    /// <summary>Returns the numeric value of the CvUnit if available.</summary>
    static member tryGetCvUnitValue (param : #IParamBase) : #IConvertible option =
        match param.Value with
        | Value                  _  -> None
        | CvValue                _  -> None
        | WithCvUnitAccession (v,_) -> Some v

    /// <summary>Returns the name of the CvUnit term if available.</summary>
    static member tryGetCvUnitTermName (param : #IParamBase) =
        match param.Value with
        | Value                  _          -> None
        | CvValue                _          -> None
        | WithCvUnitAccession   (_,cvu)     -> Some cvu.Name

    /// <summary>Returns the accession of the CvUnit term if available.</summary>
    static member tryGetCvUnitTermAccession (param : #IParamBase) =
        match param.Value with
        | Value                  _          -> None
        | CvValue                _          -> None
        | WithCvUnitAccession   (_,cvu)     -> Some cvu.Accession

    /// <summary>Returns the reference URI of the CvUnit term if available.</summary>
    static member tryGetCvUnitTermRef (param : #IParamBase) =
        match param.Value with
        | Value                  _          -> None
        | CvValue                _          -> None
        | WithCvUnitAccession   (_,cvu)     -> Some cvu.RefUri

    /// <summary>Applies a transformation function to the parameter's value and returns a new instance.</summary>
    static member mapValue (f : ParamValue -> ParamValue) (param : IParamBase) = 
        param.WithValue(f param.Value)

    /// <summary>Tries to apply a transformation function to the parameter's value and returns a new instance if successful.</summary>
    static member tryMapValue (f : ParamValue -> ParamValue option) (param : IParamBase) = 
        match f param.Value with
        | Some value -> 
            Some (param.WithValue(value))
        | None -> None

    /// <summary>Tries to add a name to the parameter's value. Returns a new instance if successful.</summary>
    static member tryAddName (value : string) (param : IParamBase) = 
        ParamBase.tryMapValue (ParamValue.tryAddName value) param

    /// <summary>Tries to add an accession to the parameter's value. Returns a new instance if successful.</summary>
    static member tryAddAccession (acc : string) (param : IParamBase) = 
        ParamBase.tryMapValue (ParamValue.tryAddAccession acc) param

    /// <summary>Tries to add a reference URI to the parameter's value. Returns a new instance if successful.</summary>
    static member tryAddReference (ref : string) (param : IParamBase) = 
        ParamBase.tryMapValue (ParamValue.tryAddReference ref) param

    /// <summary>Tries to add a CvUnit to the parameter's value. Returns a new instance if successful.</summary>
    static member tryAddUnit (unit : CvUnit) (param : IParamBase) = 
        ParamBase.tryMapValue (ParamValue.tryAddUnit unit) param
