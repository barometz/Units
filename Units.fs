(* 
  Unit conversion.  Doesn't use [<Measure>] because I have no idea how to write templates for that
  and because eventually this should be able to source all units from plaintext data files.
  Currently does not have complete support for resolving compound units (e.g. from joule/second to
  watt) but beyond that supports essentially all operations for converting between single units.
  Goals:
  - Parsing units and conversion information from files
  - Automatically finding the inverse of some conversions (factors, ^-1)
  - Finding the *shortest* composite conversion
  - Avoiding loops in recFindConvPath
*)

type Operation =
    | Mult
    | Div

type Unit = 
    | Simple of string
    | Compound of Unit * Unit * Operation

    member this.ToString =
        match this with
        | Simple(x) -> x
        | Compound(x, y, op) -> 
            match op with
                | Mult -> sprintf "(%s * %s)" x.ToString y.ToString
                | Div ->  sprintf "(%s / %s)" x.ToString y.ToString

/// Test whether two Units are equal.  Should eventually test for equivalence as well.
let rec unitEqual one two =
    match one, two with
    | Simple(t), Simple(s) -> t = s
    | Compound(u1, v1, o1), Compound(u2, v2, o2) -> 
        unitEqual u1 u2 && unitEqual v1 v2 && o1 = o2
    | _, _ -> false

type Datum = { Value : float; Unit: Unit }

type Conversion = { From : Unit; To : Unit; Func : (Datum -> Datum)}

let makeConv from_u to_u (func:(float -> float)) = 
    { From = from_u; To = to_u; Func = fun x -> { Value = func x.Value; Unit = to_u } }

/// Generates a list of two conversions between the given Units based on a factor between them.
let factorConv from_u to_u factor = 
    [
        makeConv from_u to_u <| fun x -> x * factor;
        makeConv to_u from_u <| fun x -> x / factor;
    ]

/// Recursively try to find a path from one Unit to another with at most `n` steps, storing the
/// accumulated function composition in `acc`.
let rec recFindConvPath from_u to_u acc n convs : (Datum -> Datum) option = 
    if n = 0 then
        None
    else
        // Is there a one-step conversion available?
        let perfect = List.tryFind (fun x -> unitEqual from_u x.From && unitEqual to_u x.To) convs
        if perfect.IsSome then
            // We do!
            Some(acc >> perfect.Value.Func)
        else 
            // No perfect match, so look for conversions that have the right From unit
            let partials = List.filter (fun x -> unitEqual from_u x.From) convs
            let result = 
                partials
                // Recurse
                |> List.map (fun x -> recFindConvPath x.To to_u (acc >> x.Func) (n - 1) convs) 
                |> List.tryFind (fun x -> x.IsSome)
            if result.IsSome then
                Some(acc >> result.Value.Value)
            else
                None

let findConvPath from_u to_u convs = 
    recFindConvPath from_u to_u (fun x -> x) 5 convs

[<EntryPoint>]
let main argv = 
    let inch = Simple("in")
    let meter = Simple("m")
    let yard = Simple("yd")
    let fahr = Simple("F")
    let cels = Simple("C")
    let kelv = Simple("K")
    let conversions = 
        factorConv inch meter 0.0254 
        @ factorConv yard inch 36.0
        @ [ 
            makeConv fahr cels (fun x -> (x - 32.0) * 5.0/9.0);
            makeConv cels fahr (fun x -> (x * 9.0/5.0 + 32.0)); 
            makeConv kelv cels (fun x -> x - 273.15);
            makeConv cels kelv (fun x -> x + 273.15) 
          ]
    let convertPrint datum to_u =
        let convfunc = findConvPath datum.Unit to_u conversions
        if convfunc.IsSome then
            let result = convfunc.Value datum
            printf "%f %s = %f %s\n" datum.Value datum.Unit.ToString result.Value result.Unit.ToString
        else
            printf "Could not resolve %s to %s\n" datum.Unit.ToString to_u.ToString

    convertPrint { Value = 5.0; Unit = yard } meter
    convertPrint { Value = 0.9; Unit = meter } yard
    convertPrint { Value = -40.0; Unit = fahr } kelv
    convertPrint { Value = 10.0; Unit = fahr } yard
 
    System.Console.ReadKey false |> ignore
    0 // return an integer exit code
