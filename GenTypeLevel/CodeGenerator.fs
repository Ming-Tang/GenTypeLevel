/// Convert any lambda calculus expression to run in the F# compiler.
/// In the generated code, the result of the evaluation is stored in type
/// of a variable.
[<AutoOpen>]
module GenTypeLevel.CodeGenerator

let inline header() : Format<_, _, _, _> = """
// BEGIN HEADER ---
type Expr = interface end

let inline (<|-) (x : ^x) (y : ^y) : ^z when ^z :> Expr =
  ( ( ^x or ^y) : (static member Apply : ^x * ^y -> ^z) (x, y))

type E<'a> = E of 'a with
  interface Expr
  static member inline (!!) (E a) = a

type T<'a> = T of 'a with
  interface Expr
  // Tuple implementations here
%s

type App<'a, 'b> when 'a :> Expr and 'b :> Expr = App of 'a * 'b with
  interface Expr
  static member inline (!!) (App(a, b)) = !!(!!a <|- !!b)

let inline (<<-) x y = App(x, y)
let inline (!) x = E x

type A<'a, 'b> = A of ('a -> 'b) with
  static member inline Apply(A f, x) = !(f x)

// END HEADER ---
"""

type Const = string
type Var = string

/// A pattern in a lambda abstraction.
type Pat =
| Single of Var
| TuplePat of Var list

/// A lambda calculus expression.
type Expr =
| Const of Const
| Var of Var
| Tuple of Expr list
| Lambda of suggestedName : string option * Pat * Expr
| App of Expr * Expr

/// A set of free variables associated to an expression.
type FreeVars = FreeVars of Var list

type Pat with
  member p.Vars =
    match p with
    | Single x -> [x]
    | TuplePat xs -> xs

  member p.GeneratedCode =
    match p with
    | Single x -> x
    | TuplePat xs -> sprintf "(%s)" <| String.concat ", " xs

type FreeVars with
  member b.List =
    match b with FreeVars xs -> List.rev xs

  member b.TypeParamList =
    match b with FreeVars xs -> List.rev xs |> List.map (sprintf "'%s")

  member b.Args =
    match b with
    | FreeVars [] -> ""
    | FreeVars xs ->
      b.List
      |> String.concat ", "
      |> sprintf "(%s)"

  member b.TypeParams =
    match b with
    | FreeVars [] -> ""
    | FreeVars xs ->
      b.TypeParamList
      |> String.concat ", "
      |> sprintf "<%s>"

  member b.TypeParamsOf =
    match b with
    | FreeVars [] -> ""
    | FreeVars xs ->
      b.TypeParamList
      |> String.concat " * "
      |> sprintf " of %s"

/// A generated F# type declaration.
type TypeDecl =
  { TypeName : string
    TypeFreeVars : FreeVars
    Pattern : Pat
    Code : string } with

  member t.GeneratedCode =
    let header = sprintf "%s%s = %s%s with"
                         t.TypeName t.TypeFreeVars.TypeParams
                         t.TypeName t.TypeFreeVars.TypeParamsOf
    let apply = sprintf "static member inline Apply(%s%s, %s) ="
                        t.TypeName t.TypeFreeVars.Args
                        t.Pattern.GeneratedCode
    sprintf "%s\n  %s\n    %s" header apply t.Code

/// Get the free variables in the expression expr.
let rec freeVars expr =
  match expr with
  | Const _ -> Set.empty
  | Var v -> Set.singleton v
  | Lambda(_, pat, expr) ->
    Set.difference (freeVars expr) (Set.ofList pat.Vars)
  | App(a, b) -> Set.union (freeVars a) (freeVars b)
  | Tuple xs -> Set.unionMany (List.map freeVars xs)

/// State variable for generating unique names.
type NameGen = Set<string> ref

/// Translate lambda calculus expression to F# type-level expression.
/// Returns the generated type declarations and translated expression.
let rec genExpr (nameGen : NameGen)
                (FreeVars fvs)
                (prefix : string)
                (expr : Expr)
                : TypeDecl list * string =
  let fvs0 = freeVars expr
  let fvs = List.filter (fun x -> Set.contains x fvs0) fvs
  let boundVars = FreeVars fvs

  match expr with
  | Const c -> [], sprintf "!%s" c
  | Var v -> [], sprintf "!%s" v
  | Lambda(lambdaName, pat, expr) ->
    let prefix' = Option.defaultValue prefix lambdaName
    let typeDecls, name = genLambda nameGen pat expr boundVars prefix'
    typeDecls, sprintf "%s%s" name boundVars.Args

  | Tuple xs ->
    let tss, es =
      xs
      |> List.map (fun x -> genExpr nameGen boundVars prefix x)
      |> List.unzip

    List.concat tss,
    es
    |> String.concat ", "
    |> sprintf "T (%s)"

  | App(x, y) ->
    let tsx, gx = genExpr nameGen boundVars prefix x
    let tsy, gy = genExpr nameGen boundVars prefix y
    tsx @ tsy, sprintf "(%s <<- %s)" gx gy

/// Translate a lambda abstraction to F# type declaration.
and genLambda (nameGen : NameGen) (pat : Pat) expr (FreeVars fvs) prefix =
  let fixTypeName (x : string) =
    if x = "" then
      "T_NoName"
    elif not <| System.Char.IsUpper(x.[0]) then
      sprintf "T_%s" x
    else
      x

  let mutable name = fixTypeName prefix
  let mutable k = 1
  while Set.contains name !nameGen do
    name <- fixTypeName <| sprintf "%s_%d" prefix k
    k <- k + 1

  nameGen := Set.add name !nameGen

  let ts, code = genExpr nameGen (FreeVars <| pat.Vars @ fvs) prefix expr
  let typeDecl =
    { TypeName = name
      TypeFreeVars = FreeVars fvs
      Pattern = pat
      Code = code }

  typeDecl :: ts, sprintf "!%s" name

[<AutoOpen>]
module TupleGen =
  /// Get the sizes of tuples appeared in the expression
  let rec tupleSizes expr =
    match expr with
    | Const _ | Var _ -> Seq.empty
    | Lambda(_, Single _, e) -> tupleSizes e
    | Lambda(_, TuplePat xs, e) ->
      seq {
        yield List.length xs
        yield! tupleSizes e
      }
    | App(a, b) -> Seq.append (tupleSizes a) (tupleSizes b)
    | Tuple xs ->
      seq {
        yield List.length xs
        yield! Seq.concat <| Seq.map tupleSizes xs
      }

  let genTupleImpl n =
    let args = Seq.map (sprintf "a%d") [1..n]
    let args1 = Seq.map (sprintf "!!a%d") [1..n]
    sprintf "  static member inline (!!) (T(%s)) = (%s)"
      (String.concat ", " args)
      (String.concat ", " args1)

