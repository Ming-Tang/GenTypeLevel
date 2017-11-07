/// Convert from F# quotation to Expr's.
[<AutoOpen>]
module GenTypeLevel.FromQuotation

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns
open System.Globalization

type GExpr = GenTypeLevel.CodeGenerator.Expr

let inline A x = failwith ""

let private ti = CultureInfo("en-US", false).TextInfo
let cleanIdent (x : string) =
  x.Replace("@", "")

let inline (|Name|) (x : ^x) =
  let name = ( ^x : (member Name : string) x)
  cleanIdent name

let setName name expr =
  match expr with
  | GExpr.Lambda(_, x, e) -> GExpr.Lambda(Some name, x, e)
  | _ -> expr

let appList xs = List.reduce (fun a b -> GExpr.App(a, b)) xs

let rec fromQ expr : GExpr =
  match expr with
  | Value(_, t) when t = typeof<unit> -> GExpr.Const("()")
  | Value(e, t) when t = typeof<string> -> GExpr.Const(sprintf "%A" e)
  | Value(e, t) when t = typeof<int> -> GExpr.Const(sprintf "%A" e)

  | Var(Name v) -> GExpr.Var(v)
  | Application(a, b) -> GExpr.App(fromQ a, fromQ b)

  | NewUnionCase(Name name, xs) ->
    appList (GExpr.Var(name) :: List.map fromQ xs)

  | SpecificCall <@@ box @@> (None, t, [expr]) -> fromQ expr
  | SpecificCall <@@ unbox @@> (None, t, [expr]) -> fromQ expr
  | SpecificCall <@@ A @@> (None, t, [Value(:? string as v, _)]) ->
    GExpr.Const(sprintf "A(%s)" v)

  | Call(_, Name f, [a]) -> GExpr.App(GExpr.Var(f), fromQ a)
  | Call(_, Name f, xs) -> appList (GExpr.Var(f) :: List.map fromQ xs)

  | Lambda(n, Call(None, Name g, [Var n1]))
      when n.Name = n1.Name && n.Name.EndsWith("@") ->
    //printfn "%A -> Var(%A)" expr g
    GExpr.Var(g)

  | Lambda(Name x, e) ->
    GExpr.Lambda(None, Single x, fromQ e)
  | Let(Name x, e, body) ->
    let name = sprintf "Let_%s" x
    GExpr.App(GExpr.Lambda(Some name, Single x, fromQ body),
              fromQ e |> setName x)

  | Coerce(x, _) -> fromQ x
  | _ -> failwithf "Can't convert: %A" expr

