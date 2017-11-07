[<AutoOpen>]
module GenTypeLevel.RewriteRules

let rec subst var rep expr =
  match expr with
  | Const _ | Var _ -> expr
  | App(a, b) -> App(subst var rep a, subst var rep b)
  | Lambda(n, pat, expr) when not (List.contains rep pat.Vars) ->
    Lambda(n, pat, subst var rep expr)
  | Lambda(_, _, _) -> expr
  | Tuple xs -> Tuple(List.map (subst var rep) xs)

/// Match a chain of lambda applications that can be uncurried.
/// (\e1. (\e2 ... (\en -> body) argn) arg2) arg1
/// ==> (\(e1, e2, ... en) -> body) (arg1, arg2, ... argn)
/// where argk does not depend on e1... ek-1
let rec (|AppChain|_|) expr =
  match expr with
  | App(Lambda(n, Single var, body), arg) ->
    match body with
    | AppChain(_, vs, args, lastBody) ->
      let fvs = args |> List.map freeVars |> Set.unionMany
      if not <| Set.contains var fvs then
        Some(n, var :: vs, arg :: args, lastBody)
      else
        None

    | _ -> Some(n, [var], [arg], body)
  | _ -> None

let rewriteUncurry expr =
  match expr with
  | AppChain(name, vars & (_ :: _ :: _), args, body) ->
    Some(App(Lambda(name, TuplePat vars, body), Tuple args))
  | e -> None

type RewriteRule = Expr -> Expr option

/// Perform one iteration of rewriting on an expression.
let rec rewrite1 (rules : #seq<RewriteRule>) expr =
  let hd = rules |> Seq.tryPick (fun rule -> rule expr)

  match hd with
  | Some hd ->
    Some(hd)
  | None ->
    match expr with
    | Const _ | Var _ -> None
    | App(a, b) ->
      match rewrite1 rules a, rewrite1 rules b with
      | None, None -> None
      | Some(a'), None -> Some(App(a', b))
      | None, Some(b') -> Some(App(a, b'))
      | Some(a'), Some(b') -> Some(App(a', b'))

    | Lambda(n, pat, expr) ->
      rewrite1 rules expr
      |> Option.map (fun expr' -> Lambda(n, pat, expr'))
    | Tuple xs ->
      let xs' = xs |> List.map (rewrite1 rules)
      match Seq.tryPick id xs' with
      | None -> None
      | Some _ ->
        Tuple [ for x, x' in Seq.zip xs xs' ->
                  Option.defaultValue x x' ]
        |> Some

/// Apply rewrite rules repeatedly on an expression.
let rewrite rules expr =
  let rec loop e0 e1 =
    match e1 with
    | None -> e0
    | Some e1 -> loop e1 (rewrite1 rules e1)

  loop expr (rewrite1 rules expr)
