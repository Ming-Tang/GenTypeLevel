/// Pretty printing of lambda expressions.
[<AutoOpen>]
module GenTypeLevel.ShowExpr

let private nl = System.Environment.NewLine

let indent x =
  let indent = "  "
  (x : string).Split([|nl|], System.StringSplitOptions.RemoveEmptyEntries)
  |> Array.map (sprintf "%s%s" indent)
  |> String.concat nl
  |> sprintf "%s%s" indent

let rec (|MultiLambda|_|) expr =
  match expr with
  | Lambda(name, pat, body) ->
    match body with
    | MultiLambda(names, pats, body) ->
      Some(name :: names, pat :: pats, body)
    | _ -> Some([name], [pat], body)
  | Const _ | Var _ | App(_, _) | Tuple _ -> None

let rec (|MultiApp|_|) expr =
  match expr with
  | App(a, b) ->
    match a with
    | MultiApp(xs) -> Some(xs @ [b])
    | _ -> Some [a; b]

  | Lambda(_, _, _) | Const _ | Var _ | Tuple _ -> None


let showMultiple (xs : string list) =
  let sum = xs |> Seq.sumBy (fun (x : string) -> x.Length)
  let multiline = xs |> Seq.exists (fun (x : string) -> x.Contains(nl))
  if sum >= 40 || multiline then
    match xs with
    | [] -> ""
    | [x] -> x
    | x :: ((_ :: _) as xs') ->
      xs'
      |> List.map indent
      |> String.concat "\n"
      |> sprintf "%s\n%s" x
  else
    String.concat " " xs

let rec showExpr expr =
  let mapFirst f g xs =
    match xs with
    | [] -> []
    | x :: xs -> f x :: List.map g xs

  match expr with
  | Const c -> sprintf "[%s]" c
  | Var v -> sprintf "%s" v
  | MultiApp(xs) ->
    showMultiple (List.map showExpr xs) |> sprintf "(%s)"

  | MultiLambda(names, pats, body) ->
    let ps =
      Seq.zip names pats
      |> Seq.map (fun (a, p) ->
        match a with
        | None -> sprintf "%s" p.GeneratedCode
        | Some(n) -> sprintf "[%s]%s" n p.GeneratedCode)
      |> List.ofSeq
      |> List.rev

    showExpr body :: mapFirst (sprintf "%s.") id ps
    |> List.rev
    |> showMultiple
    |> sprintf "{\\%s}"

  | App(_, _) | Lambda(_, _, _) -> failwith "unreachable code"

#if false
  | App(a, b) ->
    sprintf "(%s)" <| showMultiple [showExpr a; showExpr b]

  | Lambda(None, pat, expr) ->
    showMultiple [sprintf "\\%s." pat.GeneratedCode; showExpr expr]
    |> sprintf "{%s}"
  | Lambda(Some n, pat, expr) ->
    showMultiple [sprintf "\\[%s]%s." n pat.GeneratedCode; showExpr expr]
    |> sprintf "{%s}"
#endif

  | Tuple xs ->
    List.map showExpr xs
    |> List.rev
    |> mapFirst id (sprintf "%s,")
    |> List.rev
    |> showMultiple
    |> sprintf "(%s)"

