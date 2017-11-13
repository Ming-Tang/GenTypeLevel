module GenTypeLevel.Main
open GenTypeLevel
open FParsec

[<AutoOpen>]
module ExprParser =
  let ws = spaces
  let str_ws s = pstring s .>> ws

  let ident =
    let isAsciiIdStart    = fun c -> isAsciiLetter c || c = '_'
    let isAsciiIdContinue = fun c -> isAsciiLetter c || isDigit c || c = '_'

    IdentifierOptions(
      isAsciiIdStart = isAsciiIdStart,
      isAsciiIdContinue = isAsciiIdContinue,
      normalization = System.Text.NormalizationForm.FormKC,
      normalizeBeforeValidation = true,
      allowAllNonAsciiCharsInPreCheck = true)
    |> identifier
    .>> ws

  let primary, primaryRef = createParserForwardedToRef()
  let expr, exprRef = createParserForwardedToRef()
  let pat, patRef = createParserForwardedToRef()

  let identPat = ident |>> Single
  let tuplePat =
    str_ws "(" >>. sepBy ident (str_ws ",") .>> str_ws ")"
    |>> TuplePat

  do patRef := choice [identPat; tuplePat]

  let parens = str_ws "(" >>. expr .>> str_ws ")"

  let namePart = str_ws "[" >>. ident .>> str_ws "]"

  let lamBody lamArrow =
    let lamBody, lamBodyRef = createParserForwardedToRef()
    lamBodyRef :=
      opt namePart .>>. pat .>>. choice [lamArrow >>. expr; ws >>. lamBody]
      |>> (fun ((np, pat), e) -> Lambda(np, pat, e))
    lamBody

  let lam lamArrow =
    str_ws "\\" >>. lamBody lamArrow

  let app = primary .>>. (ws >>. primary) |>> App

  let lamArrow = (str_ws "->" <|> str_ws ".")
  do
    primaryRef :=
      choice [
        ident |>> Var
        lam lamArrow
        parens
      ]

type Cmd =
| Assign of Var * Expr
| Eval of Var * Expr

[<AutoOpen>]
module CmdParser =
  let assignArrow, evalArrow = str_ws "=", str_ws ":="
  let assign = ident .>>. (assignArrow >>. expr <|> lamBody assignArrow)
  let eval = ident .>>. (evalArrow >>. expr)

  let cmd =
    choice [ attempt (assign |>> Assign)
             eval |>> Eval ]

do
  let makeApp = List.reduce (fun a b -> App(a, b))
  exprRef := many1 primary |>> makeApp

module Main =
  open System

  exception ParseFailure of string * string

  [<EntryPoint>]
  let main args =
    try
      let cmds =
        Seq.initInfinite (fun _ -> Console.ReadLine())
        |> Seq.zip (Seq.initInfinite id)
        |> Seq.takeWhile (fun (n, line) -> line <> null)
        |> Seq.filter (fun (n, line) -> line <> "")
        |> Seq.map (fun (n, line) ->
          match run cmd line with
          | Success(cmd, _, _) -> cmd
          | Failure(msg, _, _) -> raise <| ParseFailure(line, msg))
        |> Seq.cache

      let evals = Seq.choose (function Eval(v, e) -> Some(v, e) | _ -> None) cmds
      let assigns = Seq.choose (function Assign(v, e) -> Some(v, e) | _ -> None) cmds
      let vars = evals |> Seq.map fst
      let resExpr = evals |> Seq.map snd |> List.ofSeq |> Expr.Tuple

      let assembledExpr =
        Seq.foldBack (fun (v, e) innerExpr ->
         App(Lambda(Some(sprintf "Let_%s" v), Pat.Single v, innerExpr), setName v e))
            assigns resExpr
        |> doRewrite

      let aexpr = GenTypeLevel.Example.generate
                    (ref Set.empty, ref [], BoundVars []) assembledExpr

      let varsTuple = String.concat ", " vars
      printfn "let %s = !!(%s)" varsTuple aexpr
      printfn "printfn \"%s = %%A\" (%s)" varsTuple varsTuple
      0

    with
    | ParseFailure(line, msg) ->
      printfn "%s" line
      printfn "%s" msg
      1

