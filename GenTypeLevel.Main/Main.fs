module GenTypeLevel.Main
open GenTypeLevel
open FParsec

[<AutoOpen>]
module ExprParser =
  let ws, ws1 = spaces, spaces1
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
| Eval of Expr
| Del of Var
| Gen
| Clear

[<AutoOpen>]
module CmdParser =
  let assignArrow = str_ws "=" <|> str_ws ":="
  let assign = ident .>>. (assignArrow >>. expr <|> lamBody assignArrow)
  let del = (str_ws ":del" >>. ident) |>> Del
  let gen = str_ws ":gen" |>> (fun _ -> Gen)
  let clear = str_ws ":clear" |>> (fun _ -> Clear)

  let cmd : Parser<_, _> =
    choice [ del
             gen
             clear
             attempt (assign |>> Assign)
             expr |>> Eval ]

do
  let makeApp = List.reduce (fun a b -> App(a, b))
  exprRef := many1 primary |>> makeApp

module Main =
  open System
  let readLine() =
    eprintf "> "
    Console.ReadLine()

  let nameGen = ref Set.empty
  let typeDecls : TypeDecl list ref = ref []
  let mutable expr1 = None

  let rec loop() =
    let line = readLine()
    if line.Trim() = "" then
      loop()
    elif line <> null then
      let res = line |> run CmdParser.cmd
      match res with
      | Failure(msg, e1, _) -> printfn "%s" line; printfn "%A" e1
      | Success(res, _, _) ->
        //printfn "%A" res
        match res with
        | Assign(v, expr) ->
          let typeDecls', _ =
            Lambda(Some v, Pat.Single "_", expr)
            |> genExpr nameGen (FreeVars []) "E"
          typeDecls := !typeDecls @ List.rev typeDecls'

        | Gen ->
          //printfn "%A" !typeDecls
          let res =
            Example.generate (nameGen, typeDecls, FreeVars []) expr1.Value

          printfn ""
          printfn "//let result = !!%s" res
          printfn "//printfn \"%%A\" result"

        | Eval e -> expr1 <- Some e

        | Clear ->
          expr1 <- None
          nameGen := Set.empty
          typeDecls := []

        | _ -> ()

      loop()

  loop()
