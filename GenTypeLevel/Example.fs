﻿module GenTypeLevel.Example
let Fold _ _ _ = failwith "For quotations only."
let C _ _ = failwith "For quotations only."
let E = null

let (!.) x = Const x
let (!) x = Var x
let ln n x y = Lambda(Some n, Single x, y)
let l x y = Lambda(None, Single x, y)
let lt xs y = Lambda(None, TuplePat xs, y)
let ltn n xs y = Lambda(Some n, TuplePat xs, y)
let (<<) x y = App(x, y)

let gs = setName "S_" <| fromQ <@ fun x y z -> x z (y z) @>
let gk = setName "K_" <| fromQ <@ fun x y -> x @>
let gi = setName "I_" <| fromQ <@ fun x -> x @>

let gsucc = setName "Succ" <| fromQ <@ fun n f x -> f (n f x) @>
let gmap = setName "Map1" <| fromQ <@ fun f xs -> Fold (fun x xs -> C (f x) xs) E xs @>
//printfn "%A" <@ fun f xs -> Fold (fun x xs -> C (f x) xs) E xs @>

let e10 =
  let qzero() = <@ fun f x -> x @>
  let qone() = <@ fun f x -> f x @>
  let qsucc() = <@ fun n f x -> f (n f x) @>
  let qadd() = <@ fun a b -> a %(qsucc()) b @>
  let qtrue() = <@ fun a b -> a @>
  let qfalse() = <@ fun a b -> b @>
  let qiszero() = <@ fun n -> n (fun _ -> %(qfalse())) %(qtrue()) @>
  let qpred() = <@ fun n f x -> n (fun g h -> h (g f)) (fun u -> x) (fun u -> u) @>
  let qminus() = <@ fun m n -> n %(qpred()) m @>

  fromQ
    <@
      let s x (y : _ -> obj) (z : obj) : obj = x z (y z)
      let k x (y : obj) : obj = x
      let i x : obj = x

      let zero = %(qzero())
      let succ = %(qsucc())
      let add = %(qadd())

      let n1 = succ zero
      let n2 = succ n1
      let n3 = succ n2
      let n4 = succ n3
      let n5 = succ n4
      let n6 = succ n5

      let t = %(qtrue())
      let f = %(qfalse())

      let iszero = %(qiszero())
      let pred = %(qpred())
      let minus = %(qminus())

      pred (
        (fun a b -> a (add b) zero)
            (unbox (box n5))
            (unbox (box n4)))
    @>

let e1 = rewrite [rewriteUncurry] e10

let typeDecls, expr =
  genExpr (ref Set.empty)
          e1
          (FreeVars [])
          "E"

printfn "module Generated"
printfn "(* -----------"
printfn "%s" <| showExpr e1
printfn "   ----------- *)"

let tupleImpls =
  tupleSizes e1
  |> Set.ofSeq
  |> Set.union (set [2;3])
  |> Seq.map genTupleImpl
  |> String.concat "\n"

printfn (header()) tupleImpls

let mutable prefix = "type"
for typeDecl in List.rev typeDecls do
  printfn "%s %s" prefix typeDecl.GeneratedCode
  printfn ""
  prefix <- "and"

printfn (footer()) expr

