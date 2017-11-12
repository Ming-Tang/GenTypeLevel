module Generated
(* -----------
(s k i test)
   ----------- *)

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
  static member inline (!!) (T(a1, a2)) = (!!a1, !!a2)
  static member inline (!!) (T(a1, a2, a3)) = (!!a1, !!a2, !!a3)

type App<'a, 'b> when 'a :> Expr and 'b :> Expr = App of 'a * 'b with
  interface Expr
  static member inline (!!) (App(a, b)) = !!(!!a <|- !!b)

let inline (<<-) x y = App(x, y)
let inline (!) x = E x

type A<'a, 'b> = A of ('a -> 'b) with
  static member inline Apply(A f, x) = !(f x)

// END HEADER ---

type T_s_3<'x, 'y> = T_s_3 of 'x * 'y with
  static member inline Apply(T_s_3(x, y), z) =
    ((!x <<- !z) <<- (!y <<- !z))

and T_s_2<'x> = T_s_2 of 'x with
  static member inline Apply(T_s_2(x), y) =
    !T_s_3(x, y)

and T_s_1 = T_s_1 with
  static member inline Apply(T_s_1, x) =
    !T_s_2(x)

and T_s = T_s with
  static member inline Apply(T_s, _) =
    !T_s_1

and T_k_2<'x> = T_k_2 of 'x with
  static member inline Apply(T_k_2(x), y) =
    !x

and T_k_1 = T_k_1 with
  static member inline Apply(T_k_1, x) =
    !T_k_2(x)

and T_k = T_k with
  static member inline Apply(T_k, _) =
    !T_k_1

and T_i_1 = T_i_1 with
  static member inline Apply(T_i_1, x) =
    !x

and T_i = T_i with
  static member inline Apply(T_i, _) =
    !T_i_1

and T_zero_2 = T_zero_2 with
  static member inline Apply(T_zero_2, x) =
    !x

and T_zero_1 = T_zero_1 with
  static member inline Apply(T_zero_1, f) =
    !T_zero_2

and T_zero = T_zero with
  static member inline Apply(T_zero, _) =
    !T_zero_1

and T_succ_3<'n, 'f> = T_succ_3 of 'n * 'f with
  static member inline Apply(T_succ_3(n, f), x) =
    (!f <<- ((!n <<- !f) <<- !x))

and T_succ_2<'n> = T_succ_2 of 'n with
  static member inline Apply(T_succ_2(n), f) =
    !T_succ_3(n, f)

and T_succ_1 = T_succ_1 with
  static member inline Apply(T_succ_1, n) =
    !T_succ_2(n)

and T_succ = T_succ with
  static member inline Apply(T_succ, _) =
    !T_succ_1

and T_add_2<'a> = T_add_2 of 'a with
  static member inline Apply(T_add_2(a), b) =
    ((!a <<- !succ) <<- !b)

and T_add_1 = T_add_1 with
  static member inline Apply(T_add_1, a) =
    !T_add_2(a)

and T_add = T_add with
  static member inline Apply(T_add, _) =
    !T_add_1

and T_n1 = T_n1 with
  static member inline Apply(T_n1, _) =
    (!succ <<- !zero)

and T_n2 = T_n2 with
  static member inline Apply(T_n2, _) =
    (!succ <<- !n1)

and T_n3 = T_n3 with
  static member inline Apply(T_n3, _) =
    (!succ <<- !n2)

and T_n4 = T_n4 with
  static member inline Apply(T_n4, _) =
    (!succ <<- !n3)

and T_n5 = T_n5 with
  static member inline Apply(T_n5, _) =
    (!succ <<- !n4)

and T_n6 = T_n6 with
  static member inline Apply(T_n6, _) =
    (!succ <<- !n5)

and T_true_2<'a> = T_true_2 of 'a with
  static member inline Apply(T_true_2(a), b) =
    !a

and T_true_1 = T_true_1 with
  static member inline Apply(T_true_1, a) =
    !T_true_2(a)

and T_true = T_true with
  static member inline Apply(T_true, _) =
    !T_true_1

and T_false_2 = T_false_2 with
  static member inline Apply(T_false_2, b) =
    !b

and T_false_1 = T_false_1 with
  static member inline Apply(T_false_1, a) =
    !T_false_2

and T_false = T_false with
  static member inline Apply(T_false, _) =
    !T_false_1

and T_iszero_1 = T_iszero_1 with
  static member inline Apply(T_iszero_1, n) =
    ((!n <<- (!k <<- !false)) <<- !true)

and T_iszero = T_iszero with
  static member inline Apply(T_iszero, _) =
    !T_iszero_1

and T_pred_5<'f, 'g> = T_pred_5 of 'f * 'g with
  static member inline Apply(T_pred_5(f, g), h) =
    (!h <<- (!g <<- !f))

and T_pred_4<'f> = T_pred_4 of 'f with
  static member inline Apply(T_pred_4(f), g) =
    !T_pred_5(f, g)

and T_pred_3<'n, 'f> = T_pred_3 of 'n * 'f with
  static member inline Apply(T_pred_3(n, f), x) =
    (((!n <<- !T_pred_4(f)) <<- (!k <<- !x)) <<- !i)

and T_pred_2<'n> = T_pred_2 of 'n with
  static member inline Apply(T_pred_2(n), f) =
    !T_pred_3(n, f)

and T_pred_1 = T_pred_1 with
  static member inline Apply(T_pred_1, n) =
    !T_pred_2(n)

and T_pred = T_pred with
  static member inline Apply(T_pred, _) =
    !T_pred_1

and T_minus_2<'m> = T_minus_2 of 'm with
  static member inline Apply(T_minus_2(m), n) =
    ((!n <<- !pred) <<- !m)

and T_minus_1 = T_minus_1 with
  static member inline Apply(T_minus_1, m) =
    !T_minus_2(m)

and T_minus = T_minus with
  static member inline Apply(T_minus, _) =
    !T_minus_1

and B_1<'z, 'y> = B_1 of 'z * 'y with
  static member inline Apply(B_1(z, y), w) =
    (!y <<- (!z <<- !w))

and B = B with
  static member inline Apply(B, (y, z)) =
    !B_1(z, y)

and A = A with
  static member inline Apply(A, x) =
    !B

and T_test_1 = T_test_1 with
  static member inline Apply(T_test_1, (w, x)) =
    !A

and T_test = T_test with
  static member inline Apply(T_test, _) =
    !T_test_1


//let result = !!(((!s <<- !k) <<- !i) <<- !test)
//printfn "%A" result
