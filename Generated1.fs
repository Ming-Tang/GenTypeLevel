module Generated
(* -----------
({\[Let_s](s, k, i, zero, succ).
    ({\[Let_add]add.
      ({\[Let_mult](mult, n0).
        ({\[Let_n1]n1.
          ({\[Let_n2]n2.
            ({\[Let_n3]n3.
              ({\[Let_n4]n4.
                ({\[Let_n5]n5.
                  ({\[Let_n6](n6, t).
                    ({\[Let_f]f.
                      ({\[Let_iszero](iszero, pred).
                        ({\[Let_minus](minus, y, test).
                          (pred (mult n3 n2) succ zero)}
                          ({\[minus]m n. (n pred m)},
                            {\[y]f.
                              ({\x. (f {\y. (x x y)})}
                                {\x. (f {\y. (x x y)})})},
                            {\[test](w, x) [A]x [B](y, z) w. (y (z w))}))}
                        ({\[iszero]n. (n (k f) t)},
                          {\[pred]n f x. (n {\g h. (h (g f))} (k x) i)}))}
                      {\[f]a b. b})}
                    ((succ n5), {\[t]a b. a}))}
                  (succ n4))}
                (succ n3))}
              (succ n2))}
            (succ n1))}
          (succ n0))}
        ({\[mult]a b. (a (add b) zero)}, zero))}
      {\[add]a b. (a succ b)})}
    ({\[s]x y z. (x z (y z))},
      {\[k]x y. x},
      {\[i]x. x},
      {\[zero]f x. x},
      {\[succ]n f x. (f (n f x))}))
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
  static member inline (!!) (T(a1, a2, a3, a4, a5)) = (!!a1, !!a2, !!a3, !!a4, !!a5)

type App<'a, 'b> when 'a :> Expr and 'b :> Expr = App of 'a * 'b with
  interface Expr
  static member inline (!!) (App(a, b)) = !!(!!a <|- !!b)

let inline (<<-) x y = App(x, y)
let inline (!) x = E x

type A<'a, 'b> = A of ('a -> 'b) with
  static member inline Apply(A f, x) = !(f x)

// END HEADER ---

type T_succ_2<'n, 'f> = T_succ_2 of 'n * 'f with
  static member inline Apply(T_succ_2(n, f), x) =
    (!f <<- ((!n <<- !f) <<- !x))

and T_succ_1<'n> = T_succ_1 of 'n with
  static member inline Apply(T_succ_1(n), f) =
    !T_succ_2(n, f)

and T_succ = T_succ with
  static member inline Apply(T_succ, n) =
    !T_succ_1(n)

and T_zero_1 = T_zero_1 with
  static member inline Apply(T_zero_1, x) =
    !x

and T_zero = T_zero with
  static member inline Apply(T_zero, f) =
    !T_zero_1

and T_i = T_i with
  static member inline Apply(T_i, x) =
    !x

and T_k_1<'x> = T_k_1 of 'x with
  static member inline Apply(T_k_1(x), y) =
    !x

and T_k = T_k with
  static member inline Apply(T_k, x) =
    !T_k_1(x)

and T_s_2<'x, 'y> = T_s_2 of 'x * 'y with
  static member inline Apply(T_s_2(x, y), z) =
    ((!x <<- !z) <<- (!y <<- !z))

and T_s_1<'x> = T_s_1 of 'x with
  static member inline Apply(T_s_1(x), y) =
    !T_s_2(x, y)

and T_s = T_s with
  static member inline Apply(T_s, x) =
    !T_s_1(x)

and T_add_1<'succ, 'a> = T_add_1 of 'succ * 'a with
  static member inline Apply(T_add_1(succ, a), b) =
    ((!a <<- !succ) <<- !b)

and T_add<'succ> = T_add of 'succ with
  static member inline Apply(T_add(succ), a) =
    !T_add_1(succ, a)

and T_mult_1<'zero, 'add, 'a> = T_mult_1 of 'zero * 'add * 'a with
  static member inline Apply(T_mult_1(zero, add, a), b) =
    ((!a <<- (!add <<- !b)) <<- !zero)

and T_mult<'zero, 'add> = T_mult of 'zero * 'add with
  static member inline Apply(T_mult(zero, add), a) =
    !T_mult_1(zero, add, a)

and T_t_1<'a> = T_t_1 of 'a with
  static member inline Apply(T_t_1(a), b) =
    !a

and T_t = T_t with
  static member inline Apply(T_t, a) =
    !T_t_1(a)

and T_f_1 = T_f_1 with
  static member inline Apply(T_f_1, b) =
    !b

and T_f = T_f with
  static member inline Apply(T_f, a) =
    !T_f_1

and T_pred_4<'f, 'g> = T_pred_4 of 'f * 'g with
  static member inline Apply(T_pred_4(f, g), h) =
    (!h <<- (!g <<- !f))

and T_pred_3<'f> = T_pred_3 of 'f with
  static member inline Apply(T_pred_3(f), g) =
    !T_pred_4(f, g)

and T_pred_2<'i, 'k, 'n, 'f> = T_pred_2 of 'i * 'k * 'n * 'f with
  static member inline Apply(T_pred_2(i, k, n, f), x) =
    (((!n <<- !T_pred_3(f)) <<- (!k <<- !x)) <<- !i)

and T_pred_1<'i, 'k, 'n> = T_pred_1 of 'i * 'k * 'n with
  static member inline Apply(T_pred_1(i, k, n), f) =
    !T_pred_2(i, k, n, f)

and T_pred<'i, 'k> = T_pred of 'i * 'k with
  static member inline Apply(T_pred(i, k), n) =
    !T_pred_1(i, k, n)

and T_iszero<'k, 't, 'f> = T_iszero of 'k * 't * 'f with
  static member inline Apply(T_iszero(k, t, f), n) =
    ((!n <<- (!k <<- !f)) <<- !t)

and B_1<'z, 'y> = B_1 of 'z * 'y with
  static member inline Apply(B_1(z, y), w) =
    (!y <<- (!z <<- !w))

and B = B with
  static member inline Apply(B, (y, z)) =
    !B_1(z, y)

and A = A with
  static member inline Apply(A, x) =
    !B

and T_test = T_test with
  static member inline Apply(T_test, (w, x)) =
    !A

and T_y_4<'x> = T_y_4 of 'x with
  static member inline Apply(T_y_4(x), y) =
    ((!x <<- !x) <<- !y)

and T_y_3<'f> = T_y_3 of 'f with
  static member inline Apply(T_y_3(f), x) =
    (!f <<- !T_y_4(x))

and T_y_2<'x> = T_y_2 of 'x with
  static member inline Apply(T_y_2(x), y) =
    ((!x <<- !x) <<- !y)

and T_y_1<'f> = T_y_1 of 'f with
  static member inline Apply(T_y_1(f), x) =
    (!f <<- !T_y_2(x))

and T_y = T_y with
  static member inline Apply(T_y, f) =
    (!T_y_1(f) <<- !T_y_3(f))

and T_minus_1<'pred, 'm> = T_minus_1 of 'pred * 'm with
  static member inline Apply(T_minus_1(pred, m), n) =
    ((!n <<- !pred) <<- !m)

and T_minus<'pred> = T_minus of 'pred with
  static member inline Apply(T_minus(pred), m) =
    !T_minus_1(pred, m)

and Let_minus<'succ, 'zero, 'mult, 'n2, 'n3, 'pred> = Let_minus of 'succ * 'zero * 'mult * 'n2 * 'n3 * 'pred with
  static member inline Apply(Let_minus(succ, zero, mult, n2, n3, pred), (minus, y, test)) =
    (((!pred <<- ((!mult <<- !n3) <<- !n2)) <<- !succ) <<- !zero)

and Let_iszero<'succ, 'zero, 'mult, 'n2, 'n3> = Let_iszero of 'succ * 'zero * 'mult * 'n2 * 'n3 with
  static member inline Apply(Let_iszero(succ, zero, mult, n2, n3), (iszero, pred)) =
    (!Let_minus(succ, zero, mult, n2, n3, pred) <<- T (!T_minus(pred), !T_y, !T_test))

and Let_f<'succ, 'zero, 'i, 'k, 'mult, 'n2, 'n3, 't> = Let_f of 'succ * 'zero * 'i * 'k * 'mult * 'n2 * 'n3 * 't with
  static member inline Apply(Let_f(succ, zero, i, k, mult, n2, n3, t), f) =
    (!Let_iszero(succ, zero, mult, n2, n3) <<- T (!T_iszero(k, t, f), !T_pred(i, k)))

and Let_n6<'succ, 'zero, 'i, 'k, 'mult, 'n2, 'n3> = Let_n6 of 'succ * 'zero * 'i * 'k * 'mult * 'n2 * 'n3 with
  static member inline Apply(Let_n6(succ, zero, i, k, mult, n2, n3), (n6, t)) =
    (!Let_f(succ, zero, i, k, mult, n2, n3, t) <<- !T_f)

and Let_n5<'succ, 'zero, 'i, 'k, 'mult, 'n2, 'n3> = Let_n5 of 'succ * 'zero * 'i * 'k * 'mult * 'n2 * 'n3 with
  static member inline Apply(Let_n5(succ, zero, i, k, mult, n2, n3), n5) =
    (!Let_n6(succ, zero, i, k, mult, n2, n3) <<- T ((!succ <<- !n5), !T_t))

and Let_n4<'succ, 'zero, 'i, 'k, 'mult, 'n2, 'n3> = Let_n4 of 'succ * 'zero * 'i * 'k * 'mult * 'n2 * 'n3 with
  static member inline Apply(Let_n4(succ, zero, i, k, mult, n2, n3), n4) =
    (!Let_n5(succ, zero, i, k, mult, n2, n3) <<- (!succ <<- !n4))

and Let_n3<'succ, 'zero, 'i, 'k, 'mult, 'n2> = Let_n3 of 'succ * 'zero * 'i * 'k * 'mult * 'n2 with
  static member inline Apply(Let_n3(succ, zero, i, k, mult, n2), n3) =
    (!Let_n4(succ, zero, i, k, mult, n2, n3) <<- (!succ <<- !n3))

and Let_n2<'succ, 'zero, 'i, 'k, 'mult> = Let_n2 of 'succ * 'zero * 'i * 'k * 'mult with
  static member inline Apply(Let_n2(succ, zero, i, k, mult), n2) =
    (!Let_n3(succ, zero, i, k, mult, n2) <<- (!succ <<- !n2))

and Let_n1<'succ, 'zero, 'i, 'k, 'mult> = Let_n1 of 'succ * 'zero * 'i * 'k * 'mult with
  static member inline Apply(Let_n1(succ, zero, i, k, mult), n1) =
    (!Let_n2(succ, zero, i, k, mult) <<- (!succ <<- !n1))

and Let_mult<'succ, 'zero, 'i, 'k> = Let_mult of 'succ * 'zero * 'i * 'k with
  static member inline Apply(Let_mult(succ, zero, i, k), (mult, n0)) =
    (!Let_n1(succ, zero, i, k, mult) <<- (!succ <<- !n0))

and Let_add<'succ, 'zero, 'i, 'k> = Let_add of 'succ * 'zero * 'i * 'k with
  static member inline Apply(Let_add(succ, zero, i, k), add) =
    (!Let_mult(succ, zero, i, k) <<- T (!T_mult(zero, add), !zero))

and Let_s = Let_s with
  static member inline Apply(Let_s, (s, k, i, zero, succ)) =
    (!Let_add(succ, zero, i, k) <<- !T_add(succ))


let result = !!((!Let_s <<- T (!T_s, !T_k, !T_i, !T_zero, !T_succ)))
printfn "%A" result
