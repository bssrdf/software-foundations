type 'a option =
| Some of 'a
| None

type ('a, 'b) prod =
| Pair of 'a * 'b

(** val beq_nat : int -> int -> bool **)

let rec beq_nat = ( = )

type id =
  int
  (* singleton inductive, whose constructor was Id *)

(** val beq_id : id -> id -> bool **)

let beq_id id1 id2 =
  beq_nat id1 id2

type 'a partial_map = id -> 'a option

(** val extend : 'a1 partial_map -> id -> 'a1 -> id -> 'a1 option **)

let extend gamma x t x' =
  if beq_id x x' then Some t else gamma x'

type ty =
| TBool
| TArrow of ty * ty

type tm =
| Tvar of id
| Tapp of tm * tm
| Tabs of id * ty * tm
| Ttrue
| Tfalse
| Tif of tm * tm * tm

type value =
| V_abs of id * ty * tm
| V_true
| V_false

type step =
| ST_AppAbs of id * ty * tm * tm * value
| ST_App1 of tm * tm * tm * step
| ST_App2 of tm * tm * tm * value * step
| ST_IfTrue of tm * tm
| ST_IfFalse of tm * tm
| ST_If of tm * tm * tm * tm * step

type ('x, 'r) multi =
| Multi_refl of 'x
| Multi_step of 'x * 'x * 'x * 'r * ('x, 'r) multi

type ('x, 'p) ex =
| Ex_intro of 'x * 'p

type context = ty partial_map

type has_type =
| T_Var of (id -> ty option) * id * ty
| T_Abs of ty partial_map * id * ty * ty * tm * has_type
| T_App of ty * ty * context * tm * tm * has_type * has_type
| T_True of context
| T_False of context
| T_If of context * tm * tm * tm * ty * has_type * has_type * has_type

type halts = (tm, ((tm, step) multi, value) prod) ex

(** val normalization : tm -> ty -> has_type -> halts **)

let normalization =
  failwith "AXIOM TO BE REALIZED"

(** val beq_ty : ty -> ty -> bool **)

let rec beq_ty t1 t2 =
  match t1 with
  | TBool ->
    (match t2 with
     | TBool -> true
     | TArrow (t, t0) -> false)
  | TArrow (t11, t12) ->
    (match t2 with
     | TBool -> false
     | TArrow (t21, t22) -> if beq_ty t11 t21 then beq_ty t12 t22 else false)

(** val type_check : context -> tm -> ty option **)

let rec type_check gamma = function
| Tvar x -> gamma x
| Tapp (t1, t2) ->
  (match type_check gamma t1 with
   | Some t0 ->
     (match t0 with
      | TBool -> None
      | TArrow (t11, t12) ->
        (match type_check gamma t2 with
         | Some t3 -> if beq_ty t11 t3 then Some t12 else None
         | None -> None))
   | None -> None)
| Tabs (x, t11, t12) ->
  (match type_check (extend gamma x t11) t12 with
   | Some t13 -> Some (TArrow (t11, t13))
   | None -> None)
| Tif (x, t0, f) ->
  (match type_check gamma x with
   | Some t1 ->
     (match t1 with
      | TBool ->
        (match type_check gamma t0 with
         | Some t2 ->
           (match type_check gamma f with
            | Some t3 -> if beq_ty t2 t3 then Some t2 else None
            | None -> None)
         | None -> None)
      | TArrow (t2, t3) -> None)
   | None -> None)
| _ -> Some TBool

(** val type_checking_sound : context -> tm -> ty -> has_type **)

let type_checking_sound =
  failwith "AXIOM TO BE REALIZED"

