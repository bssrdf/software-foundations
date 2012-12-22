type 'a option =
| Some of 'a
| None

type ('a, 'b) prod =
| Pair of 'a * 'b

val beq_nat : int -> int -> bool

type id =
  int
  (* singleton inductive, whose constructor was Id *)

val beq_id : id -> id -> bool

type 'a partial_map = id -> 'a option

val extend : 'a1 partial_map -> id -> 'a1 -> id -> 'a1 option

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

val normalization : tm -> ty -> has_type -> halts

val beq_ty : ty -> ty -> bool

val type_check : context -> tm -> ty option

val type_checking_sound : context -> tm -> ty -> has_type

