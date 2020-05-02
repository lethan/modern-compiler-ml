load "Int";

type id = string

datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm
             | AssignStm of id * exp
             | PrintStm of exp list

     and exp = IdExp of id
             | NumExp of int
             | OpExp of exp * binop * exp
             | EseqExp of stm * exp

(* a := 5 + 3 ; b:= ( print ( a, a - 1) , 10 * a ) ; print ( b ) *)
val prog =
  CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
    CompoundStm(AssignStm("b",
      EseqExp(PrintStm[IdExp"a",OpExp(IdExp"a", Minus,NumExp 1)],
        OpExp(NumExp 10, Times, IdExp"a"))),
   PrintStm[IdExp "b"]))

fun maxargs (CompoundStm(left, right)) = Int.max(maxargs left, maxargs right)
  | maxargs (AssignStm(_, exp))        = expargs exp
  | maxargs (PrintStm(exps))           = Int.max(length exps, 0)

and expargs (OpExp(left, _, right))    = Int.max(expargs left, expargs right)
  | expargs (EseqExp(stm, exp))        = Int.max(maxargs stm, expargs exp)
  | expargs _                          = 0

and explist []                         = 0
  | explist (exp::l)                   = Int.max(expargs exp, explist l);


fun lookup ([], _)                = 0
  | lookup ((var, value)::xs, id) = if var = id then value else lookup(xs, id);

fun update (xs, id, value) = (id, value) :: xs;

fun operation Plus  = op+
  | operation Minus = op-
  | operation Times = op*
  | operation Div   = fn (a, b) => a div b;

fun interpExp ((NumExp value), assigns)           = (value, assigns)
  | interpExp ((IdExp id), assigns)               = (lookup(assigns, id), assigns)
  | interpExp (OpExp(left, opr, right), assigns)  = let
                                                      val (left_value, left_assigns) = interpExp(left, assigns)
                                                      val (right_value, right_assigns) = interpExp(right, left_assigns)
                                                    in (operation(opr)(left_value, right_value), right_assigns) end
  | interpExp (EseqExp(stm, exp), assigns)        = let
                                                      val new_assigns = interpStm(stm, assigns)
                                                    in interpExp(exp, new_assigns) end

and interpStm (AssignStm(id, exp), assigns)       = let
                                                      val (value, new_assigns) = interpExp(exp, assigns)
                                                    in update(new_assigns, id, value) end
  | interpStm (CompoundStm(left, right), assigns) = let
                                                      val new_assigns = interpStm(left, assigns)
                                                    in interpStm(right, new_assigns) end
  | interpStm (PrintStm([]), assigns)             = (print "\n"; assigns)
  | interpStm (PrintStm(exp::exps), assigns)      = let
                                                      val (value, new_assigns) = interpExp(exp, assigns)
                                                    in (print (Int.toString(value) ^ " "); interpStm(PrintStm(exps), new_assigns)) end;

fun interp stm = let val _ = interpStm(stm, []) in () end;
