type key = string

datatype tree = LEAF | TREE of tree * key * tree

val empty = LEAF

fun insert(key, LEAF)          = TREE (LEAF, key, LEAF)
  | insert(key, TREE(l, k, r)) = if key < k
                                   then TREE(insert(key, l), k, r)
                                 else if key>k
                                   then TREE(l, k, insert(key, r))
                                 else TREE(l, k, r);

fun member(key, LEAF)          = false
  | member(key, TREE(l, k, r)) = if key = k
                                   then true
                                 else if key < k
                                   then member(key, l)
                                 else member(key, r);

datatype 'a tree2 = LEAF | TREE of 'a tree2 * key * 'a * 'a tree2
exception NoValue

fun insert2(LEAF, key, value)             = TREE (LEAF, key, value, LEAF)
  | insert2(TREE(l, k, v, r), key, value) = if key < k
                                             then TREE(insert2(l, key, value), k, v, r)
                                           else if key>k
                                             then TREE(l, k, v, insert2(r, key, value))
                                           else TREE(l, key, value, r);

fun member2(key, LEAF)             = false
  | member2(key, TREE(l, k, _, r)) = if key = k
                                       then true
                                     else if key < k
                                       then member2(key, l)
                                     else member2(key, r);

fun lookup2(LEAF, _)                   = raise NoValue
  | lookup2(TREE(l, k, value, r), key) = if key = k
                                           then value
                                         else if key < k
                                           then lookup2(l, key)
                                         else lookup2(r, key);

val t = insert("t", empty);
val s = insert("s", t);
val p = insert("p", s);
val i = insert("i", p);
val p = insert("p", i);
val f = insert("f", p);
val b = insert("b", f);
val s = insert("s", b);
val t = insert("t", s);

(*
           t
          / \
         s
        / \
       p
      / \
     i
    / \
   f
  / \
 b
/ \
*)

val a = insert("a", empty);
val b = insert("b", a);
val c = insert("c", b);
val d = insert("d", c);
val e = insert("e", d);
val f = insert("f", e);
val g = insert("g", f);
val h = insert("h", g);
val i = insert("i", h);

(*
 a
/ \
  b
 / \
   c
  / \
    d
   / \
     e
    / \
      f
     / \
       g
      / \
        h
       / \
         i
        / \
*)
