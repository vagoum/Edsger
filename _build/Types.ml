type typ = TYPE_none
         | TYPE_int
         | TYPE_byte
         | TYPE_array of
             typ *
             int
         | TYPE_proc
         | TYPE_bool
         | TYPE_char
         | TYPE_double
         | TYPE_void
         | TYPE_pointer of typ

let rec sizeOfType t =
   match t with
   | TYPE_int            -> 2
   | TYPE_byte | TYPE_bool | TYPE_char           -> 1
   | TYPE_array (et, sz) -> sz * sizeOfType et
   | TYPE_double -> 10
   | _                   -> 0

let rec equalType t1 t2 =
   match t1, t2 with
   | TYPE_array (et1, sz1), TYPE_array (et2, sz2) -> equalType et1 et2
   | (TYPE_pointer a),TYPE_array (b,_) -> equalType a b
   | TYPE_array (b,_),TYPE_pointer a -> equalType a b
   | TYPE_array (b,_), a -> equalType a b
   | TYPE_pointer b, a -> equalType a b
   | a,TYPE_pointer b -> equalType a b
   | _                                            -> t1 = t2
let rec print_type x= match x with 
| TYPE_array _ -> Printf.printf "This is array\n";()
| TYPE_double -> Printf.printf "This is dobule\n";()
| TYPE_char -> Printf.printf "This is char\n";()
| TYPE_int -> Printf.printf "This is int\n";()
| TYPE_bool -> Printf.printf "This is bool\n";()
| TYPE_pointer x1-> Printf.printf "This is a pointer of \n"; print_type x1;()
| TYPE_none -> Printf.printf "This is none type \n"
