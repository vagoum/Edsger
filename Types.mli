type typ = TYPE_none        (* no type (should not be used)       *)
         | TYPE_int         (* int                                *)
         | TYPE_byte        (* byte                               *)
         | TYPE_array of    (* array                              *)
             typ *          (*   element type                     *)
             int            (*   size of array, if known, or zero *)
         | TYPE_proc        (* proc (return type)                 *)
	 | TYPE_bool
	 | TYPE_char
	 | TYPE_double
	 | TYPE_void
	 | TYPE_pointer of typ
val sizeOfType : typ -> int
val equalType : typ -> typ -> bool
val print_type : typ -> unit
