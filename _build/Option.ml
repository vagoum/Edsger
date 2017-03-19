open Error
(*let may f x = match x with
| Some e -> ignore (f e) ;
| None -> ();;

let is_some x = match x with
| Some e-> true;
| None ->false;;

let map f x = match x with
|Some y -> Some (f y);
|None -> None;;
let get_some x= match x with
| Some y->y;
|None ->error "get_some";;
let get = get_some;;
let get_some1 x= match x with
| Some y->y;
|None ->error "get_some"; [];;a
*)
exception No_value



let may f = function

	| None -> ()

	| Some v -> f v



let map f = function

	| None -> None

	| Some v -> Some (f v)



let default v = function

	| None -> v

	| Some v -> v



let is_some = function

	| None -> false

	| _ -> true



let is_none = function

	| None -> true

	| _ -> false



let get = function

	| None -> raise No_value

	| Some v -> v



let map_default f v = function

	| None -> v

	| Some v2 -> f v2

let get_some = get;;
let get_some1 = get;;
