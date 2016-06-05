open Error
let may f x = match x with
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
|None ->error "get_some"; -1;;
