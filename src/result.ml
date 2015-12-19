open Or_errors.Std

module Impl = struct
  include Rresult.R
  let map ~f t = map f t
(*  let bind ~f t = bind f t*)
  let ignore t = map ~f:(fun _ -> ()) t
  let all t = List.fold_left (fun acc e -> map ~f:(fun l -> e::l) acc) (ok []) t
  let all_ignore t = List.fold_left (fun acc e -> map ~f:(fun _ -> ()) acc) (ok ()) t
  let both x y = 
    if 
      is_ok x 
    then 
        if 
          is_ok y 
        then 
          ok (get_ok x, get_ok y)
        else
          error @@ get_error y
    else
      error @@ get_error x
  let map_error ~f t = 
    if 
      is_ok t 
    then 
      ok @@ get_ok t
    else      
      error @@ f @@ get_error t      
  let (>|=) = (>>|)
  module Monad_infix = struct
    include Infix
    let (>|=) = (>|=)
  end

end

module Signature : RESULT =
  struct
    include Impl
  end



