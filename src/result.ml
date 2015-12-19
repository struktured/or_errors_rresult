open Or_errors.Std

module Impl = struct
  include Rresult.R
  let map t ~f = map t f
  let bind t f = (>>=) t f
  let ignore t = map ~f:(fun _ -> ()) t
  let all (t: ('a, 'b) t list) : ('a list, 'b) t = 
    let folder acc e = if is_ok e then
      map acc ~f:(fun l -> get_ok e :: l) else error @@ get_error e
    in
    List.fold_left folder (ok []) t
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
  let map_error t ~f = 
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
include Impl


