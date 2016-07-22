open Or_errors.Std
module Impl = struct
  include Rresult.R
  let map t ~f = map f t
  let bind t f = (>>=) t f
  let ignore t = map t ~f:(fun _ -> ())
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

module type S = 
sig
  include module type of Rresult.R
  include RESULT with type ('ok, 'err) t := ('ok, 'err ) t
end

module Signature : RESULT =
  struct
    include Impl
  end

