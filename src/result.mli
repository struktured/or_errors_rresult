open Or_errors.Std

include module type of Rresult.R
include RESULT with type ('ok, 'err) t := ('ok, 'err ) t
