open Or_errors.Std

module Result = Rresult_result.Impl

module Make(Error:Or_errors.Error.S) : Or_errors.Or_error.S
  with module Result = Result =
struct
  module Impl = Or_errors.Or_error.Showable.Make(Result)(Error)
      (struct
        type 'a t = ('a, Error.t) Result.t
        let fail x = Rresult.Error x 
      end)
  module Result = Result
  include (Impl : OR_ERROR with module Result := Result)
end

