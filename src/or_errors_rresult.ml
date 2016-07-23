module Std =
struct
  module Result = Rresult_result
  module Or_error = Rresult_or_error
  module Error = Or_errors.Std.Error
end
