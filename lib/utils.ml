
let remap_errors result =
  Result.map_error
    (function
      | `Exception -> `Msg "exception occured during request"
      | `Eof -> `Msg "encountered EOF during request"
      | `Toomuch -> `Msg "got too much during request"
      | `Port_already_allocated p ->
          `Msg ("Port already allocated: " ^ string_of_int p)
      | `Full -> `Msg "Couldn't allocate an IP: is full."
      | `Parse m -> `Msg ("ASN parse error: " ^ m))
    result
