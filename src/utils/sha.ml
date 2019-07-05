open Digestif.SHA512

let sha512 buf = to_hex (digest_string buf)
