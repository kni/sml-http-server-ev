structure HttpChunks =
struct

exception HttpBadChunks

local

fun getChunkSize t =
  let
    fun parser i getc strm =
      case getc strm of
          NONE => NONE
        | SOME (#"\r", strm) => parser (i + 1) getc strm
        | SOME (#"\n", strm) => SOME ((i + 1), strm)
        | SOME (c, strm)     => parser (i + 1) getc strm
  in
    case StringCvt.scanString (parser 0) t of NONE => NONE | SOME i =>
    case StringCvt.scanString (Int.scan StringCvt.HEX) t of NONE => NONE | SOME n => SOME (n, i)
  end



fun readChunkData read socket buf n i =
 let
   val s = String.size buf
 in
   if s >= i + n + 1
   then
     let
       val data = String.substring (buf, i, n)
       val c = String.sub (buf, i + n)
     in
       if c = #"\r"
       then (
         if s >= i + n + 2
         then (if String.sub (buf, i + n + 1) = #"\n" then String.substring (buf, i + n + 2, s - i - n - 2) else raise HttpBadChunks )
         else (case read socket of "" => raise HttpBadChunks | b => readChunkData read socket (buf ^ b) n i)
       )
       else if c = #"\n"
       then String.substring (buf, i + n + 1, s - i - n - 1)
       else if n = 0
       then String.substring (buf, i + n, s - i - n) (* it is delete "\r\n" after size *)
       else raise HttpBadChunks
     end
   else
     let
       val data = buf
     in
       case read socket of "" => raise HttpBadChunks | buf => readChunkData read socket buf (n - s) 0
     end
 end


in


fun readChunkes read socket buf =
  case getChunkSize buf of
      NONE => (case read socket of "" => raise HttpBadChunks | b => readChunkes read socket (buf ^ b))
    | SOME (n, i) =>
      let
        val buf = readChunkData read socket buf n i
      in
        if n = 0
        then buf
        else readChunkes read socket buf
      end

end

end

(*
val socket = "ToDo"
fun read socket = "ToDo"
val buf = "6\r\nHellow\r\nA\r\n, Plack!\r\n\r\n0\r\nXXXX\n"
val buf = HttpChunks.readChunkes read socket buf
val _ = print buf
*)
