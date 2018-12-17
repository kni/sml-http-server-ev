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



fun readChunkData needStop read timeout socket buf n i =
 let
   val s = String.size buf
   (* val _ = print ("n=" ^ (Int.toString n) ^ " i=" ^ (Int.toString i) ^ " s=" ^ (Int.toString s) ^ " buf: " ^ buf ^ "\n") *)
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
         else (case read timeout socket of "" => if needStop () then "" else raise HttpBadChunks | b => readChunkData needStop read timeout socket (buf ^ b) n i)
       )
       else if c = #"\n"
       then String.substring (buf, i + n + 1, s - i - n - 1)
       else if n = 0 then String.substring (buf, i, s - i) (* it is delete "\r\n" after size *)
       else raise HttpBadChunks
     end
   else
     let
       val data = buf
     in
       case read timeout socket of "" => if needStop () then "" else raise HttpBadChunks | buf => readChunkData needStop read timeout socket buf (n - s + i) 0
     end
 end


in


fun readChunkes needStop read timeout socket buf =
  case getChunkSize buf of
      NONE => (case read timeout socket of "" => if needStop () then "" else raise HttpBadChunks | b => readChunkes needStop read timeout socket (buf ^ b))
    | SOME (n, i) =>
      let
        val buf = readChunkData needStop read timeout socket buf n i
      in
        if n = 0
        then buf
        else readChunkes needStop read timeout socket buf
      end

end

end


fun test () =
  let

    fun needStop () = false

    val timeout = NONE

    fun test chunks tail =
      let
        val bufs = ref chunks
        val socket = "ToDo"

        fun read timeout socket =
          let
            val buf = hd (!bufs)
          in
            bufs := tl (!bufs);
            buf
          end

        val buf = HttpChunks.readChunkes needStop read timeout socket ""
      in
        if buf = tail then print "OK\n" else print "ERROR\n"
      end


  in
    test ["0\r\n\r\n"] "";
    test ["6\r\n", "Hellow\r\n", "0\r\n\r\n"] "";
    test ["6\r\n", "Hell", "ow\r\n", "0\r\n\r\n", ""] "";
    test ["6\r\nHellow\r\n", "A\r\n, Plack!\r\n\r\n", "0\r\n\r\n", "", ""] "";
    test ["6\r\nHellow\r\nA\r\n, Plack!\r\n\r\n0\r\n\r\nXXXX\n"] "XXXX\n";
    ()
  end

(* val _ = test () *)
