structure HttpHeaders =
struct

fun getHeaders t =
  let
    fun parser getc strm =
      let
        fun return h hs = (String.implode (List.rev h))::hs

        fun parser_two i h hs strm j = 
           case getc strm of NONE => NONE
               | SOME (#"\r", strm) => (
                    case getc strm of NONE => NONE
                       | SOME (#"\n", strm) => SOME (((return h hs), i + 3 + j), strm)
                       | SOME (c, strm)     => parser_one (i + 3 + j) (c::h) hs strm
                 )
               | SOME (#"\n", strm) => SOME (((return h hs), i + 2 + j), strm)
               | SOME (c, strm)     => (* one header *) parser_one (i + 2 + j) [c] (return h hs) strm

        and parser_one i h hs strm = 
          case getc strm of NONE => NONE
             | SOME (#"\r", strm) => (
               case getc strm of NONE => NONE
                  | SOME (#"\n", strm) => parser_two i h hs strm 1
                  | SOME (c, strm) => parser_one (i + 2) (c::h) hs strm
               )
             | SOME (#"\n", strm) => parser_two i h hs strm 0
             | SOME (c, strm) => parser_one (i + 1) (c::h) hs strm
      in
        parser_one 0 [] [] strm
      end
  in
    case StringCvt.scanString parser t of NONE => NONE | SOME (hs, i) =>
      SOME (List.rev hs, String.substring (t, i, (String.size t) - i))
  end


fun parse t = case getHeaders t of NONE => NONE | SOME (hs, body) =>
  let
    val fl = hd hs
    val hs = tl hs

    val (method, uri, protocol) = case String.fields (fn c => c = #" ") fl of [m, u, p] => (m, u, p) | _ => ("", "", "")

    val chomp = Substring.dropr  (fn c => c = #"\r" orelse c = #"\n")
    val droplSpace = Substring.dropl (fn c => c = #" ")

    fun split by s =
      let
        val (f, t) = Substring.splitl (fn c => c <> by) (Substring.full s)
        val t      = Substring.dropl  (fn c => c = by)  t
      in
        (Substring.string f, Substring.string (droplSpace t))
      end

    val hs = List.map (fn h => let val (k, v) = split #":" h in ((String.map Char.toLower k), v) end) hs

    val (path, query) = split #"?" uri
  in
    SOME (method, uri, path, query, protocol, hs, body)
  end


fun test () =
let

val _ = if (getHeaders "GET / HTTP/1.1\nHost: server.com\nAccept: */*\n\nBODY\n") =
                SOME (["GET / HTTP/1.1", "Host: server.com", "Accept: */*"], "BODY\n") then print "OK\n" else print "ERROR\n"

val _ = if (getHeaders "GET / HTTP/1.1\r\nHost: server.com\r\nAccept: */*\r\n\r\nBODY\r\n") =
                SOME (["GET / HTTP/1.1", "Host: server.com", "Accept: */*"], "BODY\r\n") then print "OK\n" else print "ERROR\n"

val _ = if (getHeaders "GET / HTTP/1.1\nHost: server.com\nAccept: */*\r\n\nBODY\r\n") =
                SOME (["GET / HTTP/1.1", "Host: server.com", "Accept: */*"], "BODY\r\n") then print "OK\n" else print "ERROR\n"

val _ = if (getHeaders "GET / HTTP/1.1\r\nHost: server.com\r\nAccept: */*\r\n\r\nBODY\r\n") =
                SOME (["GET / HTTP/1.1", "Host: server.com", "Accept: */*"], "BODY\r\n") then print "OK\n" else print "ERROR\n"

val _ = if (getHeaders "GET / HTTP/1.1\r\nHost: server.com\r\nAccept: */*\r\n\r\n") =
                SOME (["GET / HTTP/1.1", "Host: server.com", "Accept: */*"], "") then print "OK\n" else print "ERROR\n"

val _ = if (getHeaders "GET / HTTP/1.1\r\nHost: server.com\r\nAccept: */*\r\n") =
                NONE then print "OK\n" else print "ERROR\n"

val _ = print "-----\n"

val t = "GET /foo HTTP/1.1\r\nHost: server.com\r\nAccept: */*\r\n\r\nBODY\r\n"
val r = parse t
val _ = if r = SOME ("GET", "/foo", "/foo", "", "HTTP/1.1", [("host", "server.com"), ("accept", "*/*")], "BODY\r\n") then print "OK\n" else print "ERROR\n"
val _ = print "-----\n"
val _ = case r of NONE => print "NONE\n" | SOME (method, uri, path, query, protocol, hs, body) => (
    print (method ^ " " ^ uri ^ " " ^ protocol ^ "\n");
    print (String.concatWith "\n" (List.map (fn (f,s) => f ^ ": " ^ s) hs));
    print ("\n-----\n" ^ body ^ "\n-----\n")
  )

in () end

(* val _ = test () *)

end
