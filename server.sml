structure HttpServer =
struct

datatype Env = Env of {
  requestMethod  : string,
  requestURI     : string,
  pathInfo       : string,
  queryString    : string,
  serverProtocol : string,
  headers        : (string * string) list
}


datatype Response =
    ResponseSimple  of   string * (string * string) list * string
  | ResponseDelayed of ((string * (string * string) list * string) -> unit) -> unit
  | ResponseStream  of ((string * (string * string) list) -> ((string -> unit) * (unit -> unit))) -> unit


datatype settings = Settings of {
  handler      : Env -> Response,
  port         : int,
  host         : string,
  accept_queue : int,
  workers      : int,
  max_requests : int,
  reuseport    : bool,
  logger       : string -> unit,
  timeout      : int
}


val chunksize = 64 * 1024
fun read socket = Byte.bytesToString(Socket.recvVec (socket, chunksize))
fun write socket text = Socket.sendVec (socket, Word8VectorSlice.full (Byte.stringToBytes text))

local

  fun doHeaders [] = ""
    | doHeaders headers = (String.concatWith "\r\n" (List.map (fn (a, b) => (a ^ ": " ^ b)) headers)) ^ "\r\n"

  fun doResponseSimple socket (code, headers, body) =
    let
      val contentLength = String.size body
    in
      write socket ("HTTP/1.1 " ^ code ^ "\r\n" ^
        (doHeaders headers) ^
        "Content-Length: " ^ (Int.toString contentLength) ^ "\r\n" ^
        "\r\n" ^
        body
      );
      ()
    end

in
  fun doResponse socket (ResponseSimple (code, headers, body)) = doResponseSimple socket (code, headers, body)
    | doResponse socket (ResponseDelayed f) = f (doResponseSimple socket)
    | doResponse socket (ResponseStream f) =
      let
        fun doWrite t = let val length = String.size t in write socket ((Int.fmt StringCvt.HEX length) ^ "\r\n" ^ t ^ "\r\n"); () end

        fun doClose () = (write socket "0\r\n\r\n" ; ())

        fun doit (code, headers) = (
            write socket ("HTTP/1.1 " ^ code ^ "\r\n" ^ (doHeaders headers) ^ "Transfer-Encoding: chunked\r\n" ^ "\r\n");
            (doWrite, doClose)
          )
      in
        f doit
      end
end


fun findPairValue _ [] = NONE
  | findPairValue x ((k,v)::ks) = if k = x then SOME v else findPairValue x ks


exception HttpBadRequest





fun run (Settings settings) =
  let

    val timeout = #timeout settings
    val logger  = #logger  settings

    fun handler socket =
      let

        fun readContent socket cl buf =
          let
            val s = String.size buf
          in
            if cl <= s
            then String.substring (buf, cl, s - cl)
            else (case read socket of "" => raise HttpBadRequest | buf => readContent socket (cl - s) buf)
          end


        fun readChunkes socket buf = HttpChunks.readChunkes read socket buf
          handle HttpChunks.HttpBadChunks => raise HttpBadRequest | exc => raise exc


        fun doit buf =
          case HttpHeaders.parse buf of
               NONE => (case read socket of "" => () | b => doit (buf ^ b))
             | SOME (method, uri, path, query, protocol, headers, buf) =>
                 let
                   val env = Env {
                     requestMethod  = method,
                     requestURI     = uri,
                     pathInfo       = path,
                     queryString    = query,
                     serverProtocol = protocol,
                     headers        = headers
                   }

                   val contentLength = findPairValue "content-length" headers
                   val buf = case contentLength of NONE => (print "contentLength is NONE\n"; buf) | SOME cl => (
                       print ("contentLength is " ^ cl ^ "\n");
                       case Int.fromString cl of NONE => raise HttpBadRequest | SOME cl => (
                       readContent socket cl buf
                     ))


                   val chunked = case findPairValue "transfer-encoding" headers of SOME "chunked" => true | SOME v => ( print ( "XXX " ^ v ^ "\n") ;false) | _ => false
                   val _ = print ("chunked " ^ (Bool.toString chunked) ^ "\n")
                   val buf = if chunked then readChunkes socket buf else buf

                   val _ = print ("buf size is " ^ (Int.toString (String.size buf)) ^ "\n") (* ToDo *)

                   val res = (#handler settings) env handle exc => ResponseSimple ("500", [], "Internal server error\r\n")
                 in
                   doResponse socket res;
                   doit buf
                 end

      in
        logger "HELLO, socket.";
        (doit "" handle
            HttpBadRequest => (doResponse socket (ResponseSimple ("400", [], "Bad Request\r\n")))
          | OS.SysErr (msg,  SOME ECONNRESET) => logger ("ERROR ECONNRESET: " ^ msg ^ "\n")
          | exc => raise exc);
        logger "BY, socket.";
        Socket.close socket
      end

  in
    logger "Start.";
    NetServer.run (NetServer.Settings {
      handler      = handler,
      port         = (#port         settings),
      host         = (#host         settings),
      accept_queue = (#accept_queue settings),
      workers      = (#workers      settings),
      max_requests = (#max_requests settings),
      reuseport    = (#reuseport    settings),
      logger       = logger
    })
  end

end
