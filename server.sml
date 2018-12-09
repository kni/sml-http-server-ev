structure HttpServer =
struct

datatype ('c, 'd) Env = Env of {
  requestMethod   : string,
  requestURI      : string,
  pathInfo        : string,
  queryString     : string,
  serverProtocol  : string,
  headers         : (string * string) list,
  workerHookData  : 'c option,
  connectHookData : 'd option
}


datatype Response =
    ResponseSimple  of   string * (string * string) list * string
  | ResponseDelayed of ((string * (string * string) list * string) -> unit) -> unit
  | ResponseStream  of ((string * (string * string) list) -> (string -> unit)) -> unit


datatype ('c, 'd) settings = Settings of {
  handler      : ('c, 'd) Env -> Response,
  port         : int,
  host         : string,
  acceptQueue  : int,
  workers      : int,
  maxRequests  : int,
  reuseport    : bool,
  workerHook   : ((unit -> 'c) * ('c -> unit)) option,
  connectHook  : ((unit -> 'd) * ('d -> unit)) option,
  logger       : string -> unit,
  timeout      : int
}


val chunksize = 64 * 1024

fun read socket = Byte.bytesToString(Socket.recvVec (socket, chunksize))

fun write socket text =
  let
    val data = Word8VectorSlice.full (Byte.stringToBytes text)

    fun doit data =
      let
        val n = Socket.sendVec (socket, data)
      in
        if n = Word8VectorSlice.length data then () else
        doit (Word8VectorSlice.subslice (data, n, NONE))
      end
  in
    doit data
  end


local

  fun doHeaders [] = ""
    | doHeaders headers = (String.concatWith "\r\n" (List.map (fn (a, b) => (a ^ ": " ^ b)) headers)) ^ "\r\n"

  fun doResponseSimple socket keepAliveHeader (code, headers, body) =
    let
      val contentLength = String.size body
    in
      if contentLength = 0
      then write socket ("HTTP/1.1 " ^ code ^ "\r\n" ^
          (if keepAliveHeader then "Connection: keep-alive\r\n" else "") ^
          (doHeaders headers) ^
          "\r\n"
        )
      else write socket ("HTTP/1.1 " ^ code ^ "\r\n" ^
          (if keepAliveHeader then "Connection: keep-alive\r\n" else "") ^
          (doHeaders headers) ^
          "Content-Length: " ^ (Int.toString contentLength) ^ "\r\n" ^
          "\r\n" ^
          body
        )
      ; ()
    end

in
  fun doResponse socket keepAliveHeader (ResponseSimple (code, headers, body)) = doResponseSimple socket keepAliveHeader (code, headers, body)
    | doResponse socket keepAliveHeader (ResponseDelayed f) = f (doResponseSimple socket keepAliveHeader)
    | doResponse socket keepAliveHeader (ResponseStream f) =
      let
        fun writer t =
          let
            val length = String.size t
         in
           if length = 0 then write socket "0\r\n\r\n" else
           write socket ((Int.fmt StringCvt.HEX length) ^ "\r\n" ^ t ^ "\r\n")
         end

        fun doit (code, headers) = (
            write socket ("HTTP/1.1 " ^ code ^ "\r\n" ^
              (if keepAliveHeader then "Connection: keep-alive\r\n" else "") ^
              (doHeaders headers) ^
              "Transfer-Encoding: chunked\r\n" ^
              "\r\n"
            );
            writer
          )
      in
        f doit
      end
end


fun findPairValue _ [] = NONE
  | findPairValue x ((k,v)::ks) = if k = x then SOME v else findPairValue x ks


fun isPersistent "HTTP/1.0" headers = (case findPairValue "connection" headers of SOME "keep-alive" => (true, true)   | _ => (false, false))
  | isPersistent protocol   headers = (case findPairValue "connection" headers of SOME "close"      => (false, false) | _ => (true, false))


exception HttpBadRequest





fun run (Settings settings) =
  let

    val timeout = #timeout settings
    val logger  = #logger  settings

    fun handler (workerHookData, connectHookData) socket =
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
                     requestMethod   = method,
                     requestURI      = uri,
                     pathInfo        = path,
                     queryString     = query,
                     serverProtocol  = protocol,
                     headers         = headers,
                     workerHookData  = workerHookData,
                     connectHookData = connectHookData
                   }

                   val (persistent, keepAliveHeader) = isPersistent protocol headers

                   val buf =
                     if method = "POST" orelse method = "PUT"
                     then (
                       if findPairValue "expect" headers = SOME "100-continue"
                       then doResponse socket keepAliveHeader (ResponseSimple ("100 Continue", [], "")) else ();

                       case findPairValue "content-length" headers of
                         SOME cl => (
                           case Int.fromString cl of
                               NONE => raise HttpBadRequest
                             | SOME cl => readContent socket cl buf
                         )
                       | NONE => (
                           if findPairValue "transfer-encoding" headers = SOME "chunked"
                           then readChunkes socket buf
                           else buf
                         )
                     )
                     else buf

                   val res = (#handler settings) env handle exc => ResponseSimple ("500", [], "Internal server error\r\n")
                 in
                   doResponse socket keepAliveHeader res;
                   if persistent then doit buf else ()
                 end


      in
        logger "HELLO, socket.";

        (doit "" handle
            HttpBadRequest => (doResponse socket false (ResponseSimple ("400", [], "Bad Request\r\n")))
          | OS.SysErr (msg,  SOME ECONNRESET) => logger ("ERROR ECONNRESET: " ^ msg ^ "\n")
          | exc => raise exc);
        logger "BY, socket."
      end

  in
    logger "Start.";
    NetServer.run (NetServer.Settings {
      handler      = handler,
      port         = (#port         settings),
      host         = (#host         settings),
      acceptQueue  = (#acceptQueue settings),
      workers      = (#workers      settings),
      maxRequests  = (#maxRequests settings),
      reuseport    = (#reuseport    settings),
      workerHook   = (#workerHook  settings),
      connectHook  = (#connectHook settings),
      logger       = logger
    })
  end

end
