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
  | ResponseDelayed of ((string * (string * string) list * string) -> bool) -> bool
  | ResponseStream  of ((string * (string * string) list) -> (string -> bool)) -> bool


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
  timeout      : Time.time option
}

val needStop = NetServer.needStop


val chunksize = 64 * 1024


local

  fun doHeaders [] = ""
    | doHeaders headers = (String.concatWith "\r\n" (List.map (fn (a, b) => (a ^ ": " ^ b)) headers)) ^ "\r\n"

  (* ToDo rm timeout *)
  fun doResponseSimple timeout stream keepAliveHeader (code, headers, body) =
    let
      val contentLength = String.size body
      val res =
        if contentLength = 0
        then
          "HTTP/1.1 " ^ code ^ "\r\n" ^
            (if keepAliveHeader then "Connection: keep-alive\r\n" else "") ^
            (doHeaders headers) ^
            "\r\n"
        else
          "HTTP/1.1 " ^ code ^ "\r\n" ^
            (if keepAliveHeader then "Connection: keep-alive\r\n" else "") ^
            (doHeaders headers) ^
            "Content-Length: " ^ (Int.toString contentLength) ^ "\r\n" ^
            "\r\n" ^
             body
    in
      NetServer.write (stream, res);
      true
    end

in
  fun doResponse timeout stream keepAliveHeader (ResponseSimple (code, headers, body)) = doResponseSimple timeout stream keepAliveHeader (code, headers, body)
    | doResponse timeout stream keepAliveHeader (ResponseDelayed f) = f (doResponseSimple timeout stream keepAliveHeader)
    | doResponse timeout stream keepAliveHeader (ResponseStream f) =
      let
        fun writer t =
          let
            val length = String.size t
            val res = if length = 0
                      then "0\r\n\r\n"
                      else (Int.fmt StringCvt.HEX length) ^ "\r\n" ^ t ^ "\r\n"
         in
           NetServer.write (stream, res);
           true
         end

        fun doit (code, headers) = (
            NetServer.write (stream, ("HTTP/1.1 " ^ code ^ "\r\n" ^
              (if keepAliveHeader then "Connection: keep-alive\r\n" else "") ^
              (doHeaders headers) ^
              "Transfer-Encoding: chunked\r\n" ^
              "\r\n"
            ));
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

    fun handler (workerHookData, connectHookData) stream =
      let

        (*
        fun readContent socket cl buf =
          let
            val s = String.size buf
          in
            if cl <= s
            then String.substring (buf, cl, s - cl)
            else (case read timeout socket of "" => if needStop () then "" else raise HttpBadRequest | buf => readContent socket (cl - s) buf)
          end


        fun readChunkes socket buf = HttpChunks.readChunkes needStop read timeout socket buf
          handle HttpChunks.HttpBadChunks => raise HttpBadRequest | exc => raise exc
        *)

        fun doit (stream, buf) = 
            case HttpHeaders.parse buf of NONE => buf
               | SOME (method, uri, path, query, protocol, headers, buf) =>
                   (* ToDo *)
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

                     (*
                     ToDo For POST

                   val buf =
                     if method = "POST" orelse method = "PUT"
                     then (
                       if findPairValue "expect" headers = SOME "100-continue"
                       then doResponse timeout socket keepAliveHeader (ResponseSimple ("100 Continue", [], "")) else true;

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

                     *)

                     val res = (#handler settings) env handle exc => ResponseSimple ("500", [], "Internal server error\r\n")
                   in
                     doResponse timeout stream keepAliveHeader res;
                     (* ToDo if persistent ... *)
                     buf
                   end

        fun readCb (stream, "")  = (logger "BY, stream (client closed socket)."; "")
          | readCb (stream, buf) = (
            doit (stream, buf) handle
                HttpBadRequest => (doResponse timeout stream false (ResponseSimple ("400", [], "Bad Request\r\n")); "")
              | OS.SysErr (msg,  SOME ECONNRESET) => (logger ("ERROR ECONNRESET: " ^ msg ^ "\n"); "")
              | exc => raise exc
            )
      in
        (* logger "BY, socket." ToDo *)
        logger "HELLO, socket.";
        NetServer.read (stream, readCb)
      end

  in
    logger "Start.";
    NetServer.run (NetServer.Settings {
      handler      = handler,
      port         = (#port        settings),
      host         = (#host        settings),
      acceptQueue  = (#acceptQueue settings),
      workers      = (#workers     settings),
      maxRequests  = (#maxRequests settings),
      reuseport    = (#reuseport   settings),
      workerHook   = (#workerHook  settings),
      connectHook  = (#connectHook settings),
      logger       = logger
    })
  end

end
