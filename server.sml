structure HttpServer =
struct

type ev = Ev.ev

datatype ('c, 'd) Env = Env of {
  requestMethod   : string,
  requestURI      : string,
  pathInfo        : string,
  queryString     : string,
  serverProtocol  : string,
  headers         : (string * string) list,
  workerHookData  : 'c option,
  connectHookData : 'd option,
  ev : ev
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
  workerHook   : ((ev -> 'c) * ('c -> unit)) option,
  connectHook  : ((ev -> 'd) * ('d -> unit)) option,
  logger       : string -> unit,
  timeout      : Time.time option
}

val needStop = NetServer.needStop


val chunksize = 64 * 1024


local

  fun doHeaders [] = ""
    | doHeaders headers = (String.concatWith "\r\n" (List.map (fn (a, b) => (a ^ ": " ^ b)) headers)) ^ "\r\n"

  (* ToDo rm timeout *)
  fun doResponseSimple timeout stream persistent keepAliveHeader (code, headers, body) =
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
      if persistent then () else NetServer.shutdown stream;
      true
    end

in
  fun doResponse timeout stream persistent keepAliveHeader (ResponseSimple (code, headers, body)) = doResponseSimple timeout stream persistent keepAliveHeader (code, headers, body)
    | doResponse timeout stream persistent keepAliveHeader (ResponseDelayed f) = f (doResponseSimple timeout stream persistent keepAliveHeader)
    | doResponse timeout stream persistent keepAliveHeader (ResponseStream f) =
      let
        fun writer t =
          let
            val length = String.size t
            val res = if length = 0
                      then "0\r\n\r\n"
                      else (Int.fmt StringCvt.HEX length) ^ "\r\n" ^ t ^ "\r\n"
         in
           NetServer.write (stream, res);
           if length = 0 then (if persistent then () else NetServer.shutdown stream) else ();
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


fun findConnectionHeader headers = case findPairValue "connection" headers of NONE => NONE | SOME v => SOME (String.map Char.toLower v)

fun isPersistent "HTTP/1.0" headers = (case findConnectionHeader headers of SOME "keep-alive" => (true, true)   | _ => (false, false))
  | isPersistent protocol   headers = (case findConnectionHeader headers of SOME "close"      => (false, false) | _ => (true, false))



fun run (Settings settings) =
  let

    val timeout = #timeout settings
    val logger  = #logger  settings

    fun handler ev (workerHookData, connectHookData) stream =
      let

        (*
        ToDo
        fun readContent socket cl buf = HttpContent.readContent needStop read timeout socket cl buf
          handle HttpContent.HttpBadContent => raise HttpBadRequest | exc => raise exc
        *)

        (*
        ToDo
        fun readChunkes socket buf = HttpContent.readChunkes needStop read timeout socket buf
          handle HttpContent.HttpBadChunks => raise HttpBadRequest | exc => raise exc
        *)

        datatype readState = ReadHeaders | ReadContent of int | ReadChunkes of int
        val readState = ref ReadHeaders

        fun doRead (stream, buf) =
            case !readState of ReadHeaders => (
              case HttpHeaders.parse buf of NONE => buf
                 | SOME (method, uri, path, query, protocol, headers, buf) =>
                     let
                       val (persistent, keepAliveHeader) = isPersistent protocol headers

                       fun callHandlerAnddoResponse () =
                         let
                           val env = Env {
                             requestMethod   = method,
                             requestURI      = uri,
                             pathInfo        = path,
                             queryString     = query,
                             serverProtocol  = protocol,
                             headers         = headers,
                             workerHookData  = workerHookData,
                             connectHookData = connectHookData,
                             ev              = ev
                           }
                         in
                           doResponse timeout stream persistent keepAliveHeader ((#handler settings) env) handle exc =>
                             doResponse timeout stream false false (ResponseSimple ("500", [], "Internal server error\r\n"))
                         end

                     in
                       if method = "POST" orelse method = "PUT"
                       then (
                         if findPairValue "expect" headers = SOME "100-continue"
                         then doResponse timeout stream persistent keepAliveHeader (ResponseSimple ("100 Continue", [], "")) else true;

                         case findPairValue "content-length" headers of
                           SOME cl => (
                             case Int.fromString cl of
                                 NONE => doResponse timeout stream false false (ResponseSimple ("400", [], "Bad Request\r\n"))
                               | SOME cl => (readState := ReadContent cl ; true)
                           )
                         | NONE => (
                             if findPairValue "transfer-encoding" headers = SOME "chunked"
                             then (readState := ReadChunkes 0; true)
                             else true
                           )
                       )
                       else callHandlerAnddoResponse ()
                       ;
                       if buf = "" then buf else doRead (stream, buf)
                     end
              )
           | ReadContent cl => (
               (* ToDo *)
               print "ReadContent\n";
               readState := ReadHeaders;
               doResponse timeout stream false false (ResponseSimple ("501", [], "Not Implemented \r\n"));
               buf
             )
           | ReadChunkes size => (
               (* ToDo *)
               print "ReadChunkes\n";
               readState := ReadHeaders;
               doResponse timeout stream false false (ResponseSimple ("501", [], "Not Implemented \r\n"));
               buf
             )


        fun readCb (stream, "")  = (logger "BY, stream (client closed socket)."; "")
          | readCb (stream, buf) = doRead (stream, buf)
      in
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
