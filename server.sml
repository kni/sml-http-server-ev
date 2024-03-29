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
  input           : TextIO.instream option,
  workerHookData  : 'c option,
  connectHookData : 'd option,
  ev : Ev.ev
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
  workerHook   : ((Ev.ev -> 'c) * ('c -> unit)) option,
  connectHook  : ((Ev.ev -> 'd) * ('d -> unit)) option,
  logger       : string -> unit,
  timeout      : Time.time option
}

val needStop = NetServer.needStop


val maxHeadersSize = 8 * 1024
val chunksize = 64 * 1024


local

  fun doHeaders [] = ""
    | doHeaders headers = (String.concatWith "\r\n" (List.map (fn (a, b) => (a ^ ": " ^ b)) headers)) ^ "\r\n"

  fun doResponseSimple stream persistent keepAliveHeader (code, headers, body) =
    let
      val res =
        "HTTP/1.1 " ^ code ^ "\r\n" ^
          (if keepAliveHeader then "Connection: keep-alive\r\n" else "") ^
          (doHeaders headers) ^
          "Content-Length: " ^ (Int.toString (String.size body)) ^ "\r\n" ^
          "\r\n" ^
           body
    in
      NetServer.write (stream, res);
      if persistent then () else NetServer.shutdown stream;
      true
    end

in
  fun doResponseHeaders stream persistent keepAliveHeader (code, headers) =
    let
      val res =
        "HTTP/1.1 " ^ code ^ "\r\n" ^
          (if keepAliveHeader then "Connection: keep-alive\r\n" else "") ^
          (doHeaders headers) ^
          "\r\n"
    in
      NetServer.write (stream, res);
      if persistent then () else NetServer.shutdown stream;
      true
    end

  fun doResponse stream persistent keepAliveHeader (ResponseSimple (code, headers, body)) = doResponseSimple stream persistent keepAliveHeader (code, headers, body)
    | doResponse stream persistent keepAliveHeader (ResponseDelayed f) = f (doResponseSimple stream persistent keepAliveHeader)
    | doResponse stream persistent keepAliveHeader (ResponseStream f) =
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


exception HttpBadRequest


fun run (Settings settings) =
  let

    val logger  = #logger  settings

    fun handler ev (workerHookData, connectHookData) stream =
      let

        datatype ('a, 'b) readState =
            ReadHeaders
          | ReadContent of (TextIO.instream option -> bool) * 'a ref
          | ReadChunkes of (TextIO.instream option -> bool) * 'b ref

        val readState = ref ReadHeaders


        fun readContent (f, state, buf, cl) =
            case HttpContent.readContent (state, buf, cl) of
                (NONE,     buf) => buf
              | (SOME ics, buf) => (
                  readState := ReadHeaders;
                  f (SOME ics);
                  TextIO.closeIn ics;
                  buf
                )

        fun readChunkes (f, state, buf) = (
          case HttpContent.readChunkes (state, buf) handle HttpContent.HttpBadChunks => raise HttpBadRequest | exc => raise exc of
               (NONE,     buf) => buf
             | (SOME ics, buf) => (
                 readState := ReadHeaders;
                 f (SOME ics);
                 TextIO.closeIn ics;
                 buf
               )
            )


        fun doRead (stream, buf) =
            case !readState of ReadHeaders => (
              case HttpHeaders.parse buf of
                   NONE => if String.size buf > maxHeadersSize
                           then (doResponse stream false false (ResponseSimple ("413", [], "Entity Too Large\r\n")); "")
                           else buf
                 | SOME (method, uri, path, query, protocol, headers, buf) =>
                     let
                       val (persistent, keepAliveHeader) = isPersistent protocol headers

                       fun callHandlerAnddoResponse (inputContent:TextIO.instream option) : bool =
                         let
                           val env = Env {
                             requestMethod   = method,
                             requestURI      = uri,
                             pathInfo        = path,
                             queryString     = query,
                             serverProtocol  = protocol,
                             headers         = headers,
                             input           = inputContent,
                             workerHookData  = workerHookData,
                             connectHookData = connectHookData,
                             ev              = ev
                           }
                         in
                           doResponse stream persistent keepAliveHeader ((#handler settings) env) handle exc => (
                               logger (exnMessage exc);
                               doResponse stream false false (ResponseSimple ("500", [], "Internal server error\r\n"))
                             )
                         end

                     val buf =
                       if method = "POST" orelse method = "PUT"
                       then (
                         if findPairValue "expect" headers = SOME "100-continue"
                         then doResponseHeaders stream persistent keepAliveHeader ("100 Continue", []) else true;

                         case findPairValue "content-length" headers of
                           SOME cl => (
                             case Int.fromString cl of
                                 NONE => (
                                   doResponse stream false false (ResponseSimple ("400", [], "Bad Request\r\n"));
                                   buf
                                 )
                               | SOME cl =>
                                 let
                                   val state = ref NONE
                                   val _ = readState := ReadContent (callHandlerAnddoResponse, state);
                                   val buf:string = readContent (callHandlerAnddoResponse, state, buf, SOME cl);
                                 in
                                   buf
                                 end
                           )
                         | NONE => (
                             if findPairValue "transfer-encoding" headers = SOME "chunked"
                             then
                               let
                                 val state = ref NONE
                                 val _ = readState := ReadChunkes (callHandlerAnddoResponse, state);
                                 val buf:string = readChunkes (callHandlerAnddoResponse, state, buf);
                               in
                                 buf
                               end
                             else
                               buf
                           )
                       )
                       else (
                         callHandlerAnddoResponse NONE;
                         buf
                       )
                     in
                       if buf = "" then "" else doRead (stream, buf)
                     end
              )
           | ReadContent (f, state) => readContent (f, state, buf, NONE)
           | ReadChunkes (f, state) => readChunkes (f, state, buf)


        fun readCb (stream, "")  = (logger "BY, stream (client closed socket)."; "")
          | readCb (stream, buf) = doRead (stream, buf) handle
              HttpBadRequest => (doResponse stream false false (ResponseSimple ("400", [], "Bad Request\r\n")); "")
            | exc => raise exc
      in
        logger "HELLO, socket.";
        NetServer.read (stream, readCb)
      end

  in
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
