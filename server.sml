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


exception HttpBadRequest

fun run (Settings settings) =
  let


    val timeout = #timeout settings
    val logger  = #logger  settings

    fun handler socket =
      let

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

                   val res = (#handler settings) env handle exc => ResponseSimple ("500", [], "Internal server error\r\n")
                 in
                   doResponse socket res;
                   doit buf
                 end

        (*
        val serverAddr = Socket.Ctl.getSockName socket
        val remoteAddr = Socket.Ctl.getPeerName socket
        val (s_in_addr, s_port) = INetSock.fromAddr serverAddr
        val (r_in_addr, r_port) = INetSock.fromAddr remoteAddr
        val s_host = NetHostDB.toString s_in_addr;
        val r_host = NetHostDB.toString r_in_addr;
        val _ = print ("S: " ^ s_host ^ " " ^ (Int.toString s_port) ^ "\n")
        val _ = print ("R: " ^ r_host ^ " " ^ (Int.toString r_port) ^ "\n")
        *)
      in
        logger "HELLO, socket.";
        doit "" handle HttpBadRequest => doResponse socket (ResponseSimple ("400", [], "Bad Request\r\n")) | exc => raise exc;
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
