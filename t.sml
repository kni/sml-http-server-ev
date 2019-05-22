(*
curl -D - 'http://localhost:5000/simple?a=b'
curl -D - 'http://localhost:5000/delayed?a=b'
curl -D - 'http://localhost:5000/stream?a=b'

curl -D - 'http://localhost:5000/simple?a=b' -: -D - 'http://localhost:5000/delayed?a=b' -: -D - 'http://localhost:5000/stream?a=b'

curl -H "Transfer-Encoding: chunked" -d 'a=b' -D - 'http://localhost:5000/simple'

curl -H "Transfer-Encoding: chunked" -d 'a=b' -D - 'http://localhost:5000/simple' -: -d 'a=b' -D - 'http://localhost:5000/stream'

curl --data-binary @/etc/services -H "Transfer-Encoding: chunked" -D - 'http://localhost:5000/stream'

curl --data-binary "a=b" -H "Transfer-Encoding: chunked" 'http://localhost:5000/simple' -: 'http://localhost:5000/simple' -: -d 'a=b' 'http://localhost:5000/stream'

curl -s --data-binary @/etc/services -H "Transfer-Encoding: chunked" -D - 'http://localhost:5000/show-data'
*)

fun logger msg = print ((Date.fmt "%Y-%m-%d %H:%M:%S" (Date.fromTimeUniv(Time.now()))) ^ "\t" ^ msg ^ "\n")



fun handler (HttpServer.Env env) =
  let
    val ev   = #ev env (* Use Ev for asynchronous. *)
    val uri  = #requestURI env
    val path = #pathInfo env
    val connectHookData = #connectHookData env
  in
    logger ("Request URI: " ^ uri);

    case connectHookData of NONE => () | SOME data => logger data;

    case path of
        "/simple"  => HttpServer.ResponseSimple ("200 OK", [], "Hello! Simple.\r\n")
      | "/delayed" => HttpServer.ResponseDelayed (fn responder => responder ("200 OK", [("a", "b")], "Hello! Delayed.\r\n"))
      | "/stream"  => HttpServer.ResponseStream (fn responder =>
          let
            (* ... *)
            val writer = responder ("200 OK", [("a", "b")])
          in
            writer "Hello!";
            writer " Stream.\r\n";
            writer ""
          end
        )
      | "/show-data" => HttpServer.ResponseSimple ("200 OK", [],
          case #input env of
              SOME inputStream => TextIO.inputAll inputStream ^ "\r\n"
            | NONE => "-\r\n"
          )
      | _ => HttpServer.ResponseSimple ("200 OK", [], "Hello!\r\n")
  end


val settings = HttpServer.Settings {
  handler        = handler,
  port           = 5000,
  host           = "*",
  acceptQueue    = 128,
  workers        = 4,    (* 0 - without workers *)
  maxRequests    = 1000, (* max requests per worker, 0 - without limit *)
  reuseport      = false,
  workerHook     = SOME ( (fn ev => logger "Worker init hook."),  (fn _  => logger "Worker cleanup hook.") ),
  connectHook    = SOME ( (fn ev => (logger "Connect init hook."; "It's connect hook data.")), (fn _  => logger "Connect cleanup hook.") ),
  logger         = logger,
  timeout        = SOME (Time.fromSeconds 180)
}


fun main () = (
    logger "Start.";
    HttpServer.run settings
  )
