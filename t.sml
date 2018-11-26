(*
curl -D - 'http://localhost:5000/simple?a=b'
curl -D - 'http://localhost:5000/delayed?a=b'
curl -D - 'http://localhost:5000/stream?a=b'

curl -H "Transfer-Encoding: chunked" -d 'a=b' -D - 'http://localhost:5000/simple'

curl -H "Transfer-Encoding: chunked" -d 'a=b' -D - 'http://localhost:5000/simple' -: -d 'a=b' -D - 'http://localhost:5000/stream'

curl --data-binary @/etc/services -H "Transfer-Encoding: chunked" -D - 'http://localhost:5000/stream'

curl --data-binary "a=b" -H "Transfer-Encoding: chunked" 'http://localhost:5000/simple' -: 'http://localhost:5000/simple' -: -d 'a=b' 'http://localhost:5000/stream'

*)

fun logger msg = print ((Date.fmt "%Y-%m-%d %H:%M:%S" (Date.fromTimeUniv(Time.now()))) ^ "\t" ^ msg ^ "\n")



fun handler (HttpServer.Env env) =
  let
    val uri  = #requestURI env
    val path = #pathInfo env
  in
    logger ("Request URI: " ^ uri);

    case path of
        "/simple"  => HttpServer.ResponseSimple ("200 OK", [], "Hello! Simple.\r\n")
      | "/delayed" => HttpServer.ResponseDelayed (fn responder => responder ("200 OK", [("a", "b")], "Hello! Delayed.\r\n"))
      | "/stream"  => HttpServer.ResponseStream (fn responder =>
          let
            (* ... *)
            val (write, close) = responder ("200 OK", [("a", "b")])
          in
            write "Hello!";
            write " Stream.\r\n";
            close ()
          end
        )
      | _ => HttpServer.ResponseSimple ("200 OK", [], "Hello!\r\n")
  end


val settings = HttpServer.Settings {
  handler      = handler,
  port         = 5000,
  host         = "*",
  accept_queue = 10,
  workers      = 0,
  max_requests = 1000, (* ToDo *)
  reuseport    = false,
  logger       = logger,
  timeout      = 3 (* ToDo *)
}

fun main () = HttpServer.run settings
