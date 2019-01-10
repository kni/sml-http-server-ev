structure HttpContentStream :
sig
  type InputContent
  val init: int -> InputContent
  val add:  InputContent -> string -> unit
  val done: InputContent -> TextIO.instream
end
=
struct

type input_size = int

datatype Input = InputString of string list ref | InputFile of (TextIO.instream * TextIO.outstream)

datatype InputContent = InputContent of input_size ref * Input ref

val maxSizeForString = 16 * 1024


fun initS () = InputString (ref [])

fun initF () =
  let
    val file = OS.FileSys.tmpName ()
    val i    = TextIO.openIn  file
    val out  = TextIO.openOut file
  in
    OS.FileSys.remove file;
    InputFile (i, out)
  end

fun init size = InputContent (ref 0, ref ( (if size <= maxSizeForString then initS else initF) ()))


fun add (InputContent (size_ref, input_ref)) s = (
  size_ref := (!size_ref) + String.size s;
  case !input_ref of
      InputString sl_ref => (
        sl_ref := s::(!sl_ref);
        if !size_ref > maxSizeForString
        then (
          case initF () of
               (inputContent as InputFile (_, out)) => (TextIO.output (out, String.concat (List.rev (!sl_ref))); input_ref := inputContent)
             | _ => ()
        )
        else ()
      )
    | InputFile (i, out) => TextIO.output (out, s)
)


fun done (InputContent (size_ref, input_ref)) =
  case !input_ref of
      InputString sl_ref => TextIO.openString (String.concat (List.rev (!sl_ref)))
    | InputFile (i, out) => (TextIO.closeOut out; i)



fun test () =
  let
    val ic  = init 3
    val _   = add ic "Hello, "
    val _   = add ic "Input Content.\n"
    val ics = done ic
  in
    print (TextIO.inputAll ics);
    TextIO.closeIn ics
  end

(* val _ = test () *)
end



structure HttpContent =
struct

structure CS = HttpContentStream


(* if return (SOME ics, buf), then content is readded fully *)
fun readContent (state, buf, SOME cl) : (TextIO.instream option * string) = (
    state := SOME (cl, CS.init cl);
    readContent (state, buf, NONE)
  )
  | readContent (state, buf, NONE) =
    let
      val s = String.size buf
      val (cl, ic) = valOf (!state)
    in
      if cl <= s
      then (
        CS.add ic (String.substring (buf, 0, cl));
        let
          val buf = String.substring (buf, cl, s - cl)
          val ics = CS.done ic
        in
          state := NONE;
          (SOME ics, buf)
        end
      )
      else (
        CS.add ic buf;
        state := SOME ((cl - s), ic);
        (NONE, "")
      )
    end


fun testReadContent chunks cl content tail =
  let
    val state = ref NONE

    fun doit []      = print "ERROR\n"
      | doit (x::xs) =
        let
          val r = readContent (state, x, if !state = NONE then SOME cl else NONE)
        in
          case r of
              (NONE,     buf) => doit xs
            | (SOME ics, buf) =>
                let
                  val inputContent = TextIO.inputAll ics
                in
                  TextIO.closeIn ics;
                  if inputContent = content andalso buf = tail then print "OK\n" else print "ERROR\n"
                end
        end
  in
    doit chunks
  end

(* val _ = testReadContent ["Hello, ", "Plack!XXX"] 13 "Hello, Plack!" "XXX" *)


exception HttpBadChunks

local

val maxChunkSizeEntity = 1024

(* return SOME (n, i), where n - is chunck size, i - shift to chunck begin *)
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



fun readChunkData buf n i ic =
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
       if String.size data = 0 then () else CS.add ic data;
       if c = #"\r"
       then (
         if s >= i + n + 2
         then (if String.sub (buf, i + n + 1) = #"\n" then (0, 0, String.substring (buf, i + n + 2, s - i - n - 2)) else raise HttpBadChunks )
         else (n, i, buf)
       )
       else if c = #"\n"
       then (0, 0, String.substring (buf, i + n + 1, s - i - n - 1))
       else if n = 0 then (0, 0, String.substring (buf, i, s - i)) (* it is delete "\r\n" after size *)
       else raise HttpBadChunks
     end
   else
     let
       val data = String.substring (buf, i, s - i)
     in
       if String.size data = 0 then () else CS.add ic data;
       ((n - s + i), 0, "")
     end
 end


in


fun readChunkes (state, buf) =
  let
    val (n, i, ic) = case !state of
                NONE =>
                  let
                    val ic = CS.init (String.size buf)
                  in
                    state := SOME (0, 0, ic);
                    (0, 0, ic)
                  end
              | SOME (n, i, ic) => (n, i, ic)


    fun doit ""  n i = (state := SOME (n, i, ic); (NONE, ""))
      | doit buf 0 i = (
      case getChunkSize buf of
          NONE => if String.size buf > maxChunkSizeEntity then raise HttpBadChunks else
            ( state := SOME (n, i, ic); (NONE, buf))
        | SOME (n, i) =>
          let
            val (n', i', buf) = readChunkData buf n i ic
          in
            if n = 0
            then (state := SOME (n', i', ic); (SOME (CS.done ic), buf))
            else doit buf n' i'
          end
      )
      | doit buf n i =
          let
            val (n', i', buf) = readChunkData buf n i ic
          in
            if n = 0
            then (state := SOME (n', i', ic); (SOME (CS.done ic), buf))
            else doit buf n' i'
          end
  in
    doit buf n i
  end


fun testReadChunkes () =
  let

    fun test chunks content tail =
      let

        val state = ref NONE

        fun doit _ []      = print "ERROR\n"
          | doit buf (x::xs) =
            let
              val r = readChunkes (state, buf ^ x)
            in
              case r of
                  (NONE,     buf) => doit buf xs
                | (SOME ics, buf) =>
                    let
                      val inputContent = TextIO.inputAll ics
                    in
                      TextIO.closeIn ics;
                      if inputContent = content andalso buf = tail then print "OK\n" else print "ERROR\n"
                    end
            end
      in
        doit "" chunks
      end

  in
    test ["0\r\n\r\n"] "" "";
    test ["5\r\n", "Hello\r\n", "0\r\n\r\n"] "Hello" "";
    test ["5\r\n", "Hell", "o\r\n", "0\r\n\r\n", ""] "Hello" "";
    test ["5 chunk-extension \r\n", "Hello\r\n", "0\r\n\r\n"] "Hello" "";
    test ["5\r\nHello\r\n", "A\r\n, Plack!\r\n\r\n", "0\r\n\r\n", "", ""] "Hello, Plack!\r\n" "";
    test ["5\r\nHello\r\nA\r\n, Plack!\r\n\r\n0\r\n\r\nXXXX\n"] "Hello, Plack!\r\n" "XXXX\n";
    ()
  end

end

(* val _ = testReadChunkes () *)
end
