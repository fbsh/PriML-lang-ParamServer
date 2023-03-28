structure SocketComm : sig
  val start_server : int -> (string -> unit)
  val send_message : string * int * string -> unit
end = struct
  open Socket

  fun start_server port on_message = let
    val server_addr = INetSock.fromAddr (INetSock.IN_ADDR_ANY, port)
    val server_sock = INetSock.TCP.socket ()
  in
    INetSock.bind (server_sock, server_addr);
    INetSock.listen (server_sock, 5);
    while true do
      let
        val (client_sock, _) = INetSock.accept server_sock
        val input = BinIO.openIn (SockIO.mkBinReader client_sock)
        val message = BinIO.inputLine input
      in
        on_message message;
        BinIO.closeIn input;
        OS.IO.close client_sock
      end
  end

  fun send_message (host, port, message) = let
    val client_sock = INetSock.TCP.socket ()
    val server_addr = INetSock.getAddr (host, port)
  in
    INetSock.connect (client_sock, server_addr);
    let
      val output = BinIO.openOut (SockIO.mkBinWriter client_sock)
    in
      BinIO.output (output, message ^ "\n");
      BinIO.closeOut output
    end;
    OS.IO.close client_sock
  end
end


val my_node_id = 0
val total_nodes = 2
val server_port = 9000

fun handle_message message = (
  print ("Received message: " ^ message ^ "\n")
)

val _ = if my_node_id = 0 then
  let
    val _ = SocketComm.start_server (server_port + my_node_id) handle_message
  in ()
  end
else
  let
    val message = "Hello from node " ^ Int.toString my_node_id
    val _ = SocketComm.send_message ("localhost", server_port, message)
  in ()
  end


fun serialize_point (x, y) = Real.toString x ^ "," ^ Real.toString y
fun deserialize_point str = let
  val [x, y] = String.tokens (fn c => c = #",") str
in
  (Real.fromString x, Real.fromString y)
end

fun serialize_gradient (dm, db) = Real.toString dm ^ "," ^ Real.toString db
fun deserialize_gradient str = let
  val [dm, db] = String.tokens (fn c => c = #",") str
in
  (Real.fromString dm, Real.fromString db)
end


fun handle_message message =
  let
    val (msg_type, data) = String.extract (message, 0, SOME 1), String.extract (message, 1, NONE)
  in
    case msg_type of
      "D" => (* Deserialize and store the received data point *)
    | "G" => (* Deserialize and update the model parameters (m, b) with the received gradient update *)
    | _ => print ("Unknown message type\n")
  end
