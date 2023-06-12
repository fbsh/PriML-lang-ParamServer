use "./socket.sml";

(* Initialize a parameter. *)
val param = ref 0

(* Listen for connections on a socket. *)
fun listenOnSocket sock =
  let
    (* Bind the socket to an address and start listening. *)
    (* You would need to replace these with appropriate SML socket functions. *)
    val _ = bindSocket sock myAddress
    val _ = listenOnSocket sock queueSize

    fun acceptLoop () =
      (* Accept a new connection. *)
      let
        val (newSock, clientAddr) = accept sock
      in
        (* Handle the connection in a new thread. *)
        spawnThread (fn () => handleConnection newSock)

        (* Keep accepting new connections. *)
        acceptLoop ()
      end
  in
    acceptLoop ()
  end

(* Handle a connection from a client. *)
and handleConnection sock =
  let
    (* Receive an update from the client. *)
    val update = recvVec sock bufferSize

    (* Update the parameter. *)
    val _ = param := !param + update

    (* Send back the updated parameter. *)
    val _ = sendVec sock (Word8Vector.fromList (map Word8.fromInt (!param :: [])))
  in
    (* Close the connection. *)
    closeSocket sock
  end
