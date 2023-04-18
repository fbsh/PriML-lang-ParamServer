structure Sock = Sockets.OS

fun setup_master_node host port : (Sockets.inet_addr, Sockets.SOCK_STREAM) Sock.sock = 
  let
    val master_sock = Sock.socket (Sockets.AF_INET, Sockets.SOCK_STREAM, Sockets.IPPROTO_TCP)
    val addr = Sock.inet_addr host
    val sockaddr = Sock.SA (Sock.sockaddr_in (addr, port))
  in
    Sock.bind (master_sock, sockaddr)
    Sock.listen (master_sock, 5)
    master_sock
  end

fun setup_worker_node master_host master_port : (Sockets.inet_addr, Sockets.SOCK_STREAM) Sock.sock = 
  let
    val worker_sock = Sock.socket (Sockets.AF_INET, Sockets.SOCK_STREAM, Sockets.IPPROTO_TCP)
    val master_addr = Sock.inet_addr master_host
    val master_sockaddr = Sock.SA (Sock.sockaddr_in (master_addr, master_port))
  in
    Sock.connect (worker_sock, master_sockaddr)
    worker_sock
  end

fun accept_connection (master_sock : (Sockets.inet_addr, Sockets.SOCK_STREAM) Sock.sock) =
  let
    val (client_sock, client_addr) = Sock.accept master_sock
  in
    (client_sock, client_addr)
  end

fun send_data (sock : (Sockets.inet_addr, Sockets.SOCK_STREAM) Sock.sock, data : Word8Vector.vector) : unit =
  let
    val bytes_sent = Sock.sendVec (sock, data)
  in
    if bytes_sent = Word8Vector.length data then
      ()
    else
      raise Fail "Failed to send all data"
  end

fun receive_data (sock : (Sockets.inet_addr, Sockets.SOCK_STREAM) Sock.sock, buffer_size : int) : Word8Vector.vector =
  Sock.recvVec (sock, buffer_size)
