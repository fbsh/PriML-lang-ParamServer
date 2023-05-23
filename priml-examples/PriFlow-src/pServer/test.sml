open CML
fun prog() = let
  val c1: int chan = channel()
  val e1 = recvEvt(c1)
  val c2: int chan = channel()
  val e2 = recvEvt(c2)
in
  spawn(fn() => send(c1,100));
  spawn(fn() => send(c2,100));
        
  spawn(fn() =>
    let val savings = ref 1000
        val checking = ref 1000
        fun server() = (
          let val amount = select([e1,e2]) in
             savings := !savings - amount;
             checking := !checking + amount
          end;
          server())
    in
      server()
    end);
  print "main thread done"
end