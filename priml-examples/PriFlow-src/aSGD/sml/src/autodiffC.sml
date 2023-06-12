open CML
datatype node =
    ConstNode of real
  | VarNode of string * real ref
  | UnaryNode of (real -> real) * (real -> real) * node
  | BinaryNode of (real * real -> real) * (real * real -> real * real) * node * node

fun constNode x = ConstNode x
fun varNode name x = VarNode (name, ref x)
fun unaryNode f g x = UnaryNode (f, g, x)
fun binaryNode f g x y = BinaryNode (f, g, x, y)

fun negNode x = unaryNode (fn a => ~a) (fn a => ~a) x
fun sinNode x = unaryNode Math.sin Math.cos x
fun cosNode x = unaryNode Math.cos (fn a => ~ (Math.sin a)) x
fun invNode x = unaryNode (fn a => 1.0/a) (fn a => ~ (1.0/(a*a))) x
fun addNode x y = binaryNode (op+) (fn (a, b) => (1.0, 1.0)) x y
fun subNode x y = binaryNode (op-) (fn (a, b) => (1.0, ~1.0)) x y
fun mulNode x y = binaryNode (op*) (fn (a, b) => (b, a)) x y

fun evalNode (ConstNode x) = x
  | evalNode (VarNode (_, x)) = !x
  | evalNode (UnaryNode (f, _, x)) = f (evalNode x)
  | evalNode (BinaryNode (f, _, x, y)) = f (evalNode x, evalNode y)

fun backpropNode (ConstNode _) _ = ()
  | backpropNode (VarNode (_, x)) dx = x := !x + dx
  | backpropNode (UnaryNode (_, g, x)) dx =
    let
      val gx = g (evalNode x)
    in
      backpropNode x (dx * gx)
    end
  | backpropNode (BinaryNode (_, g, x, y)) dx =
    let
      val (gx, gy) = g (evalNode x, evalNode y)
    in
      backpropNode x (dx * gx);
      backpropNode y (dx * gy)
    end

fun parallelEvalNodes nodes =
  let
    fun spawnEval node =
      let
        val mbox = CML.mailbox ()
        val _ = CML.spawn (fn () => CML.send (mbox, evalNode node))
      in
        mbox
      end
    val mboxes = List.map spawnEval nodes
    fun collectResults [] = []
      | collectResults (mbox::rest) = CML.recv mbox :: collectResults rest
  in
    collectResults mboxes
  end

fun parallelBackpropNodes nodes dxs =
  let
    fun spawnBackprop (node, dx) =
      CML.spawn (fn () => backpropNode node dx)
    val _ = List.map spawnBackprop (ListPair.zip (nodes, dxs))
  in
    ()
  end
val x = varNode "x" 1.0
val y = varNode "y" 2.0
val sin_x = sinNode x
val x_mul_y = mulNode x y
val f = addNode sin_x x_mul_y



val f_val = evalNode f
val f_str = Real.toString f_val
val _ = print ("Result of f: " ^ f_str ^ "\n")

val _ = backpropNode f 1.0
fun getVal (VarNode (_, v)) = !v

val df_dx = getVal x
val df_dy = getVal y

val _ = print ("df/dx = " ^ Real.toString df_dx ^ "\n")
val _ = print ("df/dy = " ^ Real.toString df_dy ^ "\n")

