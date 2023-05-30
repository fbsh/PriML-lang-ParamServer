datatype color = Red | Black;
datatype color_data = NO_COLOR | COLOR of int;
datatype tree = Empty | Node of color * tree * int * color_data * tree;

val empty = Empty;

fun member (x, Empty) = NONE
  | member (x, Node (_, a, y, v, b)) =
      if x < y then member (x, a)
      else if x > y then member (x, b)
      else SOME v;

fun insert (x, z, Empty) = Node (Red, Empty, x, z, Empty)
  | insert (x, z, s as Node (color, a, y, v, b)) =
      if x < y then balance (color, insert (x, z, a), y, v, b)
      else if x > y then balance (color, a, y, v, insert (x, z, b))
      else Node (color, a, x, z, b)

and balance (Black, Node (Red, Node (Red, a, x, v, b), y, _, c), z, u, d) = Node (Red, Node (Black, a, x, v, b), y, u, Node (Black, c, z, u, d))
  | balance (Black, Node (Red, a, x, v, Node (Red, b, y, _, c)), z, u, d) = Node (Red, Node (Black, a, x, v, b), y, u, Node (Black, c, z, u, d))
  | balance (Black, a, x, v, Node (Red, Node (Red, b, y, _, c), z, u, d)) = Node (Red, Node (Black, a, x, v, b), y, u, Node (Black, c, z, u, d))
  | balance (Black, a, x, v, Node (Red, b, y, u, Node (Red, c, z, _, d))) = Node (Red, Node (Black, a, x, v, b), y, u, Node (Black, c, z, u, d))
  | balance (color, a, x, v, b) = Node (color, a, x, v, b);

fun colorGraph edges k =
  let
    fun adjacentNodes node = List.filter (fn (n1, n2) => n1 = node orelse n2 = node) edges

    fun colorNode (node, colors) =
      let
        val adjacentColors = List.map (fn (n1, n2) => 
          if n1 = node then member (n2, colors) else member (n1, colors)) (adjacentNodes node)

        fun findUnusedColor usedColors color =
          if color >= k then NONE
          else if List.exists (fn c => case c of
                NONE => false
              | SOME (COLOR c') => c' = color
              | SOME NO_COLOR => false) usedColors then
            findUnusedColor usedColors (color + 1)
          else SOME color

        val newColor = case findUnusedColor adjacentColors 0 of
              NONE => NONE
            | SOME color => SOME (COLOR color)
      in
        case newColor of
            NONE => colors
          | SOME color => insert (node, color, colors)
      end

    val nodes = List.foldl (fn ((n1, n2), ns) => n1 :: n2 :: ns) [] edges

    val colored = List.foldl colorNode empty nodes
  in
    fn node => member (node, colored)
  end;

val edges = [(1,2),(2,3),(3,4),(4,1),(1,5)];
val k = 3;

val colorFunction = colorGraph edges k;

val testNodes = [1,2,3,4,5];
val results = List.map colorFunction testNodes;

List.app (fn (node, color) => 
            (print ("Node " ^ Int.toString(node) ^ ": ");
             case color of
                 NONE => print "No color found\n"
               | SOME NO_COLOR => print "No color\n"
               | SOME (COLOR c) => print ("Color " ^ Int.toString(c) ^ "\n")
            )
          ) (ListPair.zip (testNodes, results));
