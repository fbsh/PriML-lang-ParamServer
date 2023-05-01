structure T = Timer

type point = real * real

datatype activation = ReLU | Sigmoid | Tanh

type weight = real

type neuron = weight list

type layer = neuron list

type network = layer list

fun mean (nums : real list) : real =
  let
    val sum = List.foldl (op +) 0.0 nums
    val length = real (List.length nums)
  in
    sum / length
  end

fun apply_activation (act: activation) (x: real) : real =
  case act of
      ReLU => if x > 0.0 then x else 0.0
    | Sigmoid => 1.0 / (1.0 + Math.exp (~x))
    | Tanh => Math.tanh x

fun apply_activation_deriv (act: activation) (x: real) : real =
  case act of
      ReLU => if x > 0.0 then 1.0 else 0.0
    | Sigmoid => let val s = apply_activation Sigmoid x in s * (1.0 - s) end
    | Tanh => let val t = apply_activation Tanh x in 1.0 - t * t end

fun forward_pass (net : network) (input : real list) (act: activation) : real list =
  let
    fun forward_layer (layer : layer) (input : real list) : real list =
      List.map (fn neuron => apply_activation act (List.foldl (op +) 0.0 (List.map2 (op *) input neuron))) layer
  in
    List.foldl (fn (layer, input) => forward_layer layer input) input net
  end

fun mean_squared_error (predicted : real list) (actual : real list) : real =
  mean (List.map2 (fn (p, a) => (p - a) * (p - a)) predicted actual)

fun backpropagation (net : network) (data : (real list * real list) list) (act: activation) (learning_rate : real) (iterations : int) : network =
  let
    fun update_weights (layer, deltas, inputs) =
      List.map2 (fn (neuron, delta) => List.map2 (fn (w, i) => w + learning_rate * delta * i) (neuron, inputs)) (layer, deltas)

    fun backward_layer (layer, next_deltas, act) =
      let
        val z = forward_pass layer (map (fn ((x, _), _) => x) data) act
        val act_deriv = List.map (apply_activation_deriv act) z
        val next_weights = List.concat (List.map (fn neuron => tl neuron) next_deltas)
      in
        List.map2 (op *) act_deriv (List.map2 (op *) z next_weights)
      end

    fun loop (net, 0) = net
      | loop (net, iter) =
        let
          val updated_net =
            List.foldl (fn (example, net) =>
                          let
                            val (input, target) = example
                            val output = forward_pass net input act
                            val output_error = List.map2 (op -) target output
                            val output_deltas = List.map2 (op *) (List.map (apply_activation_deriv act) output) output_error
                            val hidden_deltas = backward_layer (List.hd net) (output_deltas :: []) act
                            val new_output_layer = update_weights (List.hd (tl net), output_deltas, (forward_pass (List.hd net) input act) :: [])
                            val new_hidden_layer = update_weights (List.hd net, hidden_deltas, input :: [])
                          in
                            [new_hidden_layer, new_output_layer]
                          end
                        ) net data
        in
          loop (updated_net, iter - 1)
        end
  in
    loop (net, iterations)
  end


val data = [([1.0], [2.0]), ([2.0], [4.0]), ([3.0], [6.0])]

val net = [ [ [1.0] ], [ [1.0] ] ]

val act = ReLU

val learning_rate = 0.01

val iterations = 1000

val trained_net = backpropagation net data act learning_rate iterations

val input = [2.0]

val prediction = forward_pass trained_net input act

val () = print ("Prediction: " ^ Real.toString (List.hd prediction) ^ "\n")
