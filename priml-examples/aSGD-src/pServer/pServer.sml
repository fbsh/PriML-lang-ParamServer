
structure ParameterServer =
struct
  type bias_seq = real list list
  type weight_seq = real list list list
  type parameter_tuple = bias_seq * weight_seq

  (* Custom map2 function *)
  fun map2 _ [] [] = []
    | map2 f (x::xs) (y::ys) = f x y :: map2 f xs ys
    | map2 _ _ _ = raise ListPair.UnequalLengths

  (* Fetches the biases and weights *)
  fun fetch_parameters (biases: bias_seq, weights: weight_seq): parameter_tuple = (biases, weights)

  (* Updates the biases and weights using the gradients *)
  fun push_gradient ((biases, weights): parameter_tuple, (nabla_biases, nabla_weights): parameter_tuple, learning_rate: real, mini_batch_size: real): parameter_tuple =
    let
      fun update_vector (v1: real list, v2: real list): real list =
        ListPair.map (fn (x, y) => x - learning_rate / mini_batch_size * y) (v1, v2)

      fun update_matrix (m1: real list list, m2: real list list): real list list =
        ListPair.map update_vector (m1, m2)

      val new_biases = ListPair.map update_vector (biases, nabla_biases)
      val new_weights = ListPair.map update_matrix (weights, nabla_weights)
    in
      (new_biases, new_weights)
    end

  (* Generates a random real number *)
  fun randReal (r: Random.rand): real = Real.fromInt (Random.randRange (0, 100000) r) / 100000.0

  (* Creates a new parameter server with the given dimensions, learning rate, and mini-batch size *)
  fun create (dimensions: int list, learning_rate: real, mini_batch_size: int): parameter_tuple =
    let
      val random = Random.rand (0, 0)
      val biases = List.map (fn x => List.tabulate (x, fn _ => randReal random)) (List.tl dimensions)
      val weights = map2 (fn x => fn y => List.tabulate (y, fn _ => List.tabulate (x, fn _ => randReal random))) (List.take (dimensions, List.length dimensions - 1)) (List.drop (dimensions, 1))
    in
      (biases, weights)
    end
end

structure RealMatrix = struct
  type matrix = real list list

  (* Converts a list of lists of reals into a matrix *)
  fun fromListMatrix (mat: matrix): matrix = mat

  (* Prints a matrix with each row on a new line and values separated by commas *)
  fun printMatrix (mat: matrix): unit =
    let
      fun print_row row = print (String.concatWith "," (List.map Real.toString row) ^ "\n")
    in
      List.app print_row mat
    end
  
  fun printBiasSeq (matrices: ParameterServer.bias_seq) =
    let
      fun print_each [] = ()
        | print_each (m::ms) = (printMatrix m; print "\n"; print_each ms)
      fun print_row row = print (String.concatWith "," (List.map Real.toString row) ^ "\n")
      fun print_each_bias [] = ()
        | print_each_bias (m::ms) = (print_row m; print "\n"; print_each_bias ms)
    in
      print_each_bias matrices
    end

  fun printWeightSeq (matrices: ParameterServer.weight_seq) =
    let
      fun print_each [] = ()
        | print_each (m::ms) = (printMatrix m; print "\n"; print_each ms)
    in
      print_each matrices
    end
end


(* Define the dimensions of the neural network layers *)
val dimensions = [2, 3, 1]

(* Set the learning rate and mini-batch size *)
val learning_rate = 0.1
val mini_batch_size = 10

(* Create a parameter server *)
val parameter_server = ParameterServer.create (dimensions, learning_rate, mini_batch_size)

(* Fetch the initial biases and weights from the parameter server *)
val (biases, weights) = ParameterServer.fetch_parameters parameter_server


(* Define some dummy gradients for the biases and weights *)
val nabla_biases = [ [0.1, 0.2, 0.3], [0.4] ]
val nabla_weights = [ [ [0.01, 0.02], [0.03, 0.04], [0.05, 0.06] ], [ [0.07, 0.08, 0.09] ] ]

(* Update the parameter server with the gradients *)
val updated_parameter_server = ParameterServer.push_gradient (parameter_server, (nabla_biases, nabla_weights), learning_rate, real mini_batch_size)

(* Fetch the updated biases and weights from the parameter server *)
val (updated_biases, updated_weights) = ParameterServer.fetch_parameters updated_parameter_server



