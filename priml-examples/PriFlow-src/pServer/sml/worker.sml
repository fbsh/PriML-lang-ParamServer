structure Worker =
struct

  (* Import necessary modules and define types *)
  structure M = Messages
  structure C = Communication
  structure P = ParameterServerUtils
  structure RM = RealMatrix

  (* Define the neural network architecture *)
  val num_input_nodes = 784
  val num_hidden_nodes = 128
  val num_output_nodes = 10

  (* Activation functions *)
  fun relu (x: real): real = if x > 0.0 then x else 0.0
  fun relu_derivative (x: real): real = if x > 0.0 then 1.0 else 0.0
  fun softmax (input: RM.matrix): RM.matrix = RM.softmax input

  (* Forward pass of the neural network *)
  fun forward_pass (input: RM.matrix) (weights: RM.matrix list) (biases: RM.matrix list) : RM.matrix =
    let
      val hidden = RM.map relu (RM.add (RM.mul input (List.hd weights)) (List.hd biases))
      val output = softmax (RM.add (RM.mul hidden (List.hd (List.tl weights))) (List.hd (List.tl biases)))
    in
      output
    end

  (* Backward pass of the neural network (backpropagation) *)
  fun backward_pass (output: RM.matrix) (target: RM.matrix) (weights: RM.matrix list) (biases: RM.matrix list) : RM.matrix list * RM.matrix list =
    let
      val delta_output = RM.sub output target
      val weight_gradients_output = RM.mul (RM.transpose (RM.map relu_derivative (RM.add (RM.mul output (List.hd (List.tl weights))) (List.hd (List.tl biases))))) delta_output
      val bias_gradients_output = delta_output

      val delta_hidden = RM.mul delta_output (RM.transpose (List.hd (List.tl weights)))
      val weight_gradients_hidden = RM.mul (RM.transpose (RM.map relu_derivative (RM.add (RM.mul output (List.hd weights)) (List.hd biases)))) delta_hidden
      val bias_gradients_hidden = delta_hidden
    in
      ([weight_gradients_hidden, weight_gradients_output], [bias_gradients_hidden, bias_gradients_output])
    end

  (* Main worker loop *)
  fun worker_loop (worker_id: int) (data_shard: P.data_shard) (ps_address: string) : unit =
  let
    (* Initialize weights, biases, and other variables *)
    val weights = ref [RM.random num_input_nodes num_hidden_nodes, RM.random num_hidden_nodes num_output_nodes]
    val biases = ref [RM.random 1 num_hidden_nodes, RM.random 1 num_output_nodes]
    val learning_rate = 0.01
    val batch_size = 32

    (* Main training loop *)
    fun train_loop (iter: int) =
      if iter < length data_shard then
        let
          val (input, target) = List.nth (data_shard, iter)

          (* Perform forward pass *)
          val output = forward_pass input (!weights) (!biases)

          (* Compute loss and check for termination conditions *)
          val loss = P.mean_squared_error output target
          if iter mod 10 = 0 then print ("Iteration: " ^ Int.toString iter ^ ", Loss: " ^ Real.toString loss ^ "\n") else ()

          (* Perform backward pass (backpropagation) *)
          val (weight_gradients, bias_gradients) = backward_pass output target (!weights) (!biases)

          (* Send gradients to Parameter Server *)
          C.send_gradients ps_address weight_gradients bias_gradients

          (* Receive updated weights and biases from Parameter Server *)
          val (new_weights, new_biases) = C.receive_updated_weights_biases ps_address

          (* Update local weights and biases *)
          weights := new_weights
          biases := new_biases

          (* Continue with the next iteration *)
          train_loop (iter + 1)
        end
        else
            ()
    in
        train_loop 0
        end

end
