structure Communication =
struct

  (* Import necessary modules and define types *)
  structure M = Messages
  structure RM = RealMatrix
  structure P = ParameterServerUtils
  structure S = Socket

  (* Helper functions for serialization and deserialization of messages *)
  fun serialize_matrix (mat: RM.matrix) : string =
    let
      val rows = RM.rows mat
      val cols = RM.cols mat
      val elements = RM.elements mat
      val serialized_elements = String.concatWith "," (List.map Real.toString elements)
    in
      Int.toString rows ^ "," ^ Int.toString cols ^ "," ^ serialized_elements
    end

  fun deserialize_matrix (serialized: string) : RM.matrix =
    let
      val parts = String.tokens (fn c => c = #",") serialized
      val rows = Int.fromString (List.hd parts)
      val cols = Int.fromString (List.hd (List.tl parts))
      val elements = List.map Real.fromString (List.drop (List.drop parts 1) 1)
    in
      RM.matrix_of_list (Option.valOf rows) (Option.valOf cols) elements
    end

  fun serialize_matrix_list (mats: RM.matrix list) : string =
    String.concatWith ";" (List.map serialize_matrix mats)

  fun deserialize_matrix_list (serialized: string) : RM.matrix list =
    List.map deserialize_matrix (String.tokens (fn c => c = #";") serialized)

  fun serialize_message (message: M.message) : string =
    case message of
        M.GradientUpdate (worker_id, weight_gradients, bias_gradients) =>
          "GradientUpdate," ^ Int.toString worker_id ^ "," ^ serialize_matrix_list weight_gradients ^ "," ^ serialize_matrix_list bias_gradients
      | M.UpdatedWeightsBiases (weights, biases) =>
          "UpdatedWeightsBiases," ^ serialize_matrix_list weights ^ "," ^ serialize_matrix_list biases
      (* ... other message types ... *)

  fun deserialize_message (serialized: string) : M.message =
    let
      val parts = String.tokens (fn c => c = #",") serialized
      val tag = List.hd parts
    in
      case tag of
          "GradientUpdate" =>
            let
              val worker_id = Int.fromString (List.hd (List.tl parts))
              val weight_gradients = deserialize_matrix_list (List.nth (parts, 2))
              val bias_gradients = deserialize_matrix_list (List.nth (parts, 3))
            in
              M.GradientUpdate (Option.valOf worker_id, weight_gradients, bias_gradients)
            end
        | "UpdatedWeightsBiases" =>
            let
              val weights = deserialize_matrix_list (List.hd (List.tl parts))
              val biases = deserialize_matrix_list (List.hd (List.tl (List.tl parts)))
            in
              M.UpdatedWeightsBiases (weights, biases)
            end
        (* ... other message types ... *)
        | _ => raise Fail "Invalid message type"
    end
  

  (* Send gradients to Parameter Server *)
  fun send_gradients (ps_address: string) (weight_gradients: RM.matrix list) (bias_gradients: RM.matrix list) : unit =
    let
      val socket = S.connectTo (ps_address, P.ps_port)
      val message = M.GradientUpdate (get_worker_id (), weight_gradients, bias_gradients)
    in
      send_message socket message
      S.closeOut socket
    end

  (* Receive updated weights and biases from Parameter Server *)
  fun receive_updated_weights_biases (ps_address: string) : RM.matrix list * RM.matrix list =
    let
      val socket = S.connectTo (ps_address, P.ps_port)
      val message = M.UpdatedWeightsBiases ([], []) (* Placeholder for the actual data *)
      val response = receive_message socket
      val _ = S.closeIn socket
    in
      case response of
          M.UpdatedWeightsBiases (new_weights, new_biases) => (new_weights, new_biases)
        | _ => raise Fail "Unexpected response from Parameter Server"
    end

  (* Other communication functions, such as registering workers, requesting data shards, etc. *)
  (* ... *)

end
