structure Messages =
struct

  (* Message types used for communication between the Parameter Server, Data Shards, and Replicas *)

  datatype gradient = WeightGradient of RealMatrix.matrix | BiasGradient of RealMatrix.matrix
  datatype update = WeightUpdate of RealMatrix.matrix | BiasUpdate of RealMatrix.matrix

  datatype message =
      GradientUpdate of (int * gradient list * gradient list)
    | UpdatedWeightsBiases of (RealMatrix.matrix list * RealMatrix.matrix list)
    | RegisterWorker of int
    | WorkerRegistered of unit
    | RequestDataShard of int
    | DataShardResponse of (ParameterServerUtils.data_shard option)
    | TerminateWorker of unit

  (* Add more message types as needed *)
  
end
