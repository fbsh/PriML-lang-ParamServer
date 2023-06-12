open CML
open SharedWeights
open DataGenerator
open GradientStep


structure HogwildRegressor :> sig
    type t
    val create: real -> real -> int -> int -> int -> t
    val fit: t -> real Matrix.matrix -> real Vector.vector -> unit
end = struct
    type t = { learning_rate: real, 
               chunk_size: int, 
               n_jobs: int, 
               n_epochs: int, 
               shared_weights: SharedWeights.t ref }

    fun create learning_rate chunk_size n_jobs n_epochs =
        { learning_rate = learning_rate, 
          chunk_size = chunk_size, 
          n_jobs = n_jobs, 
          n_epochs = n_epochs, 
          shared_weights = ref (SharedWeights.create 0) }

    fun fit reg X y =
        let 
            val chunks = DataGenerator.chunkify (#chunk_size reg) X
            fun train_epoch chunk = 
                let 
                    val w = !(#shared_weights reg)
                    val grad = GradientStep.mseGradientStep (#learning_rate reg) chunk y w
                    val new_w = w - grad
                in 
                    SharedWeights.write (#shared_weights reg) new_w
                end

            fun parallel_for _ [] = ()
              | parallel_for n (x::xs) = 
                if n <= 0 then () 
                else 
                    let 
                        val _ = spawn (fn () => train_epoch x)
                    in
                        parallel_for (n - 1) xs
                    end
        in
            parallel_for (#n_jobs reg) chunks
        end
end
