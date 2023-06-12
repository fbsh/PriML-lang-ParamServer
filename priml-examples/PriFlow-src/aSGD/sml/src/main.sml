structure Main = struct
    open Matrix
    open Vector
    open HogwildRegressor

    fun generateData n m =
        let
            fun generateRow m = if m = 0 then [] else (Random.randReal Random.default) :: generateRow (m - 1)
            fun generateMatrix n m = if n = 0 then [] else generateRow m :: generateMatrix (n - 1) m
        in
            (Vector.fromList (List.concat (generateMatrix n m)), Vector.fromList (List.tabulate n (fn _ => Random.randReal Random.default)))
        end

    val n = 1000  (* number of data points *)
    val m = 10  (* number of features *)
    val learning_rate = 0.01
    val chunk_size = 32
    val n_jobs = 4
    val n_epochs = 5

    val regressor = HogwildRegressor.create learning_rate chunk_size n_jobs n_epochs

    val (X, y) = generateData n m

    val _ = HogwildRegressor.fit regressor X y

    val _ = print "Training completed!\n"
end
