structure T = Timer

(* Define a type alias for a 2D point *)
type point = real * real

(* Calculate the mean of a list of real numbers *)
fun mean (nums : real list) : real =
  let
    val sum = List.foldl (op +) 0.0 nums
    val length = real (List.length nums)
  in7
    sum / length
  end

(* Compute the gradient for the given data points and current parameters (m and b) *)
fun gradient (data : point list) (m : real) (b : real) : real * real =
    let
        val n = real (List.length data)
        val diffs = List.map (fn (x, y) => (x * (m * x + b - y), (m * x + b - y))) data
        val (dm_sum, db_sum) = List.foldl (fn ((dx, dy), (mx, my)) => (mx + dx, my + dy)) (0.0, 0.0) diffs
    in
        (2.0 * dm_sum / n, 2.0 * db_sum / n)
    end

(* Perform one step of the gradient descent *)
fun step (m : real) (b : real) (learning_rate : real) (dm : real) (db : real) : real * real =
  (m - learning_rate * dm, b - learning_rate * db)

(* Custom slice function to get a sublist from a given list *)
fun slice (lst, start, len) =
  let
    fun drop (lst, 0) = lst
      | drop ([], _) = []
      | drop (x::xs, n) = drop (xs, n - 1)
    fun take (lst, 0) = []
      | take ([], _) = []
      | take (x::xs, n) = x :: take (xs, n - 1)
  in
    take (drop (lst, start), len)
  end

(* Divide the data into chunks *)
fun divide_data (data : point list) (num_chunks : int) : point list list =
  let
    val chunk_size = (List.length data + num_chunks - 1) div num_chunks
    fun divide (data, []) = []
      | divide (data, sz::sizes) =
        let
          val chunk = slice (data, 0, sz)
          val rest = slice (data, sz, List.length data - sz)
        in
          chunk :: divide (rest, sizes)
        end
    val sizes = List.tabulate (num_chunks, fn i => chunk_size)
  in
    divide (data, sizes)
  end

(* The parallel gradient descent function *)
fun parallel_gradient_descent (data : point list) (m_init : real) (b_init : real) (learning_rate : real) (iterations : int) (num_threads : int) : real * real =
  let
    fun loop (m, b, iter) =
      if iter = 0 then
        (m, b)
      else
        let
          (* Divide data into chunks *)
          val data_chunks = divide_data data num_threads

          (* Calculate gradients in parallel *)
          val gradients = List.map (fn chunk => gradient chunk m b) data_chunks

          (*Calculate mean of gradients *)
          val (dm_sum, db_sum) = List.foldl (fn ((dx, dy), (mx, my)) => (mx + dx, my + dy)) (0.0, 0.0) gradients
          val dm_mean = dm_sum / real (List.length gradients)
          val db_mean = db_sum / real (List.length gradients)
          
          (* Perform one step of the gradient descent *)
          val (new_m, new_b) = step m b learning_rate dm_mean db_mean
        in
          loop (new_m, new_b, iter - 1)
        end
  in
    loop (m_init, b_init, iterations)
  end

val start_time = T.startRealTimer()

val data = [(1.0, 2.0), (2.0, 4.0), (3.0, 6.0), (4.0, 8.0)]
val (m, b) = parallel_gradient_descent data 0.0 0.0 0.01 1000000 4

val running_time = T.checkRealTimer start_time

val _ = print ("Running time: " ^ Real.toString (Time.toReal running_time) ^ " seconds\n")
val _ = print ("Running time: " ^ Real.toString (Time.toReal running_time * 1000.0) ^ " milliseconds\n")
val _ = print ("Running time: " ^ Real.toString (Time.toReal running_time * 1000000.0) ^ " microseconds\n")