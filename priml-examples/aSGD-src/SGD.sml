(* Define a type alias for a 2D point *)
type point = real * real

(* Calculate the mean of a list of real numbers *)
fun mean (nums : real list) : real =
  let
		val sum = List.foldl (op +) 0.0 nums
		val length = real (List.length nums)
	in
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

(* Gradient descent algorithm *)
fun gradient_descent (data : point list) (m_init : real) (b_init : real) (learning_rate : real) (iterations : int) : real * real =
	let
		fun loop (m, b, iter) =
			if iter = 0 then
				(m, b)
			else
				let
					val (dm, db) = gradient data m b
					val (new_m, new_b) = step m b learning_rate dm db
				in
					loop (new_m, new_b, iter - 1)
				end
	in
		loop (m_init, b_init, iterations)
	end

val data_points = [(1.0, 2.0), (2.0, 3.0), (3.0, 5.0), (4.0, 7.0), (5.0, 9.0)]
val m_init = 1.0

val b_init = 0.0
val learning_rate = 0.01
val iterations = 10000000

val (m, b) = gradient_descent data_points m_init b_init learning_rate iterations

