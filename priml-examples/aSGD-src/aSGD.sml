(* Custom take_drop function *)
fun take_drop (lst : 'a list) (n : int) : ('a list * 'a list) =
	let
		fun loop (lst, taken, dropped, n) =
			if n = 0 then
				(List.rev taken, dropped @ lst)
			else case lst of
				[] => (List.rev taken, dropped)
				| x :: xs => loop (xs, x :: taken, dropped, n - 1)
	in
			loop (lst, [], [], n)
	end

(* Additional function to divide data into mini-batches *)
fun divide_into_batches (data : point list) (batch_size : int) : point list list =
	let
		fun loop (remaining_data, acc) =
			if null remaining_data then
				List.rev acc
			else
				let
					val (batch, rest) = take_drop remaining_data batch_size
				in
					loop (rest, batch :: acc)
				end
    in
			loop (data, [])
    end

(* Gradient function that works with a list of points *)
fun gradient (points : point list) (m : real) (b : real) : real * real =
	let
		val n = real (List.length points)
		val sum_errors_m_b = List.foldl (fn (point, (sum_m, sum_b)) =>
				let
					val x = #1 point
					val y = #2 point
					val error = y - (m * x + b)
				in
					(sum_m + error * x, sum_b + error)
				end
		) (0.0, 0.0) points
	in
		(#1 sum_errors_m_b / n * -2.0, #2 sum_errors_m_b / n * -2.0)
	end

(* Mini-batch gradient descent algorithm *)
fun mini_batch_gradient_descent (data : point list) (m_init : real) (b_init : real) (learning_rate : real) (iterations : int) (batch_size : int) : real * real =
	let
		val batches = divide_into_batches data batch_size

		fun process_batches (m, b, []) = (m, b)
			| process_batches (m, b, batch :: rest_batches) =
				let
					val (dm, db) = gradient batch m b
					val (new_m, new_b) = step m b learning_rate dm db
				in
					process_batches (new_m, new_b, rest_batches)
				end

	fun loop (m, b, iter) =
		if iter = 0 then
			(m, b)
		else
			let
				val (updated_m, updated_b) = process_batches (m, b, batches)
			in
				loop updated_m updated_b (iter - 1)
			end
	in
		loop (m_init, b_init, iterations)
	end




val data_points = [(1.0, 2.0), (2.0, 3.0), (3.0, 5.0), (4.0, 7.0), (5.0, 9.0)]
val m_init = 1.0
val b_init = 0.0
val learning_rate = 0.01
val iterations = 1000
val batch_size = 2

val (m, b) = mini_batch_gradient_descent data_points m_init b_init learning_rate iterations batch_size







