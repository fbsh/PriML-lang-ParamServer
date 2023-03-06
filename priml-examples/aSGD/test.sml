fun sgd_step (lr: real, data: (real * real) list, w: real) =
  let
    val (x, y) = List.nth (data, Random.rand (length data))
    val grad = 2.0 * (w * x - y) * x
  in
    w - lr * grad
  end

fun sgd (lr: real, data: (real * real) list, num_steps: int) =
  let
    val initial_w = Random.randRealRange (0.0, 1.0)
    val rec loop (w, step) =
      if step = num_steps then w
      else loop (sgd_step (lr, data, w), step + 1)
  in
    loop (initial_w, 0)
  end


val data = [(0.0, 1.0), (1.0, 3.0), (2.0, 5.0), (3.0, 7.0), (4.0, 9.0)]
val lr = 0.01
val num_steps = 1000

val result = sgd(lr, data, num_steps)

if Real.==(result, 2.0, 0.1) then
  print("Test case passed.")
else
  print("Test case failed.")
