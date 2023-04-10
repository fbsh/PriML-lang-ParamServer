datatype 'a task = Task of {priority: int, payload: 'a}

structure PriorityQueue = struct
  type 'a t = 'a task list

  fun empty() = []

  fun insert(task, queue) =
    let
      fun insertHelper([], acc) = List.rev (task :: acc)
        | insertHelper(t :: ts, acc) =
            if #priority task < #priority t then
              List.revAppend(acc, task :: t :: ts)
            else
              insertHelper(ts, t :: acc)
    in
      insertHelper(queue, [])
    end

  fun pop(queue) =
    case queue of
        [] => NONE
      | t :: ts => SOME(t, ts)
end

(* Function to simulate processing a task *)
fun processTask(task: 'a) = ()

(* Priority-based scheduler *)
fun priorityScheduler(queue: 'a PriorityQueue.t) =
  let
    fun processQueue(queue) =
      case PriorityQueue.pop(queue) of
          NONE => () (* No tasks left in the queue *)
        | SOME(task, remainingTasks) =>
            (processTask(#payload task);
             processQueue(remainingTasks))
  in
    processQueue(queue)
  end

(* Function to simulate processing a task *)
fun processTask(task: 'a) = ()

(* Priority-based scheduler *)
fun priorityScheduler(queue: 'a PriorityQueue.t) =
  let
    fun processQueue(queue) =
      case PriorityQueue.pop(queue) of
          NONE => () (* No tasks left in the queue *)
        | SOME(task, remainingTasks) =>
            (processTask(#payload task);
             processQueue(remainingTasks))
  in
    processQueue(queue)
  end
