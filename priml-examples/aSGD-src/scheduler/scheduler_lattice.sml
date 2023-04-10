signature LATTICE = sig
  type t
  val bottom: t
  val join: t * t -> t
  val leq: t * t -> bool
end

functor PriorityScheduler (Lattice: LATTICE) = struct
  type priority = Lattice.t

  datatype 'a task = Task of {priority: priority, payload: 'a}

  type 'a t = 'a task list

  fun empty() = []

  fun leq (t1, t2) = Lattice.leq (t1.priority, t2.priority)

  fun insert_task (task, []) = [task]
    | insert_task (task, tasks as t :: ts) =
        if leq (task, t) then task :: tasks
        else t :: insert_task (task, ts)

  fun push (queue, task) = insert_task (task, queue)

  fun pop ([]) = (NONE, [])
    | pop (task :: tasks) = (SOME task, tasks)

  fun join_priorities (queue1, queue2) =
    case (queue1, queue2) of
        ([], _) => queue2
      | (_, []) => queue1
      | (t1 :: ts1, t2 :: ts2) =>
          if leq (t1, t2) then t1 :: join_priorities (ts1, queue2)
          else t2 :: join_priorities (queue1, ts2)

end

structure IntLattice: LATTICE = struct
  type t = int
  val bottom = 0
  fun join (x, y) = Int.max (x, y)
  fun leq (x, y) = x <= y
end

structure IntPriorityScheduler = PriorityScheduler (IntLattice)
