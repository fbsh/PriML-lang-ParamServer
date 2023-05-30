structure DataGenerator :> sig
    val chunkify: int -> 'a Vector.vector -> 'a Vector.vector list
end = struct
    fun chunkify size data = 
        let 
            val length = Vector.length data
            fun loop index acc = 
                if index >= length 
                then List.rev acc
                else 
                    let 
                        val end_index = Int.min(index + size, length)
                        val chunk = Vector.tabulate(end_index - index, fn i => Vector.sub(data, index + i))
                    in
                        loop end_index (chunk::acc)
                    end
        in
            loop 0 []
        end
end
