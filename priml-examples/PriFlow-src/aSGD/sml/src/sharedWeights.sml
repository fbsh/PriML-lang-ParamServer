structure SharedWeights :> sig
    type t
    val create: int -> t
    val read: t -> real Vector.vector
    val write: t -> real Vector.vector -> unit
end = struct
    structure S = SyncVar

    type t = real Vector.vector S.var

    fun create size = S.iVar (Vector.tabulate(size, fn _ => 0.0))

    fun read var = S.iGet var

    fun write var newWeights = S.iPut var newWeights

end
