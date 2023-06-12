structure GradientStep :> sig
    val mseGradientStep: real -> real Matrix.matrix -> real Vector.vector -> real Vector.vector -> real Vector.vector
end = struct
    fun mseGradientStep learning_rate X y w = 
        let 
            val err = y - (Matrix.vectorMult X w)
            val grad = Matrix.scale (Matrix.transposeMult err X) (~2.0 / Real.fromInt(Vector.length y))
            val update = Vector.map (fn x => x * learning_rate) grad
        in
            Vector.map2 (fn wi ui => wi - ui) w update
        end
end
