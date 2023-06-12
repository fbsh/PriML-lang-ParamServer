open CML


fun spawnEval node =
      let
        val mbox = CML.mailbox ()
        val _ = CML.spawn (fn () => CML.send (mbox, evalNode node))
      in
        mbox
      end
