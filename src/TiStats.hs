module TiStats where

type TiStats = Int

default(Int)

tiStatInitial = 0

tiStatIncSteps = (+) 1

tiStatGetSteps = id

applyToStats stats_fun (stack, dump, heap, sc_defs, stats) =
    (stack, dump, heap, sc_defs, stats_fun stats)
