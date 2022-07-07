CentralCharge[reps_] := Module[{result, T},

    result = Union@Last@Last@Reap@Do[
        
        T = reps[[i]]["T"];

        Sow[<|
            "S" -> reps[[i]]["S"],
            "T" -> Chop[T / T[[1, 1]]],
            "c" -> Mod[24Rationalize[Arg[T[[1, 1]]] / (2 Pi)], 1/2]
            (* "degree" -> reps[[i]]["degree"] *)
        |>],

        {i, Length[reps]}
    ];

    result

]