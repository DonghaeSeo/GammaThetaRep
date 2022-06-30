CentralCharge[reps_] := Module[{result, T},

    result = Union@Last@Last@Reap@Do[
        
        T = reps[[i]]["T"];

        Sow[<|
            "S" -> reps[[i]]["S"],
            "T" -> T / T[[1, 1]],
            "c" -> Rationalize@Mod[Arg[T[[1, 1]]^24] / (2 Pi), 1 / 2],
            "degree" -> reps[[i]]["degree"]
        |>],

        {i, Length[reps]}
    ];

    result

]