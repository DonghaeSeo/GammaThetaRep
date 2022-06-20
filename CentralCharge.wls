CentralCharge[reps_] := Module[{result, T},

    result = Union@Last@Last@Reap@Do[
        
        T = Normal[reps[i, "T"]];

        Sow[<|
            "S" -> Normal[reps[i, "S"]],
            "T" -> T / T[[1, 1]],
            "c" -> Rationalize@Mod[Arg[T[[1, 1]]^24] / (2 Pi), 1 / 2],
            "degree" -> Normal[reps[i, "degree"]]
        |>],

        {i, Length[reps]}
    ];

    Dataset[result]

]