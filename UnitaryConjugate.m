SignedDiagonal[reps_] := Module[{Vs, result},

    result = Union@Last@Last@Reap@Do[
        Vs = DiagonalMatrix/@Tuples[{1, -1}, reps[[i]]["degree"]];
        Do[
            Sow[<|
                    "S" -> Vs[[j]].reps[[i]]["S"].Vs[[j]], 
                    "T" -> reps[[i]]["T"],
                    "degree" -> reps[[i]]["degree"]
                |>],
            {j, Length[Vs]}
        ],
        {i, Length[reps]}
    ];

    result

]