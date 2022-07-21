SignedDiagonal[reps_] := Module[{Vs, result},

    result = Union@Last@Last@Reap@Do[
        (* Vs = DiagonalMatrix/@Tuples[{1, -1}, reps[[i]]["degree"]]; *)
        Vs = DiagonalMatrix/@Tuples[{1, -1}, Length[reps[[i]]["s"]]];
        Do[
            Sow[<|
                    "s" -> Vs[[j]].reps[[i]]["s"].Vs[[j]], 
                    "t^2" -> reps[[i]]["t^2"],
                    "d" -> reps[[i]]["d"],
                    "n" -> reps[[i]]["n"]
                |>],
            {j, Length[Vs]}
        ],
        {i, Length[reps]}
    ];

    result

]