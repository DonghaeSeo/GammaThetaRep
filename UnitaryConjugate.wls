SignedDiagonal[reps_] := Module[{Vs, result},

    result = Union@Last@Last@Reap@Do[
        Vs = DiagonalMatrix/@Tuples[{1, -1}, reps[i, "degree"]];
        Do[
            Reap[<|
                    "S" -> Vs[[j]].Normal[reps[i, "S"]].Vs[[j]], 
                    "T" -> Normal[reps[i, "T"]],
                    "degree" -> Normal[reps[i, "degree"]]
                |>],
            {j, Length[Vs]}
        ],
        {i, Length[reps]}
    ];

    Dataset[result]

]