PrimitiveQ[S_, T_] := Reduce[
    RootOfUnityQ/@RootApproximant[
        Eigenvalues[MatrixPower[S.T, 3].Inverse[S.S]]
    ]
]


Primitive[reps_] := Module[{test, result, V},

    test = Union@Dataset@Table[<|
        "S" -> Normal[reps[i, "S"]],
        "T^2" -> MatrixPower[Normal[reps[i, "T"]], 2],
        "degree" -> Normal[reps[i, "degree"]]
    |>, {i, Length[reps]}];

    result = Last@Reap@Monitor[Do[
        V = DiagonalMatrix/@Tuples[{1, -1}, test[i, "degree"]];
        If[
            Reduce@PrimitiveQ[N@Normal[test[i, "S"]], #]&/@Table[
                DiagonalMatrix[Sqrt/@Diagonal@N@Normal[test[i, "T^2"]]].V[[j]],
                {j, Length[V]}
            ],
            Sow@Normal[test[i]]
        ],
        {i, Length[test]}
    ], ProgressIndicator[i / Length[test]]];

    If[Length[result] == 0, Return[False]];

    Union@Last[result]

]