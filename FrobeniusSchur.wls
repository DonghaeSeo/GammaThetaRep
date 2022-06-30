FrobeniusSchurQ[fu_, S_, T_] := Module[{rank, d, theta, nu},

    rank = Length[fu];
    d = S[[1]] / S[[1, 1]];
    theta = Diagonal[T];

    nu = Table[
        Sum[fu[[j, k, i]] d[[j]] d[[k]] (theta[[j]] / theta[[k]])^2,
            {j, rank}, {k, rank}
        ] / Sum[d[[l]]^2, {l, rank}], 
        {i, rank}
    ];

    Reduce@Table[If[fu[[i, i, 1]] == 1, nu[[i]] == 1 || nu[[i]] == -1, True],
        {i, rank}
    ]

]

FrobeniusSchur[reps_] := Module[{S, T, result},
    
    result = Last@Reap@Do[
        S = reps[[i]]["S"];
        T = reps[[i]]["T"];

        If[FrobeniusSchurQ[
            Verlinde[S], S, T
        ], Sow[reps[[i]]], Continue[]],
        {i, Length[reps]}
    ];

    If[Length[result] == 0, Return[False]];

    Dataset[result[[1]]]

]