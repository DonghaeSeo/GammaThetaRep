FrobeniusSchurQ[fu_, S_, T_] := Module[{rank, d, theta, nu},

    rank = Length[fu];
    d = S[[1]];
    theta = Diagonal[T];

    nu = Table[
        2 Sum[fu[[j, k, i]] d[[j]] d[[k]] (theta[[j]] / theta[[k]])^2,
            {j, rank}, {k, rank}
        ] / Sum[d[[l]]^2, {l, rank}], 
        {i, rank}
    ];

    Reduce@Table[If[fu[[i, i, 1]] == 1, nu[[i]] == 1 || nu[[i]] == -1, True],
        {i, rank}
    ]

]

FrobeniusSchur[reps_] := Module[{S, T},
    
    Last@Reap@Do[
        S = Normal[reps[i, "S"]];
        T = Normal[reps[i, "T"]];

        If[FrobeniusSchurQ[
            Verlinde[S], S, T
        ], Sow[Normal[reps]], Continue[]],
        {i, Length[reps]}
    ]

]