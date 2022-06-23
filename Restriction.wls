Construction[reps1_, reps2_] := Module[{l1, l2, len, p1, p2, d1, d2},
    {l1, l2} = Length@Normal/@{
        reps1[1, "S"], reps2[1, "S"]
    };
    len = l1 + l2;
    Do[
        {d1, d2} = Diagonal/@Normal/@{
            reps1[i, "T"], reps2[j, "T"]
        };
        {p1, p2} = Permutations/@{d1, d2};
        Select[
            Flatten/@Tuples[{p1, p2}], 
            (Union[#[[1;;len/3]] + #[[len/3+1;;2len/3]]] == {0})&
            && OrderedQ[#[[2len/3+1;;-1]]]&
        ];
        If[

        ],
        {i, Length[reps1]}, {j, Length[reps2]}
    ]
]