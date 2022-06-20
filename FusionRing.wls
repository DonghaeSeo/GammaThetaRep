Verlinde[S_] := Module[{rank},
	
	rank = Length[S];
	
	Rationalize@Chop@N@Table[
		Sum[(S[[i, l]] S[[j, l]] Conjugate[S[[k, l]]]) / S[[1, l]],
			{l, rank}
		],
		{i, rank}, {j, rank}, {k, rank}
	]
]


FusionRingQ[fu_] := Module[{rank},
	
	rank = Length[fu];
	
	If[fu[[1]] != IdentityMatrix[rank], Return[False]];

	If[Not@Element[fu, NonNegativeIntegers], Return[False]];

	If[Reduce@Flatten@Table[
		fu[[i]].fu[[j]] == fu[[j]].fu[[i]],
		{i, rank}, {j, i, rank}
	] == False, Return[False]];

	If[Reduce@Flatten@Table[
		fu[[i, j, k]] == fu[[j, i, k]],
		{i, rank}, {j, rank}, {k, rank}
	] == False, Return[False]];

	If[Table[
		Sum[fu[[i, k, 1]] fu[[k, j, 1]], {k, rank}]
		, {i, rank}, {j, rank}
	] != IdentityMatrix[rank], Return[False]];

	True

]


FusionRing[reps_] := Module[{S, result},
	
	result = Last@Reap@Do[
		S = Normal[reps[i, "S"]];
		
		If[MemberQ[S[[1]], 0], Continue[]];

		If[Not@Element[S[[1]], Reals], Continue[]];
		
		If[Not@UnitaryMatrixQ[S], Continue[]];

		If[Not@SymmetricMatrixQ[S], Continue[]];
		
		If[FusionRingQ@Verlinde[S],
			Sow[Normal[reps[i]]],
			Continue[]
		],
		{i, Length[reps]}
	];

	If[Length[result] == 0, Return[False]];

	Dataset@Union[result[[1]]]

]