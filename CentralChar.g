CentralChar := function(reps)
    local i, S_mod, T00, en, total_qd, cc, result;

    result := [];

    for i in [1..Length(reps)] do
        if Set(ImaginaryPart(reps[i].S[1])) <> [0] then continue; fi;

        # total_qd := reps[i].S[1][1];
        # S_mod := reps[i].S / total_qd;
        # if total_qd < 0 then total_qd := -total_qd; fi;
        # if Minimum(S_mod[1]) <> 1 then
        #     S_mod := -S_mod;
        # fi;
        # T00 := reps[i].T[1][1];
        # en := DescriptionOfRootOfUnity(T00);
        Append(result, [rec(
            # D := total_qd,
            S := reps[i].S / reps[i].S[1][1],
            TT := (reps[i].T / T00)^2,
            # c := 24*en[1] / en[2] - QuoInt(48*en[1], en[2]),
            degree := reps[i].degree
        )]);
    od;

    return Set(result);

end;


