IsPrimitiveGammaThetaRep := function(S, T)
    local mat, diag;

    mat := (S*T)^3 * Inverse(S^2);
    diag := DiagonalOfMat(mat);

    if IsDiagonalMat(mat) = false then return true;
    elif Length(Set(diag)) <> 1 then return true;
    elif diag[1]^Conductor(diag[1]) <> 1 then return true;
    else return false;
    fi;

end;

Verlinde := function(S)
    local len, N, i, j, k, s, l;

    len := Length(S);

    N := [];

    for i in [1..len] do
        Append(N, [IdentityMat(len)]);
    od;

    for i in [1..len] do
        for j in [1..len] do
            for k in [1..len] do
                s := [];
                for l in [1..len] do
                    Append(s, [S[i][l]*S[j][l]*ComplexConjugate(S[k][l])/S[1][l]]);
                od;
                N[i][j][k] := Sum(s);
            od;
        od;
    od;

    return N / N[1][1][1];

end;

CentralChar := function(reps)
    local i, result, dru, tmp, N;

    result := [];

    for i in [1..Length(reps)] do
        if 0 in reps[i].S[1] then
            continue;
        elif Set(ImaginaryPart(reps[i].S[1])) <> [0] then
            continue;
        elif IsPrimitiveGammaThetaRep(reps[i].S, reps[i].T) = false then
            continue;
        fi;

        dru := DescriptionOfRootOfUnity(reps[i].T[1][1]);

        tmp := (dru[2]*24 / dru[1]) - Int(dru[2]*24 / dru[1]);

        Append(result, [rec(
            S := reps[i].S / reps[i].S[1][1],
            TT := (reps[i].T / reps[i].T[1][1])^2,
            c := (2*tmp - Int(2*tmp)) / 2,
            degree := reps[i].degree
        )]);
    od;

    return Set(result);

end;

Check := function(S, T)
    local N, Nflat, i, j, k, len;

    N := Verlinde(S);
    Nflat := Flat(N);
    
    len := Length(S);

    if N[1] <> IdentityMat(len) then
        return false;
    fi;

    for i in [1..len] do
        for j in [1..len] do
            if N[i] * N[j] <> N[j] * N[i] then
                return false;
            fi;

            for k in [1..len] do
                if not N[i][j][k] in Integers then
                    return false;
                elif N[i][j][k] < 0 then
                    return false;
                elif N[i][j][k] <> N[j][i][k] then
                    return false;
                fi;
            od;
        od;
    od;

    return true;

end;

#####################
# test code

test := function(reps)
    local result, rep;

    result := [];

    for rep in reps do
        if Check(rep.S, rep.TT) then
            Append(result, [rep]);
        fi;
    od;

    return result;

end;
