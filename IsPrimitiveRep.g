IsPrimitiveRep := function(S, T)
    local mat, diag;

    mat := (S*T)^3 * Inverse(S^2);
    diag := DiagonalOfMat(mat);

    if IsDiagonalMat(mat) = false then return true;
    elif Length(Set(diag)) <> 1 then return true;
    elif diag[1]^Conductor(diag[1]) <> 1 then return true;
    else return false;
    fi;

end;

FilterPrimitives := function(reps)
    local rep, res;

    res := [];

    for rep in reps do
        if IsPrimitiveRep(rep.S, rep.T) then
            Append(res, [rep]);
        fi;
    od;

    return res;
end;