IsFusionRing := function(N)
    local i, j, k, len, tmp;
    
    len := Length(N);

    if N[1] <> IdentityMat(len) then
        return false;
    fi;

    for i in [1..len] do
        for j in [1..len] do
            if N[i] * N[j] <> N[j] * N[i] then
                return false;
            fi;
            
            tmp := [];
            for k in [1..len] do
                Append(tmp, [N[i][k][1] * N[k][j][1]]);

                if not N[i][j][k] in Integers then
                    return false;
                elif N[i][j][k] < 0 then
                    return false;
                elif N[i][j][k] <> N[j][i][k] then
                    return false;
                fi;
            od;

            if i = j then
                if Sum(tmp) = 1 then
                    continue;
                else return false;
                fi;
            else
                if Sum(tmp) = 0 then
                    continue;
                else return false;
                fi;
            fi;
        od;
    od;

    return true;

end;