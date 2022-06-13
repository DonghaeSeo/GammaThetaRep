LoadPackage("SL2Reps");

SetInfoLevel(InfoSL2Reps,0);

Read("SortFun.g");
Read("Restrict.g");
Read("CentralChar.g");

IsPrimitiveGammaThetaRep := function(S, T)
local mat, diag;

mat := (S*T)^3 * Inverse(S^2);
diag := DiagonalOfMat(mat);

if IsDiagonalMat(mat) = false then return true;
elif Length(Set(diag)) <> 1 then return true;
elif IsCyc(diag[1]) = false then return true;
else return false;
fi;

end;

SortAndRestrict := function(testset)
local sorted, restricted, result, i, j;

result := [];

for i in [1..Length(testset)] do
    sorted := SortFun(testset[i].S, testset[i].T, testset[i].level);

    if sorted = false then continue;
    else
        for j in [1..Length(sorted)] do
            restricted := Restrict(sorted[j].S, sorted[j].T, sorted[j].level);
            if restricted = false then continue;
            # elif IsPrimitiveGammaThetaRep(sorted[j].S, sorted[j].T) then
            #     Append(result, [restricted]);
            elif true then
                Append(result, [restricted]);
            else continue;
            fi;
        od;
    fi;
od;

return result;

end;

GammaThetaRepsByIrreps := function(degree) 
local testset;

if degree mod 3 <> 0 then return false; fi;

testset := SL2IrrepsOfDegree(degree);

return SortAndRestrict(testset);

end;

GammaThetaRepsByReducibles := function()
local irreps, testset, i, j;

irreps := SL2IrrepsOfDegree(3);
testset := [];

for i in irreps do
    for j in irreps do
        Append(testset,
            [rec(
                S := BlockMatrix([[1,1,i.S],[2,2,j.S]],2,2),
                T := BlockMatrix([[1,1,i.T],[2,2,j.T]],2,2),
                level := LcmInt(i.level, j.level)
            )]
        );
    od;
od;

return SortAndRestrict(testset);

end;


