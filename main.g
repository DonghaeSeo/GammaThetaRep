LoadPackage("SL2Reps");

SetInfoLevel(InfoSL2Reps,0);

Read("SortFun.g");
Read("Restrict.g");

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
            elif IsPrimitiveGammaThetaRep(sorted[j].S, sorted[j].T) then
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

# GammaThetaRepsByReducibles := function(list_of_degrees)
# local bmatset, deg, testset, sorted, restricted, result, i;

# if Sum(list_of_degrees) mod 3 <> 0 then return false; fi;

# bmatset := [];
# for deg in Set(list_of_degrees) do
#     Append(bmatset, [rec(irreps := SL2IrrepsOfDegree(deg), degree := deg)]);
# od;

# # testset := [];
# # for i in [1..Length(list_of_degrees)] do
# #     BlockMatrix()
# # od;

# end;

# ##############
# # arguments
# dim := 6;
# level := 32;
# ##############

# result := [];

# # testset := [];
# testset := Filtered(SL2IrrepsOfDegree(dim),g->g.level = level);
# # testset := SL2IrrepsOfDegree(dim);

#############################################################################
# 3 + 3 reducible reps
#
# set := Filtered(SL2IrrepsOfDegree(3), g-> level mod g.level = 0);
# len := Length(set);
# for i in [1..len] do
#     for j in [i..len] do
#         Append(testset, 
#         [rec(S := BlockMatrix([[1,1,set[i].S],[2,2,set[j].S]],2,2), 
#         T := BlockMatrix([[1,1,set[i].T],[2,2,set[j].T]],2,2), 
#         level := LcmInt(set[i].level, set[j].level))]);
#     od;
# od;
#############################################################################

# for i in [1..Length(testset)] do
#     sorted_testset := SortFun(testset[i].S, testset[i].T, testset[i].level);

#     if Length(sorted_testset) mod 4 <> 0 then
#         Print(testset[i]);
#     fi;

#     if sorted_testset = false then
#         continue;
#     else
#         len := Length(sorted_testset);

#         for j in [1..len] do
#             res := Restrict(sorted_testset[j].S, 
#                 sorted_testset[j].T, 
#                 sorted_testset[j].level);
#             if res = false then 
#                 continue;
#             # elif Length(Set(DiagonalOfMat((res.S*res.T)^6))) = 1
#             elif IsIdentityMat((res.S*res.T)^3*Inverse(res.S^2))
#             and Length(Set(DiagonalOfMat((res.S*res.T)^3*Inverse(res.S^2)))) = 1
#             and IsCyc(((res.S*res.T)^3)[1][1]) 
#             then continue;
#             else 
#                 Append(result, [res]);
#             fi;
#         od;
#     fi;
# od;

# result := Set(result);

# Print(result);





