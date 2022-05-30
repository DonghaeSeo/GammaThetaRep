LoadPackage("SL2Reps");

SetInfoLevel(InfoSL2Reps,0);

Read("SortFun.g");
Read("Restrict.g");

##############
# arguments
dim := 6;
level := 32;
##############

result := [];

testset := Filtered(SL2IrrepsOfDegree(dim),g->g.level=level);

#############################################################################
# 3 + 3 reducible reps
#
set := Filtered(SL2IrrepsOfDegree(3), g-> level mod g.level = 0);
len := Length(set);
for i in [1..len] do
    for j in [i..len] do
        Append(testset, 
        [rec(S := BlockMatrix([[1,1,set[i].S],[2,2,set[j].S]],2,2), 
        T := BlockMatrix([[1,1,set[i].T],[2,2,set[j].T]],2,2), 
        level := LcmInt(set[i].level, set[j].level))]);
    od;
od;
#############################################################################

for i in [1..Length(testset)] do
    sorted_testset := SortFun(testset[i].S, testset[i].T);
    if sorted_testset = false then
        continue;
    else
        len := Length(sorted_testset);

        for j in [1..len] do
            res := Restrict(sorted_testset[j].S, sorted_testset[j].T, level);
            if res = false then 
                continue;
            elif Length(Set(DiagonalOfMat((res.S*res.T)^3))) = 1
            and IsCyc(((res.S*res.T)^3)[1][1]) then 
                continue;
            else 
                Append(result, [res]);
            fi;
        od;
    fi;
od;

Print(Set(result));





