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
# 
#
set := Filtered(SL2IrrepsOfDegree(3),g-> level mod g.level = 0);
testset_red := [];
len := Length(set);
upper := [[1,0],[0,0]];
lower := [[0,0],[0,1]];
for i in [1..len] do
    for j in [i..len] do
        Append(testset_red, 
        [KroneckerProduct(upper,set[i]) + KroneckerProduct(lower,set[j])]);
    od;
od;

Append(testset, testset_red);
#############################################################################

for i in [1..Length(testset)] do
    sorted_testset := SortFun(testset[i].S, testset[i].T);
    if sorted_testset = false then
        continue;
    else
        len := Length(sorted_testset);

        for j in [1..len] do
            res := Restrict(sorted_testset[j].S, sorted_testset[j].T, level);
            if res = false then continue;
            else Append(result, [res]);
            fi;
        od;
    fi;
od;

Print(Set(result));





