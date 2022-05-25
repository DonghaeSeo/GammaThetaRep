SortFun := function( T, S )
local dim, loop_index, diag, perm, filtered_perm, permed_T, len, r, m, p, i, j, result;

dim := Length( S );
diag := [];

for loop_index in [1..dim] do
   Append( diag, [T[loop_index][loop_index]] );
od;

perm := PermutationsList( diag );
filtered_perm := Filtered( perm, g -> Set( g{[1..dim/3]} + g{[dim/3+1..2*dim/3]} ) = [0] and IsSortedList( g{[2*dim/3+1..dim]} ) );
permed_T := [];
len := Length( filtered_perm );

for loop_index in [1..len] do
   Append( permed_T, [DiagonalMat( filtered_perm[loop_index] )] );
od;

r := [1..dim];
m := NullMat( dim, dim );

result := [];

for loop_index in [1..len] do
   p := Permuted( r, PermListList( diag, filtered_perm[loop_index] ) );
   for i in [1..dim] do
      for j in [1..dim] do
         m[i][j] := S[p[i]][p[j]];
         Append( result, [rec( S := m, T := permed_T[loop_index] )] );
      od;
   od;
od;

return Set( result );

end;