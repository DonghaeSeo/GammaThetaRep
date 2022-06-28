SortFun := function( S, T )
   local dim, loop_index, diag, perm, filtered_perm, permed_T, len, r, m, p, i, j, result;

   dim := Length( S );
   diag := [];

   for loop_index in [1..dim] do
      Append( diag, [T[loop_index][loop_index]] );
   od;

   filtered_perm := PermutationsList(diag);

   perm := PermutationsList( diag );
   filtered_perm := Filtered( perm, g -> Set( g{[1..dim/3]} + g{[dim/3+1..2*dim/3]} ) = [0] 
      and IsSortedList( g{[2*dim/3+1..dim]} ) );
   permed_T := [];

   if filtered_perm = [] then
      return false;
   fi;

   len := Length( filtered_perm );

   for loop_index in [1..len] do
      Append( permed_T, [DiagonalMat( filtered_perm[loop_index] )] );
   od;

   r := [1..dim];

   result := [];

   for loop_index in [1..len] do
      p := Permuted( r, PermListList( filtered_perm[loop_index], diag ) );
      m := NullMat( dim, dim );
      for i in [1..dim] do
         for j in [1..dim] do
            m[i][j] := S[p[i]][p[j]];
         od;
      od;
      Append( result, [rec( S := m, T := permed_T[loop_index] )] );
   od;

   return Set( result );

end;