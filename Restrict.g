Restrict := function( T, S )
local dim, t, id, U, SS, s;

dim := Length( T );
t := T{[dim/3+1..2*dim/3]}{[dim/3+1..2*dim/3]};
id := IdentityMat( dim/3 );
U := BlockMatrix([[1,1,-t],
		      [1,2,t],
		      [2,1,id],
		      [2,2,id],
		      [3,3,id]],3,3);
SS := U * S * Inverse( U );

if SS{[dim/3+1..2*dim/3]}{[dim/3+1..2*dim/3]} = NullMat( dim/3, dim/3 )
and SS{[dim/3+1..2*dim/3]}{[2*dim/3+1..dim]} = NullMat( dim/3, dim/3 )
and SS{[2*dim/3+1..dim]}{[dim/3+1..2*dim/3]} = NullMat( dim/3, dim/3 ) then
s := SS{[1..dim/3]}{[1..dim/3]};
else 
s := false;
fi;

return [t, s];

end;