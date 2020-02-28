
pickup(B): clear(B), ontable(B), armempty -> holding(B).

putdown(B): holding(B) -> clear(B), ontable(B), armempty.

stack(Top, Bottom): clear(Bottom), holding(Top) -> armempty, clear(Top), on(Top, Bottom).

unstack(Top, Bottom): on(Top, Bottom), clear(Top), armempty -> holding(Top), clear(Bottom).


#init: {ontable(a), ontable(b), on(c,a), clear(b), clear(c), armempty}.

#goal: {on(a,b), on(b,c)}.

