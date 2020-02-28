
pickup(B): clear(B), ontable(B), armempty -> holding(B).

putdown(B): holding(B) -> clear(B), ontable(B), armempty.

stack(Top, Bottom): clear(Bottom), holding(Top) -> armempty, clear(Top), on(Top, Bottom).

unstack(Top, Bottom): on(Top, Bottom), clear(Top), armempty -> holding(Top), clear(Bottom).


#init: {ontable(a), on(b,a), on(c,b), on(d,c), on(e,d), on(f,e), on(g,f), on(h,g) clear(h), armempty}.

#goal: {on(b,a), on(c,b), on(d,c), on(e,d), on(f,e), on(g,f), on(a,h)}.

