% The wolf, the goat, and the cabbages
%
% The configuration of the world is represented by
% config(wolf-side, goat-side, cabbages-side, human-side) 

moveEmptyEW(W, G, C): config(W, G, C, e), valid(W, G, C, w) -> config(W, G, C, w), valid(W, G, C, w).

moveEmptyWE(W, G, C): config(W, G, C, w), valid(W, G, C, e) -> config(W, G, C, e), valid(W, G, C, e).

moveWolfEW(G, C): config(e, G, C, e), valid(w, G, C, w) -> config(w, G, C, w), valid(w, G, C, w).

moveWolfWE(G, C): config(w, G, C, w), valid(e, G, C, e) -> config(e, G, C, e), valid(e, G, C, e).

moveGoatEW(W, C): config(W, e, C, e), valid(W, w, C, w) -> config(W, w, C, w), valid(W, w, C, w).

moveGoatWE(W, C): config(W, w, C, w), valid(W, e, C, e) -> config(W, e, C, e), valid(W, e, C, e).

moveCabbagesEW(W, G): config(W, G, e, e), valid(W, G, w, w) -> config(W, G, w, w), valid(W, G, w, w). 

moveCabbagesWE(W, G): config(W, G, w, w), valid(W, G, e, e) -> config(W, G, e, e), valid(W, G, e, e). 

#init: { config(e, e, e, e),
         valid(w, e, w, e), valid(e, w, e, w),
         valid(w, w, w, w), valid(w, w, e, w),
         valid(w, e, w, w), valid(e, w, w, w),
         valid(e, e, e, e), valid(e, e, w, e),
         valid(e, w, e, e), valid(w, e, e, e)
       }.

#goal: { config(w, w, w, w) }.
