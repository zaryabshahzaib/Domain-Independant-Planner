% This is Russell's flat tire domain.

open(Container): unlocked(Container), closed(Container) 
                 -> unlocked(Container), open(Container).

close(Container): open(Container) 
                  -> closed(Container).

fetch(Object,Container): in(Object, Container), open(Container) 
                         -> open(Container), have(Object).

putAway(Object,Container): have(Object), open(Container) 
                           -> open(Container), in(Object, Container).

loosen(Nut,Hub): have(wrench), tight(Nut,Hub), onGround(Hub) 
                 -> have(wrench), onGround(Hub), loose(Nut,Hub).

tighten(Nut,Hub): have(wrench), loose(Nut,Hub), onGround(Hub) 
                  -> have(wrench), onGround(Hub), tight(Nut,Hub).

jackUp(Hub): have(jack), onGround(Hub) 
             -> notOnGround(Hub).

jackDown(Hub): notOnGround(Hub) 
               -> onGround(Hub), have(jack).

undo(Nut,Hub): notOnGround(Hub), fastened(Hub), have(wrench), loose(Nut,Hub) 
               -> notOnGround(Hub), have(wrench), have(Nut), unfastened(Hub).

doUp(Nut,Hub): have(wrench), unfastened(Hub), notOnGround(Hub), have(Nut) 
               -> have(wrench), notOnGround(Hub), loose(Nut,Hub), fastened(Hub).

removeWheel(Wheel,Hub): notOnGround(Hub), on(Wheel, Hub), unfastened(Hub) 
                        -> have(Wheel), free(Hub), notOnGround(Hub), unfastened(Hub).

putOnWheel(Wheel,Hub): have(Wheel), free(Hub), unfastened(Hub), notongroung(Hub) 
                       -> on(Wheel,Hub), unfastened(Hub), notOnGround(Hub).

inflate(Wheel): have(pump), notInflated(Wheel), intact(Wheel) 
                -> have(pump), intact(Wheel), inflated(Wheel).

#init: { intact(wheel2), 
         in(jack, boot), 
         in(pump, boot), 
         in(wheel2, boot), 
         in(wrench, boot),
         on(wheel1, theHub), 
         onGround(theHub), 
         tight(nuts,theHub), 
         notInflated(wheel2),
         unlocked(boot), 
         fastened(theHub), 
         closed(boot) 
       }.

#goal: { inflated(wheel2),
         notOnGround(theHub),
         loose(nuts, theHub)
       }.
