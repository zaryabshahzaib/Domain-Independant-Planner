% Hanoi tower - 1 disk

move(Disk, From, To): on(Disk, From), clear(Disk), clear(To), smaller(Disk, To)
                   -> smaller(Disk, To), on(Disk, To), clear(Disk), clear(From).

#init: { smaller(d1, p1),
         smaller(d1, p2),
         smaller(d1, p3),
         on(d1, p1),
         clear(d1),
         clear(p2),
         clear(p3)
       }.

#goal: { on(d1, p3) }.
