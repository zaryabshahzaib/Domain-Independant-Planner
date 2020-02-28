% Hanoi tower - 3 disks

move(Disk, From, To): on(Disk, From), clear(Disk), clear(To), smaller(Disk, To)
                   -> smaller(Disk, To), on(Disk, To), clear(Disk), clear(From).

#init: { smaller(d1, d2),
         smaller(d2, d3),
         smaller(d1, p1),
         smaller(d2, p1),
         smaller(d3, p1),
         smaller(d1, p2),
         smaller(d2, p2),
         smaller(d3, p2),
         smaller(d1, p3),
         smaller(d2, p3),
         smaller(d3, p3),
         on(d1, d2),
         on(d2, d3),
         on(d3, p1),
         clear(d1),
         clear(p2),
         clear(p3)
       }.

#goal: { on(d1, d2),
         on(d2, d3),
         on(d3, p3)
       }.
