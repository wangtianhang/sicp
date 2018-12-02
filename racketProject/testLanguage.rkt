;To run an R6RS program with DrRacket choose Use language declared in source from the language dialog box and add the following line to the top of your program. #!r6rs.
#!r6rs
(import (rnrs lists (6))
        (rnrs base (6))
        (rnrs io simple (6)))
(display (find even? '(3 1 4 1 5 9)))

