# ObjectAda

This directory contains a version of the PragmARCs that compiles with the ObjectAda V10.2 Update 4 compiler. Among the differences from the version in the parent
directory are

* No bounded data structures. The bounded data structures are implemented in terms of a bounded container from the standard library. Technically, no compiler can implement the bounded containers unless it implements AI12-0409-1. OA rejects the bounded data structures, so they are omitted.
* Some unnecessary parentheses are needed in preconditions in PragmARC.Cards.Decks.General. PTC has acknowledged this as a compiler error.
* The postcondition in PragmARC.Three_Way is rejected by the compiler and has been removed. PTC has acknowledged this as a compiler error.

In addition, a use clause for package Ada in the private part of PragmARC.Data_Structures-Skip_Lists.Unbounded was not recognized as applying in the body. PTC has acknowledged this as a compiler error. However, this use clause was in violation of the PragmAda coding standard (https://github.com/jrcarter/Coding_Standard) and was removed, so this unit is the same as the version in the parent directory.

An OA project file is included that should compile all of the units.
