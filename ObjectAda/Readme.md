# ObjectAda

This directory contains a version of the PragmARCs that compiles with the ObjectAda V10.2 Update 3 compiler. Among the differences from the version in the parent
directory are

* No bounded data structures. The bounded data structures are implemented in terms of a bounded container from the standard library. Technically, no compiler can implement the bounded containers unless it implements AI12-0409-1. OA rejects the bounded data structures, so they are ommitted.
* Some categorization aspects (Pure, Preelaborate) are rejected by the compiler. This appears to be a compiler error, but the aspects are removed from these versions.
* Some pre- and postconditions are rejected by the compiler. This appears to be a compiler error, but the conditions are removed from these versions.

An OA project file is included that will compile all of the units.
