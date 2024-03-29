-module(stack).
-export([new/0, push/2, pop/1, peek/1, is_empty/1]).


new() -> [].


push(Item, Stack) -> [Item | Stack].


pop([]) -> throw({error, "Attempting to pop an element from an empty stack."});

pop([_Head | Tail]) -> Tail.


peek([]) -> throw({error, "Attempting to peek an element from an empty stack."});

peek([Head | _Tail]) -> Head.


is_empty([]) -> true;

is_empty(_) -> false.
