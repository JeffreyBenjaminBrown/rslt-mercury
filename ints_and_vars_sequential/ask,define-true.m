If I read the manual through "higher-order modes" I might get it.
---

I would like to define this function:

    true(X) = Bool :-
      Bool = (if X then yes else no).

If I compile it without a type signature, the compiler suggests one:

    test.m:033: Error: clause for function `test.true'/1
    test.m:033:   without preceding `func' declaration.
    test.m:033: Inferred :- func true((pred)) = bool.bool.

If I add the suggested type signature, I get a mode error:

    test.m:034: In clause for `true(in) = out':
    test.m:034:   in argument 1 (i.e. the predicate term) of higher-order predicate
    test.m:034:   call:
    test.m:034:   mode error: variable `X' has instantiatedness `ground',
    test.m:034:   expecting higher-order pred inst (of arity 0).

mode error: variable has instantiatedness `ground', expecting higher-order pred inst (of arity 0).