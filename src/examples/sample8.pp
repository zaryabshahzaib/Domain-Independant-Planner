% The same rule needs to be applied twice with different instanciations

rule(X): a(X) -> b(X).

#init: {a(1), a(2)}.

#goal: {b(1), b(2)}.
