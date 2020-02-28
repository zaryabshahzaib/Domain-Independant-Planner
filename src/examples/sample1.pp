% The simplest possible example.

rule: a -> b.
rule1: c -> f.
rule2: b -> c.
rule3: b -> e.
rule4: e -> c.
rule5: a -> c.


#init: {a}.

#goal: {c}.
