% *********** AUXILIARY **********
% 
% Auxiliary printing
% Set priority to a higher value than a message to turn off the message
writePriority(4).

writeList(Priority,List) :- writePriority(X), X >= Priority, writeEntry(List), nl.
writeList(Priority,_) :- writePriority(X), X < Priority.

writeEntry([]).
writeEntry([Action1|Actions]) :- 
	write(Action1), 
	write(' '), 
	writeEntry(Actions).

