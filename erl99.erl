-module (erl99).
-compile (export_all).

% p01: last item in a list
lastP01([])		-> [];
lastP01([Head|[]])	-> Head;
lastP01([_|Tail])	-> lastP01(Tail).

% p02: last but one block
lastButOneP02([])	-> [];
lastButOneP02([X,Y|[]])	-> [X,Y];
lastButOneP02([_|Tail])	-> lastButOneP02(Tail).
% last line is too clever - can't see what happens with 1 element list
% lastButOneP02([X|[]])	-> [];
% lastButOneP02([_,Y|Tail]) -> lastButOneP02([Y|Tail]).

% p03: nth element
nthElemP03([],_)	-> [];
nthElemP03([Head|_],1)	-> Head;
nthElemP03([_|Tail],N)	-> nthElemP03(Tail,N-1).

% p04: length of list
lengthP04([])		-> 0;
lengthP04([_|Tail])	-> 1 + lengthP04(Tail).

% p05: reverse a list
reverseP05([])	-> [];
reverseP05([Head|Tail])	-> reverseP05(Tail)++[Head].

reverseTailRecP05(List)		-> reverseTailRecP05(List, []).
reverseTailRecP05([],Rev)	-> Rev;
reverseTailRecP05([Head|Tail],Rev)	-> reverseTailRecP05(Tail,[Head|Rev]).

% p06: is the given string a palindrome ?
isPalindromeP06([])	-> true;
isPalindromeP06([_|[]])		-> true;
isPalindromeP06([Head|Tail])->
	case reverseP05(Tail) of
		[Last|Rest] when Head == Last -> isPalindromeP06(Rest);
		_ -> false
	end.

% alt
isPalindromeAltP06(List) ->
	hlprAreEqualLists(List, reverseP05(List)).
hlprAreEqualLists([],[]) -> true;
hlprAreEqualLists(_,[])	-> false;
hlprAreEqualLists([],_)	-> false;
hlprAreEqualLists([Head|Tail1],[Head|Tail2]) -> hlprAreEqualLists(Tail1,Tail2);
hlprAreEqualLists([Head1|Tail1],[Head2|Tail2]) -> false.
% how does a function (butLast) return two values?

% p07: flatten a list
flattenP07([])	-> [];
flattenP07([Head|Tail])	->
	if is_list(Head)	-> flattenP07(Head) ++ flattenP07(Tail);
	   true	-> [Head|flattenP07(Tail)]
	end.

% p08: eliminate conseq duplicates in list
elimConseqDupsP08([])	-> [];
elimConseqDupsP08([Head|[]]) -> [Head];
elimConseqDupsP08([Head|Tail])	->
	case Tail of [HTail|TTail]	-> 
		if Head == HTail	-> [Head|elimConseqDupsP08(TTail)];
		true	-> [Head|elimConseqDupsP08(Tail)]
		end
	end.

% p09: pack conseq elems into sublist
conseqDupsToSubLstP09([])	-> [];
conseqDupsToSubLstP09([Head|[]])-> [[Head]];
conseqDupsToSubLstP09([Head,Head|Tail]) -> 
	[NewHead|NewTail] = conseqDupsToSubLstP09([Head|Tail]),
	[lists:append([Head],NewHead)|NewTail];
conseqDupsToSubLstP09([First,Second|Tail]) ->
	[[First]|conseqDupsToSubLstP09([Second|Tail])].

% p10: runlength encoding
runLenEncP10([])	-> [];
runLenEncP10([Head|[]])	-> [[1,Head]];
runLenEncP10([Head,Head|Tail])	-> 
	[[RLCount|RLHead]|EncTail] = runLenEncP10([Head|Tail]),
	[[RLCount+1|RLHead]|EncTail];
runLenEncP10([First,Second|Tail])	->
	[[1, First]|runLenEncP10([Second|Tail])].

% p11: modify runlength encoding to not record numbers for unit length
modRunLenEncP11([])	-> [];
modRunLenEncP11([Head|[]])	-> [Head];
modRunLenEncP11([Head,Head|Tail])	->
	case modRunLenEncP11([Head|Tail]) of
		[[RLCount|RLHead]|EncTail]	->
			[[RLCount+1|RLHead]|EncTail];
		[RLHead|EncTail]	->
			[[2,RLHead]|EncTail]
	end;
modRunLenEncP11([First,Second|Tail])	->
	[First|modRunLenEncP11([Second|Tail])].

% p12: decode runlength encoded list
runLenDecP12([])	-> [];
runLenDecP12([[RLCount,RLHead]|Tail])	->
	lists:append(hlprNTimes(RLCount,RLHead), runLenDecP12(Tail));
runLenDecP12([Head|Tail])	-> [Head|runLenDecP12(Tail)].

hlprNTimes(1,Symbol)	-> [Symbol];
hlprNTimes(Count,Symbol) ->
	lists:append([Symbol],hlprNTimes(Count-1,Symbol)).

% p13: runlength encode without explicitly creating sublists

% p14: duplicate elements of a list
duplicateP14([])	-> [];
duplicateP14([Head|[]]) -> [Head,Head];
duplicateP14([Head|Tail])	->
	lists:append(duplicateP14([Head]), duplicateP14(Tail)).

% p15: replicate N times
replicateNP15([],_)	-> [];
replicateNP15([Head|Tail],N)	->
	lists:append(hlprNTimes(N,Head), replicateNP15(Tail,N)).

% p16: remove every Nth element
dropEveryNthP16(List,N)	-> dropEveryNthP16(List,N,N).
dropEveryNthP16([],_,_)	-> [];
dropEveryNthP16([_|Tail],N,1)	-> dropEveryNthP16(Tail,N,N);
dropEveryNthP16([Head|Tail],N,Counter)	->
	lists:append([Head],dropEveryNthP16(Tail,N,Counter-1)).

% p17: split list at given length
splitAtNP17(List,N) -> splitAtNP17([],List,N).
splitAtNP17(First,[],_)	-> [First,[]];
splitAtNP17(First,[Head|Tail],0)	-> [First,[Head|Tail]];
splitAtNP17(First,[Head|Tail],N)	-> splitAtNP17(lists:append(First,[Head]), Tail, N-1).

% p18: extract slice from a given list
sliceP18(List,Begin,End)	-> sliceP18([],List,Begin,End,1).
sliceP18(Acc,[],_,_,_)	-> Acc;
sliceP18(Acc,[Head|Tail],Begin,End,Counter)	->
	if Counter < Begin	-> sliceP18(Acc,Tail,Begin,End,Counter+1);
	Counter >= Begin andalso Counter =< End 	->
		sliceP18(lists:append(Acc,[Head]), Tail, Begin, End, Counter+1);
	Counter > End 	-> Acc
	end.

% p19:  rotate list N times to left 
rotateNP19(List,0)	-> List;
rotateNP19([Head|Tail],N)	-> rotateNP19(lists:append(Tail,[Head]), N-1).

rotateNP19Alt(List,0)	-> List;
rotateNP19Alt(List,N)	->
	[First,Second] = splitAtNP17(List,N),
	lists:append(Second,First).

% p20: remove kth element of the list
removeAtNP20([],_)	-> [];
removeAtNP20([_|Tail],1)	-> Tail;
removeAtNP20([Head|Tail], N)	-> lists:append([Head],removeAtNP20(Tail,N-1)).

% p21: insert symbol at Nth place in a list
insertAtNP21([],Symbol,_)	-> [Symbol];
insertAtNP21([Head|Tail], Symbol, 1)	-> lists:append([Symbol,Head],Tail);
insertAtNP21([Head|Tail], Symbol, N)	-> lists:append([Head],insertAtNP21(Tail,Symbol,N-1)).

% p22: all integers in range
integersInRangeP22(Begin,End) ->
	if Begin==End 	-> [Begin];
	Begin < End 	-> lists:append([Begin], integersInRangeP22(Begin+1,End));
	Begin > End 	-> lists:append([Begin], integersInRangeP22(Begin-1,End))
	end.

% p23: return N randomly selected elements
randSelectNP23([],_)	-> [];
randSelectNP23(List,N)	-> [List,N].
% need to figure out how to load the random module
% random:uniform(N).

% p24: 