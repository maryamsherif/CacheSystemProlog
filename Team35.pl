%General
convertBinToDec(Bin,Dec):-
helper_convert(Bin,0,0,Dec).


helper_convert(0,_,Sum,Sum).


helper_convert(Bin,Power,Sum,Dec):-
				Bin > 0,
				Bin1 is Bin mod 10,
				Sum1 is Sum + 2 ** Power * Bin1,
				Power1 is Power +1,
				Bin2 is Bin // 10,
				helper_convert(Bin2,Power1,Sum1,Dec).
				
	     
replaceIthItem(_,[],_,[]).
replaceIthItem(Item,[H|T],0,[Item|T]).
replaceIthItem(Item,[H|T],I,[H|T2]):-
			I \=0,
			I1 is I -1,
			replaceIthItem(Item,T,I1,T2).


splitEvery(_,[],[]).
splitEvery(0,[H|T],[H|T]).			
splitEvery(N,[H|T],Res):-
					N>0,
					splitHelper(0,N,[],[],[H|T],Res).

splitHelper(N,N,Acc1,Acc3,[],Res):- reverse([Acc1|Acc3],Res). 
splitHelper(M,N,_,Acc3,[],Res):- M\=N, reverse(Acc3, Res). 

splitHelper(C,N,Acc1,Acc2,[H|T],Res):-
					C <N,
					append(Acc1,[H],Accres),
					C1 is C+1,
					splitHelper(C1,N,Accres,Acc2,T,Res).
					
splitHelper(C,N,Acc1,Acc2,[H|T],Res):-
					C=N,
					Acc3 =[Acc1|Acc2],
					splitHelper(0,N,[],Acc3,[H|T],Res).
					
logBase2(1,0).
logBase2(Num,Power):-	
			Num\=1,
			helper_log(Num,1,Power).
helper_log(Num,P,Power):-
			Num\=1,
			N is 2**P,
			N \= Num,
			P1 is P+1,
			helper_log(Num,P1,Power).
			
helper_log(Num,P,Power):-
			Num\=1,
			N is 2**P,
			N = Num,
			Power=P.
			
getNumBits(_,fullyAssoc,_,0).
getNumBits(_,directMap,Cache,BitsNum):-
length(Cache,Num),
logBase2(Num,BitsNum).

getNumBits(N,setAssoc,Cache,BitsNum):-
logBase2(N,BitsNum).
						
						
fillZeros(String,0,String).
fillZeros(String,N,String4):-		
				N\=0,
				string_concat("0", String, String3),
				N1 is N-1,
				fillZeros(String3,N1,String4).				

			
%ImplementedPredicates	
getData(StringAddress,OldCache,Mem,NewCache,Data,HopsNum,Type,BitsNum,hit):-
getDataFromCache(StringAddress,OldCache,Data,HopsNum,Type,BitsNum),
NewCache = OldCache.

getData(StringAddress,OldCache,Mem,NewCache,Data,HopsNum,Type,BitsNum,miss):-
\+getDataFromCache(StringAddress,OldCache,Data,HopsNum,Type,BitsNum),
atom_number(StringAddress,Address),
convertAddress(Address,BitsNum,Tag,Idx,Type),
replaceInCache(Tag,Idx,Mem,OldCache,NewCache,Data,Type,BitsNum).

runProgram([],OldCache,_,OldCache,[],[],Type,_).

runProgram([Address|AdressList],OldCache,Mem,FinalCache,
[Data|OutputDataList],[Status|StatusList],Type,NumOfSets):-
getNumBits(NumOfSets,Type,OldCache,BitsNum),
getData(Address,OldCache,Mem,NewCache,Data,HopsNum,Type,BitsNum,Status),
runProgram(AdressList,NewCache,Mem,FinalCache,OutputDataList,StatusList,
Type,NumOfSets).
			


%DirectMap
getDataFromCache(StringAddress,Cache,Data1,0,directMap,BitsNum):-
atom_number(StringAddress,Num),							convertAddress(Num,BitsNum,Tag,Idx,directMap),
convertBinToDec(Idx,I),nth0(I,Cache,Item1),
getTag_direct(Item1,NewTag),
atom_number(NewTag,Tag1),
Tag1 =:= Tag,
get_DataDirect(Item1,Data1).
get_Data(item(Tag,data(X),1,Order),X).

get_DataDirect(item(tag(_),data(X),1,Order),X).
getTag_direct(item(tag(X),data(_),V,Idx),X).
getTag([item(tag(X),data(Z),ValidBit,Order)],X).
					
convertAddress(L,0,L,_,directMap).
convertAddress(Bin,BitsNum,Tag,Idx,directMap):-
				BitsNum>0,
				Idx is Bin mod (10**BitsNum ),
				Tag is Bin//(10**BitsNum).

replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData,directMap,BitsNum):-
				number_string(Tag,T),
				number_string(Idx,I1),
				string_concat(T,I1,IdxMem),
				atom_number(IdxMem,IdxMem1),
				convertBinToDec(IdxMem1,IdxMem1New),
				convertBinToDec(Idx,I),
				nth0(IdxMem1New,Mem,ItemData),
				number_string(Tag,Y),
				fillZeros(Tag,(6-BitsNum),W),
				replaceIthItem(item(tag(W),data(ItemData),1,0),OldCache,I,NewCache).


%FullyAssoc
getDataFromCache(StringAddress,[H|T],Data,HopsNum,fullyAssoc,_):-
					helper_getDataFull(StringAddress,[H|T],HopsNum,Data).			


								
helper_getDataFull(StringAddress,[H|T],HopsNum,Data):-
							atom_number(StringAddress,Num),
							getTag([H],X),
							atom_number(X,Z),
							Num \= Z,
							helper_getDataFull(StringAddress,T,HopsNum1,Data),
							HopsNum is HopsNum1+1.
							
helper_getDataFull(StringAddress,[H|T],0,D):-	
						Item=[H],
						atom_number(StringAddress,Num),
						getTag([H],X),
						atom_number(X,Z),
						Num = Z,
						get_Data(Item,D).

convertAddress(Bin,BitsNum,Bin,_,fullyAssoc).


findInvalid([item(_,_,0,_)|T],0).
findInvalid([item(_,_,1,_)|T],R):-
		findInvalid(T,R1),
		R is 1+R1.

replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData,fullyAssoc,BitsNum):-
				convertBinToDec(Tag,I),
				nth0(I,Mem,ItemData),
				atom_chars(Tag, TagList),length(TagList,LentagOld),
				P1 is (6-LentagOld),
				fillZeros(Tag,P1,NTag),
				findInvalid(OldCache,I1),
				replaceIthItem(item(tag(NTag),data(ItemData),1,0),OldCache,I1,NewCache).

replaceInCache(Tag,Idx,Mem,OldCache,Res,ItemData,fullyAssoc,BitsNum):-
				\+findInvalid(OldCache,I1),
				check_ValidityList(OldCache),
				atom_chars(Tag, TagList),length(TagList,LentagOld),
				P1 is (6-LentagOld),
				fillZeros(Tag,P1,NTag),
				helper_getOrder(OldCache,Max),
				helper_Idx(OldCache,Max,I2),
				increment_Order(OldCache,Res),
				replaceIthItem(item(tag(NTag),data(ItemData),1,0),Res,I2,NewCache).
				
			
check_ValidityList([]).
check_ValidityList([H|T]):-
				H=item(tag(A),data(D),1,O),
		         check_ValidityList(T).



increment_Order([H|T],[NItem1|Res]):-
					H= item(tag(A),data(D),V,O1),
					O1\=0,
					O2 is O1+1,
					length([H|T],L),
					O2 \=L,
					NItem1=item(tag(A),data(D),V,O2),
					increment_Order(T,Res).
										
increment_Order([],Res).					
					
get_Order(item(tag(X),data(Y),1,Order),Order).

helper_getOrder([],0).
helper_getOrder([item(tag(X),data(Y),V,O)|T],Max):-
				helper_getOrder(T,Max1),
				max(O,Max1,Max).
max(A,B,A):- A>B.
max(A,B,B):- B>=A.
								
helper_Idx([H|T],Max,Idx):-
			nth0(Idx,[H|T],item(tag(X),data(Y),V,Max)).
				

%SetAssoc
checkvalid(item(tag(_),data(_),1,_)).	
getDataFromCache(StringAddress,[H|T],Data,HopsNum,setAssoc,SetsNum):-
				splitEvery(SetsNum,[H|T],Res),
				atom_number(StringAddress,Num),
				convertAddress(Num,SetsNum,Tag,Idx,setAssoc),
				atom_chars(Tag, TagList),length(TagList,LentagOld),
				logBase2(SetsNum,P),
				P1 is (6-P-LentagOld),
				fillZeros(Tag,P1,NTag),
				convertBinToDec(Idx,Idx1),
				nth0(Idx1,Res,ItemSet),
				helper_getDataSet(ItemSet,NTag,0,Item1,HopsNum),
				get_Data(Item1,Data).
				
helper_getDataSet([item(tag(Y),data(Z),1,_)|T],Y,C,Item1,0):-
				Item1 = item(tag(Y),data(Z),1,X).
				%getTag_direct(Item1,Tag1),
				%NTag == Tag1.
				
helper_getDataSet([item(tag(Y),data(Z),0,_)|T],NTag,C,Item1,HopsNum):-
				Y\=NTag,
				C1 is C+1,
				helper_getDataSet(T,NTag,C1,Item1,HopsNum1),
				HopsNums is HopsNum1 +1.
				

convertAddress(Bin,SetsNum,Tag,Idx,setAssoc):-
			logBase2(SetsNum,IdxNum1),
			Idx is Bin mod (10**IdxNum1),
			Tag is Bin // (10**IdxNum1).
			
replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData,setAssoc,SetsNum):-
				splitEvery(SetsNum,OldCache,Res),
				number_string(Tag,T),
				number_string(Idx,I1),
				string_concat(T,I1,IdxMem),
				atom_number(IdxMem,IdxMem1),
				convertBinToDec(IdxMem1,IdxMem1New),
				convertBinToDec(Idx,I),
				nth0(IdxMem1New,Mem,ItemData),
				nth0(I,OldCache,ItemSet),
				atom_chars(Tag, TagList),length(TagList,LentagOld),
				logBase2(SetsNum,P),
				P1 is (6-P-LentagOld),
				fillZeros(Tag,P1,NTag),
				findInvalid(ItemSet,I2),
				replaceIthItem(item(tag(NTag),data(ItemData),1,0),OldCache,I2,NewCache).			

	replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData,setAssoc,SetsNum):-
				splitEvery(SetsNum,OldCache,Res),
				number_string(Tag,T),
				number_string(Idx,I1),
				string_concat(T,I1,IdxMem),
				atom_number(IdxMem,IdxMem1),
				convertBinToDec(IdxMem1,IdxMem1New),
				convertBinToDec(Idx,I),
				nth0(IdxMem1New,Mem,ItemData),
				nth0(I,OldCache,ItemSet),
				\+findInvalid(ItemSet,I2),
				atom_chars(Tag, TagList),length(TagList,LentagOld),
				logBase2(SetsNum,P),
				P1 is (6-P-LentagOld),
				fillZeros(Tag,P1,NTag),
				check_ValidityList(ItemSet),
				helper_getOrder(ItemSet,Max),
				helper_Idx(ItemSet,Max,I3),
				increment_Order(ItemSet,Res),
				replaceIthItem(item(tag(NTag),data(ItemData),1,0),Res,I3,NewCache).	
			
				 