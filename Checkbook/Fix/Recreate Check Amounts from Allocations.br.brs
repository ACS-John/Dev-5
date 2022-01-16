! read check amount using allocations
! GLT: 1=Post  2=Print Only
autoLibrary
dim de$*30

open #trmstr=1: 'Name=[Q]\CLmstr\TrMstr.h[cno],KFName=[Q]\CLmstr\TrIdx1.h[cno],Shr',internal,outIn,keyed
open #tralloc=3: 'Name=[Q]\CLmstr\TrAlloc.h[cno],KFName=[Q]\CLmstr\tralloc-idx.h[cno],Shr',internal,outIn,keyed
open #bankmstr=4: 'Name=[Q]\CLmstr\BankMstr.h[cno],KFName=[Q]\CLmstr\BankIdx1.h[cno],Shr',internal,outIn,keyed
open #work=5: 'Name=[Temp]\Work.[Session],SIZE=0,RecL=76,Replace',internal,output
do
	READ_TRMSTR: !
	holdck$=checkNumber$
	L200: read #trmstr,using 'form pos 1,n 2,n 1,C 8,N 6,PD 10.2,pos 28,C 8,C 30,pos 71,N 1,X 6,N 1': trbank_code,trtcde,checkNumber$,pd,ca1,vn$,de$,pcde,scd eof Finis
	if checkNumber$=holdck$ then delete #trmstr: : goto L200
	restore #tralloc,key>=cnvrt$('pic(zz)',trbank_code)&cnvrt$('pic(#)',trtcde)&checkNumber$: ! Nokey 210
	totalalloc=0
	READ_TRALLOC: !
	do
		read #tralloc,using 'form pos 1,N 2,N 1,c 8,C 12,PD 5.2,C 12,X 18,N 6,pos 80,N 1': bank_code,tcde,trck$,gl$,amt,iv$,ivd,gde eof READ_TRMSTR
		if trbank_code=bank_code and tcde=trtcde and checkNumber$=trck$ then goto L270 else goto L300
		L270: totalalloc+=amt
		if amt=ca1 then foundone=1
	loop
	L300: !
	if totalalloc<>ca1 and foundone=1 then ca1=totalalloc
	rewrite #trmstr,using 'form pos 1,n 2,n 1,C 8,N 6,PD 10.2,pos 28,C 8,C 30,pos 71,N 1,X 6,N 1': trbank_code,trtcde,checkNumber$,pd,ca1,vn$,de$,pcde,scd
	foundone=0
loop
Finis: !
Xit: fnXit
