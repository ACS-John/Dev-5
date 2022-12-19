autoLibrary
on error goto Ertn
fnTop(program$)

dim dat$*20
fndat(dat$)
dim idx$(3)*20
idx$(1)='ubIndex'
idx$(2)='ubIndx2'
idx$(3)='ubIndx3'

SCR1: ! r:
	dim resp$(10)*20
	fnTos
	respc=0
	mylen=22
	mypos=mylen+2
	fnLbl(1,1,'Sort by:',mylen,1)
	dim wrd2$(3)
	wrd2$(1)='Account'
	wrd2$(2)='Customer Name'
	wrd2$(3)='Street'
	fnComboA('bs',1,mypos,mat wrd2$)
	resp$(respc+=1)=wrd2$(1)
	fnLbl(2,1,'Report Heading Date:',mylen,1)
	fnTxt(2,mypos,20)
	resp$(respc+=1)=dat$
	fnLbl(3,1,'Print:',mylen,1)
	dim sel$(3)*38
	sel$(1)='Active customers'
	sel$(2)='Inactive customers'
	sel$(3)='[All]'
	fnComboA('bs2',3,mypos,mat sel$)
	resp$(respc+=1)=sel$(1)
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	if resp$(1)=wrd2$(1) then q0=1 else if resp$(1)=wrd2$(2) then q0=2 else if resp$(1)=wrd2$(3) then q0=3 ! sort by
	dat$=resp$(2)
	fndat(dat$,2)
	if resp$(3)=sel$(1) then ti2=1 else if resp$(3)=sel$(2) then ti2=2 else if resp$(3)=sel$(3) then ti2=3 ! active, inactive, etc...
	on fkey 5 goto DONE
	fnOpenPrn
	open #1: 'Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\'&idx$(q0)&'.h[cno],Shr',i,i,k
	gosub BUD1
	gosub HEADER
goto ReadCustomer ! /r
ReadCustomer: ! r:
	dim z$*10,e$(4)*30
	read #1,using L430: z$,mat e$,final,bal eof DONE
	if hBudMstr then gosub BUD2
	if totba=0 then goto ReadCustomer
	L430: form pos 1,c 10,pos 11,4*c 30,pos 1821,n 1,pos 292,pd 4.2
	if ti2=3 then goto L470
	if ti2=1 and final><0 then goto ReadCustomer
	if ti2=2 and final=0 then goto ReadCustomer
	L470: !
	pr #255,using L480: z$,e$(2),e$(1)(1:25),totba pageoflow PgOf
	L480: form x 5,c 10,x 5,c 30,x 7,c 25,n 11.2,skip 2
goto ReadCustomer ! /r
PgOf: ! r:
	pr #255: newpage
	gosub HEADER
goto ReadCustomer ! /r
HEADER: ! r:
	p2=p2+1
	pr #255: '\qc {\fs24 '&env$('cnam')&'}'
	! pr #255: '\qc {\fs28 {\b '&env$('program_caption')&'}}'
	pr #255: '\qc {\fs28 {\b Budget Customer List }}'
	pr #255: '\qc {\fs24 '& dat$&'}'
	pr #255: '\qc {\fs20 '&date$('mm/dd/yy')&'   '&time$ &'   Page '&str$(p2)&'}'
	pr #255: ''
	pr #255: tab(7);'Account No';tab(21);'Name';tab(58);'Meter Address';tab(81);'Budget Amount'
return ! /r
DONE: !
	close #1: ioerr ignore
	fnClosePrn
Xit: fnXit

BUD1: ! r:
	dim ba(13),badr(2),bt1(14,2),bd1(5),bd2(5),bd3(5),bd$(5)*30
	hBudMstr=fnOpenBudMstrInput
return ! /r
BUD2: ! r:
	totba=0
	if hBudMstr then
		read #hBudMstr,using L820,key=z$: z$,mat ba,mat badr nokey L860
		L820: form pos 1,c 10,pd 4,12*pd 5.2,2*pd 3
		if ba(12)>0 then totba=ba(12): goto L860 ! TOTAL BILL BUDGETED
		for j=2 to 12: totba=totba+ba(j): next j
		if env$('client')='Findlay' then totba=totba-ba(8) ! don't add the penalty
	end if
	L860: !
return ! /r

include: ertn

