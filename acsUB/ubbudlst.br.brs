! Replace S:\acsUB\ubBudLst
! -- Budget Customer List
 
	autoLibrary
	on error goto Ertn
 
	dim z$*10,e$(4)*30,dat$*20,idx$(3)*20,wrd2$(3)
	dim sel$(3)*38,cap$*128,txt$*40,resp$(10)*20
 
	fndat(dat$)
	fnTop(program$,cap$="Customer List")
	idx$(1)="ubIndex" : _
	idx$(2)="ubIndx2" : _
	idx$(3)="ubIndx3"
 
SCR1: !
	sn$="ubBudLst" : _
	fnTos(sn$) : _
	respc=0
	mylen=22 : _
	mypos=mylen+2
	fnLbl(1,1,"Sort by:",mylen,1)
	wrd2$(1)="Account" : _
	wrd2$(2)="Customer Name" : _
	wrd2$(3)="Street" : _
	fncomboa("bs",1,mypos,mat wrd2$) : _
	resp$(respc+=1)=wrd2$(1)
	fnLbl(2,1,"Report Heading Date:",mylen,1)
	fnTxt(2,mypos,20) : _
	resp$(respc+=1)=dat$
	fnLbl(3,1,"Print:",mylen,1)
	sel$(1)="Active customers" : _
	sel$(2)="Inactive customers" : _
	sel$(3)="[All]" : _
	fncomboa("bs2",3,mypos,mat sel$) : _
	resp$(respc+=1)=sel$(1)
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	if resp$(1)=wrd2$(1) then q0=1 else : _
		if resp$(1)=wrd2$(2) then q0=2 else : _
			if resp$(1)=wrd2$(3) then q0=3 ! sort by
	dat$=resp$(2) : _
	fndat(dat$,2)
	if resp$(3)=sel$(1) then ti2=1 else : _
		if resp$(3)=sel$(2) then ti2=2 else : _
			if resp$(3)=sel$(3) then ti2=3 ! active, inactive, etc...
 
	on fkey 5 goto DONE
	fnopenprn
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\"&idx$(q0)&".h[cno],Shr",internal,input,keyed
	gosub BUD1
	gosub HEADER
	goto READ_CUSTOMER
 
READ_CUSTOMER: !
	read #1,using L430: z$,mat e$,final,bal eof DONE
	if bud1=1 then gosub BUD2
	if totba=0 then goto READ_CUSTOMER
L430: form pos 1,c 10,pos 11,4*c 30,pos 1821,n 1,pos 292,pd 4.2
	if ti2=3 then goto L470
	if ti2=1 and final><0 then goto READ_CUSTOMER
	if ti2=2 and final=0 then goto READ_CUSTOMER
L470: pr #255,using L480: z$,e$(2),e$(1)(1:25),totba pageoflow PGOF
L480: form x 5,c 10,x 5,c 30,x 7,c 25,n 11.2,skip 2
	goto READ_CUSTOMER
 
PGOF: !
	pr #255: newpage
	gosub HEADER
	goto READ_CUSTOMER
 
HEADER: !
	p2=p2+1
	pr #255: "\qc {\fs24 "&env$('cnam')&"}"
! pr #255: "\qc {\fs28 {\b "&env$('program_caption')&"}}"
	pr #255: "\qc {\fs28 {\b Budget Customer List }}"
	pr #255: "\qc {\fs24 "& dat$&"}"
	pr #255: "\qc {\fs20 "&date$("mm/dd/yy")&"   "&time$ &"   Page "&str$(p2)&"}"
	pr #255: ""
	pr #255: tab(7);"Account No";tab(21);"Name";tab(58);"Meter Address";tab(81);"Budget Amount"
return
 
DONE: close #1: ioerr L680
L680: fncloseprn
Xit: fnXit
 
BUD1: bud1=0
	dim ba(13),badr(2),bt1(14,2),bd1(5),bd2(5),bd3(5),bd$(5)*30
	open #81: "Name=[Q]\UBmstr\BudMstr.h[cno],KFName=[Q]\UBmstr\BudIdx1.h[cno],Shr",internal,outIn,keyed ioerr L760
	open #82: "Name=[Q]\UBmstr\BudTrans.h[cno],Shr",internal,outIn,relative
	bud1=1
L760: return
 
BUD2: !
	totba=0
	if bud1=0 then goto L860
	read #81,using L820,key=z$: z$,mat ba,mat badr nokey L860
L820: form pos 1,c 10,pd 4,12*pd 5.2,2*pd 3
	if ba(12)>0 then totba=ba(12): goto L860 ! TOTAL BILL BUDGETED
	for j=2 to 12: totba=totba+ba(j): next j
	if env$('client')="Findlay" then totba=totba-ba(8) ! don't add the penalty
L860: return
 
include: ertn
 
