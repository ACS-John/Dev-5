! formerly S:\acsUB\ubmeans.br

	autoLibrary

	on error goto Ertn

	dim cd1(8),x(13),txt$*60,message$(5)*80,message$*60,tg(11)

	fnTop(program$)

	open #2: "Name=[Q]\UBmstr\UBTransVB.h[cno],KFName=[Q]\UBmstr\UBTrIndx.h[cno],Shr",i,i,k 
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",i,outIn,k 
	open #5: "Name=[Q]\UBmstr\MEANs.h[cno],RecL=22,REPLACE",internal,output 
	read #1,using L490: x$,a2 eof DONE
	restore #2,key>=x$&"         ": nokey L230
L170: read #2,using L540: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof L230
	if p$<>x$ then goto L230           ! history record must belong to this customer
	if tcode<>1 then goto L170 ! charge transaction
	j=j+1 
	if j>8 then goto L230
	resp$(j)=str$(tdate)
	goto L170
L230: restore #1: 

SCR1: ! 
	fnTos(sn$:='means-1') 
	mylen=62 : mypos=50
	txt$="Billing Dates for Months to be Considered:" 
	fnLbl(1,1,txt$,mylen,1)
	for j=1 to 8 
		fnTxt(j+1,mypos,10,0,0,"3") 
		resp$(j)="" 
	next j
	fnCmdSet(2): ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	for j=1 to 8
L320: x=pos(resp$(j),"/",1)
		if x>0 then resp$(j)(x:x)="": goto L320
	next j
	for j=1 to 8 
		cd1(j)=val(resp$(j)) conv SCR1 
	next j
	if cd1(1)=0 then 
		mat message$(1): mytype=0 
		message$(1)="You must enter at least one date!" 
		fnMsgBox(mat message$,resp$,'',mytype) 
		goto SCR1
	end if

SCR2: ! 
	sn$="Means-2" 
	fnTos(sn$)
	txt$="Sewer code to analyze:" 
	fnLbl(1,1,txt$,22,1)
	fnTxt(1,24,2,2,0,"20") 
	resp$(1)=""
	fnCmdSet(2): ckey=fnAcs(mat resp$)
	if ckey=5 then goto SCR1
	sewcode=val(resp$(1)) conv SCR2
	if sewcode=0 then 
		mat message$(1): mytype=0 
		message$(1)="You must enter at least one date!" 
		fnMsgBox(mat message$,resp$,'',mytype) 
		goto SCR2
	end if
	fnopenprn
L470: read #1,using L490: x$,a2 eof DONE
	if a2<>sewcode then goto L470 ! only average certain rate codes
L490: form pos 1,c 10,pos 145,pd 2,pos 1822,n 9
	t1=t2=t3=x=0
	mat x=(0)
	restore #2,key>=x$&"         ": nokey L470
L530: read #2,using L540: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof L620
L540: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
	if p$<>x$ then goto L620
	if tcode<>1 then goto L530 ! only charge transactions
	for j1=1 to 8
		if cd1(j1)=tdate then 
			t1=t1+1
			t2=t2+wu 
			x=x+1
			x(x)=wu 
			goto L530
		end if
	next j1
	goto L530

L620: if t1>0 then t3=int(t2/t1) else t3=0
	write #5,using "form pos 1,C 10,N 12.2": x$,t3
	goto L470

DONE: ! 
	gosub PRINT_REPORT
	fncloseprn
Xit: fnXit



PRINT_REPORT: ! 
	close #1: ioerr L820
L820: close #2: ioerr L830
L830: close #5: ioerr L840
L840: execute "Index [Q]\UBmstr\MEANs.h[cno],[Q]\UBmstr\MEANIDX.h[cno] 11,12,REPLACE,DupKeys -n"
	open #5: "Name=[Q]\UBmstr\MEANs.h[cno],KFName=[Q]\UBmstr\MEANIDX.h[cno],Shr",i,outIn,k 
	gosub HEADER
	means=int(lrec(5)/2)
L880: read #5,using "form pos 1,C 10,N 12.2": z$,t3 eof L940
	x=x+1
	pr #255,using L910: z$,t3 pageoflow L950
L910: form pos 11,c 10,x 3,n 12.2,skip 1
	if means=x then pr #255: "This should be the halfway point"
	goto L880
L940: return 
L950: pr #255: newpage
	gosub HEADER
continue 
HEADER: ! r:
	pr #255,using L1000: date$,env$('cnam'),time$,"Median Sewer Charge"
	L1000: form skip 2,pos 1,c 10,pos 20,cc 40,skip 1,pos 1,c 10,pos 20,cc 40,skip 2
return ! /r

include: ertn
