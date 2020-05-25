! Replace S:\acsUB\UBowed
 
	autoLibrary
	on error goto Ertn
 
	dim dat$*20,message$*40,resp$(5)*20
	dim z$*10,e$*30,g(12),adr(2),o(2),e(5),rt(10,3)
	dim cap$*128,firstday(3),lastday(3),month(4)
 
	fnTop(program$,cap$="Balance Breakdown Aged by Month")
	fndat(dat$,1)
 
 
	fnTos(sn$="ubowed")
	respc=0 : mylen=29 : mypos=mylen+2 : frac=0
	fnFra(1,1,3,mypos+14,"Aging Dates","Use the last day of each month for your aging dates.") : fraaging=frac+=1
	fnLbl(1,1,"Last Day of Current Month:",mylen,1,0,fraaging)
	fnTxt(1,mypos,10,10,0,"3",0,"ccyymmdd",fraaging) : _
	resp$(respc+=1)=''
	fnLbl(2,1,"Last Day of Last Month:",mylen,1,0,fraaging)
	fnTxt(2,mypos,10,10,0,"3",0,"ccyymmdd",fraaging) : _
	resp$(respc+=1)=''
	fnLbl(3,1,"Last Day of Third Month:",mylen,1,0,fraaging)
	fnTxt(3,mypos,10,10,0,"3",0,"ccyymmdd",fraaging) : _
	resp$(respc+=1)=''
	fnLbl(7,1,"Report Heading Dage:",mylen,1)
	fnTxt(7,mypos,20) : _
	resp$(respc+=1)=dat$
	fnChk(9,mypos+10,"Skip customers with credit balance:",1) : _
	resp$(respc+=1)="False"
	fnCmdSet(3) : _
	fnAcs2(mat resp$,ckey,1)
	if ckey=5 then goto Xit
	for j=1 to 3
! x=POS(RESP$(J),"/",1)
! If X>0 Then rESP$(J)(X:X)="": Goto 300
		lastday(j)=val(resp$(j))
		firstday(j)=(val(resp$(j)(1:6))*100)+1
	next j
	dat$=resp$(4) : _
	fndat(dat$,2)
	if resp$(5)="True" then skipcr=1
 
PRINTING: !
	on fkey 5 goto DONE
	fnopenprn
	gosub HEADER
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed
	open #2: "Name=[Q]\UBmstr\UBTransVB.h[cno],KFName=[Q]\UBmstr\UBTrIndx.h[cno],Shr",internal,input,keyed
READ_CUSTOMER: !
L460: read #1,using 'Form POS 1,C 10,POS 41,C 30,POS 155,PD 2,POS 292,PD 4.2,POS 296,PD 4,POS 300,12*PD 4.2,POS 348': z$,e$,a7,bal,f,mat g eof L640
	if bal=0 then goto L460
	if bal<0 and skipcr=1 then goto READ_CUSTOMER
	if bal<0 then mat month=(0): month(1)=bal: goto L510
	gosub READ_TRANS
L510: if sum(month)=(0) and bal<>0 then month(4)=bal
	pr #255,using 'Form POS 1,C 12,C 20,5*N 11.2': z$,e$(1:20),month(1),month(2),month(3),month(4),bal pageoflow PGOF
	totcolumn1+=month(1) : totcolumn2+=month(2) : _
	totcolumn3+=month(3) : totcolumn4+=month(4) : totbal+=bal
	goto READ_CUSTOMER
 
HEADER: !
	pr #255: "\qc  {\f181 \fs22 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs28 \b "&env$('program_caption')&"}"
	pr #255: "\qc  {\f181 \fs22 \b "&trim$(dat$)&"}"
	pr #255,using 'Form POS 1,C 82,C 9': "\ql "&date$,"Page "&str$(p2+=1)
	pr #255: "{\ul Account No}  {\ul Customer Name}        {\ul "&(cnvrt$("PIC(zzZZ/ZZ/ZZ)",lastday(1)))(1:10)&"} {\ul "&(cnvrt$("PIC(zzZZ/ZZ/ZZ)",lastday(2)))(1:10)&"} {\ul "&(cnvrt$("PIC(zzZZ/ZZ/ZZ)",lastday(3)))(1:10)&"} {\ul      Older} {\ul    Balance}"
return
 
L640: pr #255: "                                 __________ __________ __________ __________ __________"
	pr #255,using 'Form POS 1,C 12,C 20,5*N 11.2': "","",totcolumn1,totcolumn2,totcolumn3,totcolumn4,totbal pageoflow PGOF
	pr #255: "                                 {\ul \strike           } {\ul \strike           } {\ul \strike           } {\ul \strike           } {\ul \strike           }"
DONE: close #1: ioerr L680
L680: close #2: ioerr L690
L690: fncloseprn
Xit: fnXit
 
PGOF: pr #255: newpage
	gosub HEADER
	continue
 
READ_TRANS: !
	sortreq=0 : mat month=(0)
	restore #2,key>=z$&"         ": nokey L990
L790: read #2,using 'Form POS 1,C 10,N 8,N 1,PD 4.2': p$,tdate,tcode,tamount eof L990
	if p$<>z$ then goto L880
	if tcode =3 or tcode=4 then goto L790 ! skip collections or cm
	for j=1 to 3
		if tdate<firstday(3) then goto L790 ! older than we want to analyze
		if tdate>=firstday(j) and tdate<=lastday(j) then : _
			month(j)+=tamount : goto L790
		if tdate>lastday(1) then : _
			month(1)+=tamount : goto L790 : _
			! if trans dated after the last day of the first month, : _
			! just go ahead and put it the first month
	next j
	goto L790
L880: tempbal=bal
	if month(1)=tempbal then tempbal=0: month: month(2)=month(3)=month(4)=0: goto L990
	if month(1)<tempbal then tempbal=tempbal-month(1): goto L920
	if month(1)>tempbal then month(1)=tempbal: month(2)=month(3)=month(4)=0: goto L990
L920: if month(2)=tempbal then tempbal=0: month(3)=month(4)=0: goto L990
	if month(2)<tempbal then tempbal=tempbal-month(2): goto L950
	if month(2)>tempbal then month(2)=tempbal: month(3)=month(4)=0: goto L990
L950: if month(3)=tempbal then tempbal=0: month(4)=0: goto L990
	if month(3)<tempbal then tempbal=tempbal-month(3): goto L980
	if month(3)>tempbal then month(3)=tempbal: month(4)=0: goto L990
L980: month(4)=tempbal
L990: return
 
include: Ertn
 
