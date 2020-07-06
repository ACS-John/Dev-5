! formerly S:\acsUB\UBowed


autoLibrary
on error goto Ertn

dim dat$*20,message$*40,resp$(5)*20
dim firstday(3),lastday(3),month(4)

	fnTop(program$)
fndat(dat$,1)


fnTos
respc=0 : mylen=29 : mypos=mylen+2 : frac=0
fnFra(1,1,3,mypos+14,"Aging Dates","Use the last day of each month for your aging dates.") : fraaging=frac+=1
fnLbl(1,1,"Last Day of Current Month:",mylen,1,0,fraaging)
fnTxt(1,mypos,10,10,0,"3",0,"ccyymmdd",fraaging)
resp$(respc+=1)=''
fnLbl(2,1,"Last Day of Last Month:",mylen,1,0,fraaging)
fnTxt(2,mypos,10,10,0,"3",0,"ccyymmdd",fraaging)
resp$(respc+=1)=''
fnLbl(3,1,"Last Day of Third Month:",mylen,1,0,fraaging)
fnTxt(3,mypos,10,10,0,"3",0,"ccyymmdd",fraaging)
resp$(respc+=1)=''
fnLbl(7,1,"Report Heading Dage:",mylen,1)
fnTxt(7,mypos,20)
resp$(respc+=1)=dat$
fnChk(9,mypos+10,"Skip customers with credit balance:",1)
resp$(respc+=1)="False"
fnCmdSet(3)
fnAcs(mat resp$,ckey,1)
if ckey=5 then goto Xit
for j=1 to 3
	! x=POS(RESP$(J),"/",1)
	! If X>0 Then rESP$(J)(X:X)="": Goto 300
	lastday(j)=val(resp$(j))
	firstday(j)=(val(resp$(j)(1:6))*100)+1
next j
dat$=resp$(4)
fndat(dat$,2)
if resp$(5)="True" then skipcr=1


fnopenprn
gosub PrHeader
open #2: "Name=[Q]\UBmstr\UBTransVB.h[cno],KFName=[Q]\UBmstr\UBTrIndx.h[cno],Shr",internal,input,keyed
open #hCustomer:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed
do ! r: main (customer) loop
	dim z$*10,e$*30,bal
	read #hCustomer,using 'Form POS 1,C 10,POS 41,C 30,POS 292,PD 4.2': z$,e$,bal eof Finis
	if bal<>0 then
		if bal<0 and skipcr=1 then goto NextCustomer
		if bal<0 then
			mat month=(0)
			month(1)=bal
		else
			gosub TransLoop
		end if
		if sum(month)=(0) and bal<>0 then month(4)=bal
		pr #255,using 'Form POS 1,C 12,C 20,5*N 11.2': z$,e$(1:20),month(1),month(2),month(3),month(4),bal pageoflow PgOf
		totcolumn1+=month(1) : totcolumn2+=month(2)
		totcolumn3+=month(3) : totcolumn4+=month(4) : totbal+=bal
	end if
	NextCustomer: !
loop ! /r
PrHeader: ! r:
	pr #255: "\qc  {\f181 \fs22 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs28 \b "&env$('program_caption')&"}"
	pr #255: "\qc  {\f181 \fs22 \b "&trim$(dat$)&"}"
	pr #255,using 'Form POS 1,C 82,C 9': "\ql "&date$,"Page "&str$(p2+=1)
	pr #255: "{\ul Account No}  {\ul Customer Name}        {\ul "&(cnvrt$("PIC(zzZZ/ZZ/ZZ)",lastday(1)))(1:10)&"} {\ul "&(cnvrt$("PIC(zzZZ/ZZ/ZZ)",lastday(2)))(1:10)&"} {\ul "&(cnvrt$("PIC(zzZZ/ZZ/ZZ)",lastday(3)))(1:10)&"} {\ul      Older} {\ul    Balance}"
return ! /r
PgOf: ! r:
	pr #255: newpage
	gosub PrHeader
continue ! /r
Finis: ! r:
	pr #255: "                                 __________ __________ __________ __________ __________"
	pr #255,using 'Form POS 1,C 12,C 20,5*N 11.2': "","",totcolumn1,totcolumn2,totcolumn3,totcolumn4,totbal pageoflow PgOf
	pr #255: "                                 {\ul \strike           } {\ul \strike           } {\ul \strike           } {\ul \strike           } {\ul \strike           }"
	close #hCustomer: ioerr ignore
	close #2: ioerr ignore
	fncloseprn
goto Xit ! /r
Xit: fnXit
TransLoop: ! r:
	sortreq=0
	mat month=(0)
	restore #2,key>=z$&"         ": nokey EoTrans
	do
		read #2,using 'Form POS 1,C 10,N 8,N 1,PD 4.2': p$,tdate,tcode,tamount eof EoTrans
		if p$<>z$ then goto TlTransFinis
		if tcode =3 or tcode=4 then goto TlNextTrans ! skip collections or cm
		for j=1 to 3
			if tdate<firstday(3) then goto TlNextTrans ! older than we want to analyze
			if tdate>=firstday(j) and tdate<=lastday(j) then
				month(j)+=tamount
			else if tdate>lastday(1) then
				! if trans dated after the last day of the first month, just go ahead and put it the first month
				month(1)+=tamount
			end if
		next j
		TlNextTrans: !
	loop
	TlTransFinis: !
	tempbal=bal
	if month(1)=tempbal then
		tempbal=0
		month
		month(2)=month(3)=month(4)=0
		goto EoTrans
	else if month(1)<tempbal then
		tempbal=tempbal-month(1)
		goto TlMonth2
	else if month(1)>tempbal then
		month(1)=tempbal: month(2)=month(3)=month(4)=0
		goto EoTrans
	end if
	TlMonth2: !
	if month(2)=tempbal then
		tempbal=0: month(3)=month(4)=0
		goto EoTrans
	else if month(2)<tempbal then
		tempbal=tempbal-month(2)
		goto TlMonth3
	else if month(2)>tempbal then
		month(2)=tempbal: month(3)=month(4)=0
		goto EoTrans
	end if
	TlMonth3: !
	if month(3)=tempbal then
		tempbal=0
		month(4)=0
	else if month(3)<tempbal then
		tempbal=tempbal-month(3)
		month(4)=tempbal
	else if month(3)>tempbal then
		month(3)=tempbal
		month(4)=0
	end if
	EoTrans: !
return ! /r

include: Ertn

