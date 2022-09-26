! Additional Penalty Calculation ( calculates a standard dollar amount of penalty of each penalty charge in their system
autoLibrary
on error goto Ertn

dim resp$(20)*128
dim msgline$(1)*80
dim oldtg(11)  ! read but otherwise unused
dim z$*10
dim g(12)
dim e$(4)*30,transkey$*19
dim ba(13),badr(2),bt1(14,2)
dim bd1(5),bd2(5)
dim tg(11)
dim route(99)
dim serviceName$(10)*20
dim tax_code$(10)*1
dim pencolumn(10)
dim penalty$(10)*1
dim subjectto(10)
dim gb(10)
dim extra(23)
dim a(7)
dim columnhead$(10)*13
dim tmp$*220
dim coltot(10)

fnTop(program$)
fnLastBillingDate(bildat)

fnGetServices(mat serviceName$, mat service$, mat tax_code$,mat penalty$,mat subjectto)
for j=1 to 10
	if uprc$(penalty$(j))='Y' then
		pencount=pencount+1 ! count number of penalty columns needed
		column(pencount)=j
		columnhead$(pencount)=lpad$(rtrm$(serviceName$(j)(1:10)),10)
	end if
next j
if pencount<1 then pencount=1
mat pencolumn(pencount)
mat columnhead$(pencount)
mat coltot(pencount)

fncreg_read('Second Penalty Calculation Min Balance',minimumbal$) : minimumbal=val(minimumbal$) conv ignore
fncreg_read('Second Penalty Calculation Penalty Amount',penaltyamt$) : penaltyamt=val(penaltyamt$) conv ignore
fncreg_read('Second Penalty Calculation skip Service 10 Rate 9 Customers',skip_s10r9$) ! : penaltyamt=val(penaltyamt$) conv ignore
if minimumbal=0 then
	open #minbal:=5: 'Name=[Q]\UBmstr\Minbal.h[cno],Shr',i,outi,r ioerr ignore
	read #minbal,using 'form pos 1,n 10.2',rec=1,release: minimumbal ioerr ignore
	close #minbal: ioerr ignore
end if

SCREEN1: !
	fnTos
	mylen=29 : mypos=mylen+2 : rc=0
	fnLbl(1,1,'Penalty Date:',mylen,1)
	fnTxt(1,mypos,10,0,1,'1003')
	resp$(rc+=1)=str$(pendat)
	fnLbl(2,1,'Last Billing Date:',mylen,1)
	fnTxt(2,mypos,10,0,1,'1003')
	resp$(rc+=1)=str$(bildat)
 
	fnChk(4,31,'Print Meter Address:',1)
	resp$(rc+=1)='False'
	fnChk(5,31,'Print Mailing Address:',1)
	resp$(rc+=1)='False'
	fnLbl(6,1,'Minimum Balance:',mylen,1)
	fnTxt(6,mypos,8,0,1,'10',0,'The customer''s balance must be at least this amount before a penalty will be calculated.')
	resp$(rc+=1)=str$(minimumbal)
	fnLbl(7,1,'Penalty Amount:',mylen,1)
	fnTxt(7,mypos,8,0,1,'10',0,'Amount of penalty.')
	resp$(rc+=1)=str$(penaltyamt)
	fnChk(9,31,'skip Customers with a '&trim$(serviceName$(10))&' Rate Code of 9',1)
	resp$(rc+=1)=skip_s10r9$
	fnLbl(10,50,'') ! avoids error 857 caused by check box (skip customers...) - it is a bug in acs_component - jb 2019/08/22
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	pendat=val(resp$(1)(5:6)&resp$(1)(7:8)&resp$(1)(3:4))
	bildat=val(resp$(2)(5:6)&resp$(2)(7:8)&resp$(2)(3:4))
	if resp$(3)='True' then printadr=1 ! wants meter address printed
	if resp$(4)='True' then printmail=1 ! wants meter mailing address
	minimumbal=val(resp$(5))
	penaltyamt=val(resp$(6))
	skip_s10r9$=resp$(7)
 
	if pendat=0 then
		msgline$(1)='You must enter a valid Penalty Date'
		fnMsgBox(mat msgline$,pause$,'',48)
		goto SCREEN1
	else
		pendat=fndate_mmddyy_to_ccyymmdd(pendat)
	end if
	if bildat=0 then
		msgline$(1)='You must enter a valid Last Billing Date.'
		fnMsgBox(mat msgline$,pause$,'',48)
		goto SCREEN1
	end if
 
	fncreg_write('Second Penalty Calculation Min Balance',str$(minimumbal))
	fncreg_write('Second Penalty Calculation Penalty Amount',str$(penaltyamt))
	fncreg_write('Second Penalty Calculation skip Service 10 Rate 9 Customers',skip_s10r9$)
	fnAutomatedSavePoint('before')
	open #customer=1: 'Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr',i,outIn,k
	open #h_trans=fnH: 'Name=[Q]\UBmstr\ubTransVB.h[cno],KFName=[Q]\UBmstr\ubtrindx.h[cno],Shr',i,outIn,k
	open #hTrans2=fnH: 'Name=[Q]\UBmstr\ubTransVB.h[cno],KFName=[Q]\UBmstr\UBTrdt.h[cno],Shr',i,outIn,k
	gosub BUD1
	open #ratemst:=8: 'Name=[Q]\UBmstr\ubData\RateMst.h[cno],KFName=[Q]\UBmstr\ubData\RateIdx1.h[cno],Shr',i,i,k
	fnopenprn
	gosub HDR
	do
		READ_CUSTOMER: ! r:
		read #customer,using 'form pos 1,C 10,4*C 30,pos 143,7*PD 2,pos 292,PD 4.2,PD 4,12*PD 4.2,pos 388,10*PD 5.2,pos 1741,N 2,N 7,2*N 6,N 9,PD 5.2,N 3,3*N 9,3*N 2,3*N 3,N 1,3*N 9,3*PD 5.2,C 30,7*C 12,3*C 30': z$,mat e$,mat a,bal,f,mat g,mat gb,mat extra eof EO_CUSTOMER
		! If TRIM$(Z$)='100120.06' Then Pause
		if skip_s10r9$='True' and a(7)=9 then
			!   pr 'skipping '&z$&' because service 10 rate code is a 9'
			!   pause
			goto READ_CUSTOMER
		end if
		if bud1=1 then gosub BUD2
		if totba>0 and bd1>0 and f=bildat and g(10)>0 then goto EO_READ ! Penalize if Budget Bill and Havent Paid Last Bill, Even If BAL <0
		if bal=0 or bal<minimumbal then goto READ_CUSTOMER
		if totba>0 and bd1=0 then goto READ_CUSTOMER ! have budget billing and have paid last bill
		! If G(10)=0 Then Goto 430
		if f<>bildat then goto READ_CUSTOMER
		EO_READ: !
		goto PENCAL ! /r
 
		PENCAL: ! r: penalty calculation
		mat tg=(0)
		column=0
		for j=1 to 10
			if uprc$(penalty$(j))='Y' then ! place first penalty in first column, column totals, etc
				tg(j)=penaltyamt
				column+=1
				pencolumn(column)=tg(j)
				coltot(column)=coltot(column)+tg(j)
			end if
		next j
		bal+=sum(tg)
		! tot+=sum(tg)
		tcode=2 ! penalty trans code
		for j=1 to 10
			if tg(j)<>0 then gb(j)+=tg(j) ! add new penalties into balance breakdown if here is a penalty
		next j
		transkey$=z$&cnvrt$('pic(########)',pendat)&cnvrt$('pic(#)',tcode)
		tamount=sum(tg)
		! add all penalties into total transaction amount
		read #h_trans,using 'form pos 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1',key=transkey$: y$,olddate,oldcode,oldamount,mat oldtg nokey L990 ! check for recalk
		bal=bal-oldamount
		for j=1 to 10
			gb(j)=gb(j)-tg(j) ! take off of balance breakdown
		next j
		rewrite #h_trans,using 'form pos 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': z$,pendat,2,tamount,mat tg,0,0,0,0,0,0,bal,pcode
		goto L1000
		
		L990: !
		write #h_trans,using 'form pos 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': z$,pendat,2,tamount,mat tg,0,0,0,0,0,0,bal,pcode
		L1000: !
		totb+=bal
		rewrite #customer,using 'form pos 292,PD 4.2,pos 388,10*PD 5.2': bal,mat gb
		if extra(1)<0 or extra(1)>99 then extra(1)=99
		route(extra(1))+=sum(pencolumn)
		! pr extra(1)
		if printadr<>1 then
			pr #255,using F_PRINT_LINE: z$,e$(2),mat pencolumn,bal pageoflow PgOf
		end if
		if printadr=1 then
			pr #255,using F_PRINT_LINE: z$,e$(2),mat pencolumn,bal,e$(1)(1:25) pageoflow PgOf
		end if
		if printmail=1 then
			pr #255,using 'form pos 15,C 30': e$(3) pageoflow PgOf
			pr #255,using 'form pos 15,C 30': e$(4) pageoflow PgOf
		end if
		F_PRINT_LINE: form pos 1,c 10,x 4,c 30,pos 52,pencount*pic(---------.##),x 2,pic(-------.##),x 2,c 25
	loop
 
EO_CUSTOMER: !
	tmp$=rpt$(' ',52)&'{\ul'&rpt$(' ',12)&'}'
	for j=1 to udim(mat columnhead$)
		tmp$=tmp$&' {\ul'&rpt$(' ',12)&'}'
	next j
	pr #255: tmp$
	pr #255,using 'form pos 17,C 30,x 5,pencount*N 12.2,N 12.2': 'Overall Totals',mat coltot,totb
	tmp$= rpt$(' ',52)&'{\ul \strike'&rpt$(' ',12)&'}'
	for j=1 to udim(mat columnhead$)
		tmp$=tmp$&' {\ul \strike'&rpt$(' ',12)&'}'
	next j
	pr #255: tmp$
	pr #255,using 'form skip 2,c 20': 'Totals by Route'
	for j=1 to 99
		if route(j)<>0 then
			pr #255,using 'form pos 1,c 10,pic(zzz,zzz.zz)': 'Route '&cnvrt$('pic(zz)',j),route(j) pageoflow PGOF_NO_HDR
		end if
	next j
	close #customer: ioerr ignore
	close #h_trans: ioerr ignore
	close #hTrans2: ioerr ignore
	fncloseprn
goto Xit ! /r
PgOf: ! r:
	pr #255: newpage
	gosub HDR
continue  ! /r
PGOF_NO_HDR: ! r:
	pr #255: newpage
continue  ! /r
HDR: ! r:
	! p+=1
	pr #255: '\qc '&env$('program_caption')
	pr #255: '\qc  {\f181 \fs28 \b '&env$('program_caption')&' - Listing}'
	pr #255: date$('Month DD, CCYY')
	pr #255: '\ql   '
	tmp$='{\ul Account No  } {\ul Customer Name and Address      }         {\ul '
	for j=1 to udim(mat columnhead$)
		tmp$=tmp$&columnhead$(j)&'} { \ul '
	next j
	if printadr=1 then tmp$=tmp$&'} {\ul   Balance}  {\ul  Meter Address}' else tmp$=tmp$&'} {\ul   Balance}'
	pr #255: tmp$
	pr #255: ''
return  ! /r
Xit: fnXit
BUD1: ! r:
	bud1=0
	open #81: 'Name=[Q]\UBmstr\BudMstr.h[cno],KFName=[Q]\UBmstr\BudIdx1.h[cno],Shr',i,outIn,k ioerr EO_BUD1
	open #82: 'Name=[Q]\UBmstr\BudTrans.h[cno],Shr',i,outi,r
	bud1=1
	EO_BUD1: !
return  ! /r
BUD2: ! r:
	totba=bd1=bd2=0
	mat bd1(5) : mat bd1=(0) : mat bd2=(0)
	if bud1=0 then goto EO_BUD2
	read #81,using L1520,key=z$: z$,mat ba,mat badr nokey EO_BUD2
	for j=2 to 12: totba=totba+ba(j): next j
	L1520: form pos 1,c 10,pd 4,12*pd 5.2,2*pd 3
	ta1=badr(1)
	L1540: !
	if ta1=0 then goto EO_BUD2
	read #82,using L1560,rec=ta1: z$,mat bt1,nba noRec EO_BUD2
	L1560: form pos 1,c 10,2*pd 4,24*pd 5.2,2*pd 4,pd 3
	if bt1(14,1)>0 then goto L1610
	if bt1(12,1)=0 then goto L1600 ! don't allow blank records to go thru routine
	bd1+=1
	L1600: !
	if bd1>5 then goto EO_BUD2
	L1610: !
		ta1=nba
	goto L1540
	EO_BUD2: !
return  ! /r
include: ertn
