autoLibrary
on error goto Ertn
fnTop(program$)
! r: constants, open files, initialize, etc
dim resp$(40)*60
dim dat$*20
fndat(dat$,1)
dim serviceName$(10)*20
fnGetServices(mat serviceName$)
x=0
for j=1 to 10
	dim services$(10)*20
	services$(j)=serviceName$(j)
	dim option2$(10)*20
	if trim$(serviceName$(j))<>'' then option2$(x+=1)=serviceName$(j)
	if trim$(serviceName$(j))='' then serviceName$(j)='N/A'
	serviceName$(j)=trim$(serviceName$(j)(1:8))&':'
next j
option2$(x)
open #hCustomer=fnH: 'Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\UBIndex.h[cno]',i,i,k
open #hTrans=fnH: 'Name=[Q]\UBmstr\ubTransVB.h[cno],KFName=[Q]\UBmstr\ubtrindx.h[cno]',i,i,k
open #14: 'Name=[Q]\GLmstr\GL_Work_[acsUserId].h[cno],Replace,RecL=104',internal,output ioerr ignore
if ~exists('[Q]\UBmstr\glinfo.h[cno]') then
	open #15: 'Name=[Q]\UBmstr\Glinfo.h[cno],KFName=[Q]\UBmstr\glinfoidx.h[cno],Shr,Use,RecL=89,KPs=1,KLn=23',i,outIn,k
	close #15:
end if
open #15: 'Name=[Q]\UBmstr\Glinfo.h[cno],KFName=[Q]\UBmstr\glinfoidx.h[cno],Shr',i,outIn,k

fnstyp(14)
!  styp=11 for jobcost; styp=14 for regular payroll
! /r
goto SCREEN1

Screen2: ! r:
	respc=x=0
	fnTos
	mylen=10: mypos=mylen+3 : right=1 : respc=0
	fram1=1: fnFra(1,1,12,100,'General Ledger Information','For each service, choose the appropriate g/l# for the bank, for the revenue, and possoibly the receivalbe account if using the accrual method.')
	fnLbl(2,20,'Cash In Bank',12,right,0,fram1)
	fnLbl(2,45,'Receivable',12,right,0,fram1)
	fnLbl(2,75,'Revenue',12,right,0,fram1)
	x=0
	for j=1 to 10
		fnLbl(j+2,1,serviceName$(j),mylen,right,0,fram1)
		fnQgl(j+2,12,fram1,2,1,25)
		resp$(respc+=1)=fnrgl$(gln$(x+=1))
		fnQgl(j+2,42,fram1,2,1,25)
		resp$(respc+=1)=fnrgl$(gln$(x+=1))
		fnQgl(j+2,72,fram1,2,1,25)
		resp$(respc+=1)=fnrgl$(gln$(x+=1))
	next j
	fnCmdKey('&Save',1,1,0,'Saves any changes and returns to menu.')
	fnCmdKey('&Create Accounts',3,0,0,'Allows you to create a chart of account (limited to the accounts you need) if general ledger or checkbook is not installed.')
	fnCmdKey('&Cancel',5,0,1,'Returns to menu without saving any changes on the screen.')
	ckey=fnAcs(mat resp$)
	if ckey<>5 then
		if ckey=3 then fnHamsterFio('UB GL Accounts') : fnchain(program$)
		for j=1 to 30
			gln$(j)=fnagl$(resp$(j))
		next j
	end if
rewrite #15,using 'form pos 1,c 20,n 3,3*c 12,3*n 10.2',key=key$: service$,ratecode,gl$(1),gl$(2),gl$(3),mat amount
goto SCREEN1 ! /r

SCREEN1: ! r:
	fnTos
	mylen=36 : mypos=mylen+2
	fnLbl(1,1,'Report Heading Date:',mylen,1,0)
	fnTxt(1,mypos,20)
	resp$(1)=dat$
	fnLbl(2,1,'Starting Date (blank for all):',mylen,1)
	fnTxt(2,mypos,10,0,1,'3',0,'Enter the first day of the period being posted.')
	resp$(2)=str$(ld1)
	fnLbl(3,1,'Ending Date (blank for all):',mylen,1)
	fnTxt(3,mypos,10,0,1,'3',0,'Enter the Last day of the period being posted.')
	resp$(3)=str$(hd1)
	fnChk(4,30,'General Ledger Installed:',1,0)
	if gli=1 then resp$(4)='True' else resp$(4)='False'
	fnChk(5,30,'Print Report:',1,0)
	if printreport=1 then resp$(5)='True' else resp$(5)='False'
	fnChk(6,30,'Show Details:',1,0)
	if showdetails=1 then resp$(6)='True' else resp$(6)='False'
	fnFra(8,1,2,60,'Method of Posting','You can either post on a Cash basis which only effects Cash and Revenues or you can post on an accrual method also effecting receivables.')
	fnOpt(1,3,'Cash Basis',0,1)
	if basis=0 or basis=1 then resp$(7)='True'
	fnOpt(2,3,'Accrual Method',0,1)
	if basis=2 then resp$(8)='False'
	fnCmdKey('&Post',1,1,0,'Begins the posting process.')
	fnCmdKey('&Assign GL Numbers',2,0,0,'Assign general ledger numbers to the various revenue accounts.')
	fnCmdKey('&Cancel',5,0,1,'Returns to menu without saving any changes on the screen.')
	ckey=fnAcs(mat resp$)
if ckey=5 then goto Xit
dat$=resp$(1) : ld1=val(resp$(2)) : hd1=val(resp$(3))
postingdate=val(resp$(3)(5:8))*100+val(resp$(3)(3:4))
if resp$(4)='True' then gli=1 else gli=0
if resp$(5)='True' then printreport=1 else printreport=0
if resp$(6)='True' then showdetails=1
basis=0 : if resp$(7)='True' then basis=cash=1
if resp$(8)='True' then basis=accrual=2
dim msgline$(3)*80
if ld1>hd1 and ld1>0 and hd1>0 then
	mat msgline$(1): msgline$(1)='Ending Date Before Starting Date!'
	fnMsgBox(mat msgline$,resp$,'',48)
	goto SCREEN1
end if
if basis=0 then
	mat msgline$(1): msgline$(1)='You must enter the basis for accounting!'
	fnMsgBox(mat msgline$,resp$,'',48)
	goto SCREEN1
end if
if ckey=2 then goto ScreenGlInfo
fnopenprn
gosub PrHdr
goto ACCUMULATE_TOTALS ! /r


	restore #15:
	do
		dim dollar(3)
		read #15,using 'form pos 60,3*n 10.2': mat dollar eof ACCUMULATE_TOTALS ! clear totals on end of each allocation record
		mat dollar=(0)
		rewrite #15,using 'form pos 60,3*n 10.2': mat dollar
	loop


ACCUMULATE_TOTALS: ! r:
do
	dim tg(10)
	read #hTrans,using 'form pos 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof CREATE_ENTRIES
	if tdate<ld1 or tdate>hd1 then goto NextTrans
	dim a(7)
	read #hCustomer,using 'form pos 143,7*pd 2,pos 1806,3*n 2',key=p$: mat a,extra11,extra12,extra13 nokey L1030
	goto L1040
	L1030: !
	mat msgline$(2): msgline$(1)='You have tranactions on customer # '&p$
	msgline$(2)='but the customer record no longer exists. '&p$
	fnMsgBox(mat msgline$,resp$,'',48)
	L1040: !
	dim amount(10,3)
	! 1=charge,2=penalty,3=collection,4=credit memo, 5=debit memo
	for j =1 to 10
		if basis=cash and tcode=1 or tcde=2 or tcode=4 or tcode=5 then goto NextTrans ! dont record anything but collections on cash basis.
		if basis=cash and tcode=3 then
			amount(j,1)=amount(j,1)+tg(j)
			amount(j,3)=amount(j,3)-tg(j)
			glcode=13
			goto L1130
			! debit bank and credit revenues on collections
		end if
		if basis=accrual and tcode=1 then
			amount(j,2)=amount(j,2)+tg(j)
			amount(j,3)=amount(j,3)-tg(j)
			glcode=23
			! record sales as debit to receivables and credit to  sales
		end if
		if basis=accrual and tcode=2 then
			amount(j,2)=amount(j,2)+tg(j)
			amount(j,3)=amount(j,3)-tg(j)
			glcode=23 ! record penalties as debit to receivables and credits to  penalty sales
		end if
		if basis=accrual and tcode=3 then
			amount(j,1)=amount(j,1)+tg(j)
			amount(j,2)=amount(j,2)-tg(j)
			glcode=12 ! debit bank, reduce receivables on collections
		end if
		if basis=accrual and tcode=4 then
			amount(j,2)=amount(j,2)-tg(j)
			amount(j,3)=amount(j,3)+tg(j)
			! reduce receivables and sales on credit memos
		end if
		if basis=accrual and tcode=5 then
			amount(j,2)=amount(j,2)+tg(j)
			amount(j,3)=amount(j,3)-tg(j)
			! increase receivables and increase sales on debit memos
		end if
		L1130: !
	next j
	if showdetails=1 then pr #255,using 'form pos 1,c 10,x 1,pic(zzzz/zz/zz),n 2,10 * n 9.2': p$,tdate,tcode,mat tg pageoflow PgOf
	NextTrans: !
loop
CREATE_ENTRIES: !
	for j=1 to 10
		if j=1 then ratecode=a(1)
		if j=2 then ratecode=a(2)
		if j=3 then ratecode=a(3)
		if j=4 then ratecode=a(4)
		if j=5 then ratecode=a(5)
		if j=6 then ratecode=extra11
		if j=7 then ratecode=extra12
		if j=8 then ratecode=extra13
		if j=9 then ratecode=a(6)
		if j=10 then ratecode=a(7)
		dim gl$(3)*12
		dim gln$(10,3)*12
		read #15,using 'form pos 24,3*c 12',key=services$(j)&cnvrt$('pic(zz#)',ratecode): mat gl$ nokey L1300
		gln$(j,1)=gl$(1): gln$(j,2)=gl$(2): gln$(j,3)=gl$(3) ! substitute individual breakdown based on water codes, etc into 30 item array used in entry generation
		L1300: !
	next j
	for j=1 to 10
		for x=1 to 3
			if amount(j,x) then
				write #14,using 'form pos 1,c 12,N 6,PD 6.2,N 2,N 2,C 12,C 52,C 12': gln$(j,x),postingdate,amount(j,x),3,0,'UB','Utility Billing Summary','' ! RGL$(15)
			end if
		nex x
	next j
	pr #255,using 'form skip 1,pos 1,c 60': 'G/L Number                Debits      Credits'
	for j=1 to 10
		for x=1 to 3
			if amount(j,x)>0 then
				pr #255,using 'form pos 1,c 12,x 7,n 13.2': gln$(j,x),amount(j,x) pageoflow PgOf
				totalDebits+=amount(j,x)
			else if amount(j,x)<0 then
				pr #255,using 'form pos 1,c 12,x 20,n 13.2': gln$(j,x),amount(j,x) pageoflow PgOf
				totalCredits+=amount(j,x)
			! else ! amount(j,x)=0
			end if
		next x
	next j

	pr #255,using 'form pos 20,n 13.2,n 13.2': totalDebits,totalCredits
	fncloseprn
	if gli=1 then fnchain('S:\General Ledger\Merge')
goto Xit ! /r
Xit: fnXit
PrHdr: ! r:
	pr #255: '\qc  {\f181 \fs18 \b '&env$('cnam')&'}'
	pr #255: '\qc {\f181 \fs24 \b UB Posting Recap}'
	pr #255: '\qc {\f181 \fs24 \b '&dat$&'}'
	if ld1<>0 and hd1<>0 then
		pr #255: '\qc {\f181 \fs18 \b '&trim$('From '&cnvrt$('pic(zzzz/zz/zz)',ld1)&' to '&cnvrt$('pic(zzzz/zz/zz)',hd1))&'}'
	end if
	pr #255,using 'form pos 1,C 20,pos 110,C 12': '\ql','Page '&str$(p2+=1)
	pr #255:
	dim heading$*130
	heading$='Account      Date     Cd'
	for h=1 to 10
		heading$=heading$&lpad$(serviceName$(h)(1:19),9)
		a=pos(heading$,':',1)
		if a>0 then heading$(a:a)=' '
	next h
	pr #255,using 'form pos 1,c 130': heading$
return ! /r
PgOf: ! r:
	pr #255: newpage
	gosub PrHdr
continue ! /r

ScreenGlInfo: ! r: screen
	fnTos
	respc=0
	dim item$(6)*20
	mat chdr$(6) : mat cmask$(6) : mat item$(6)
	chdr$(1)='Rec'
	chdr$(2)='Service Name' : chdr$(3)='Rate Code'
	chdr$(4)='GL-Cash' : chdr$(5)='GL-Receivable'
	chdr$(6)='GL-Revenue'
	cmask$(1)=cmask$(2)=''
	cmask$(3)=cmask$(4)=cmask$(5)=cmask$(6)=''
	fnflexinit1('GlBreak',1,1,20,100,mat chdr$,mat cmask$,1,0,frame)
	editrec=0
	restore #15:
READ_GLINFO_1: !
	dim service$*20
	read #15,using 'form pos 1,c 20,n 3,3*c 12,3*n 10.2': service$,ratecode,gl$(1),gl$(2),gl$(3),mat dollar eof EO_FLEX1
	item$(1)=str$(rec(15))
	item$(2)=service$ : item$(3)=str$(ratecode)
	item$(4)=gl$(1)
	item$(5)=gl$(2)
	item$(6)=gl$(3)
	fnflexadd1(mat item$)
	goto READ_GLINFO_1
EO_FLEX1: !
	fnCmdKey('&Add',1,0,0,'Add new records')
	fnCmdKey('&Edit',2,1,0,'Highlight any record and press Enter or click Edit or press Alt+E to change any existing record.')
	fnCmdKey('&Delete',3,0,0,'Highlight any record and press Alt+D or click Delete to remove any existing record.')
	fnCmdKey('E&xit',5,0,1,'Exit to menu')
	ckey=fnAcs(mat resp$)
	addone=_edit=0: holdvn$=''
	if ckey=5 then
		goto SCREEN1
	else if ckey=1 then
		addone=1
		service$='': ratecode=0: mat gl$=('')
		goto MaintainGlInfo
	else if ckey=2 or ckey=3 then
		editrec=val(resp$(1))
	end if
if editrec=0 then goto ScreenGlInfo
if ckey=2 or ckey=3 then
	read #15,using 'form pos 1,c 20,n 3,3*c 12,3*n 10.2',rec=editrec: service$,ratecode,gl$(1),gl$(2),gl$(3),mat dollar
end if
if ckey=2 then _edit=1 : holdvn$=vn$: goto MaintainGlInfo
if ckey=3 then gosub DeleteGlInfo : goto ScreenGlInfo
! /r
DeleteGlInfo: ! r:
	mat msgline$(2): msgline$(1)='You have chosen to delete a record.'
	msgline$(2)='Take OK to delete, Cancel to retain.'
	fnMsgBox(mat msgline$,resp$,'',49)
	if resp$='OK' then
		delete #15,rec=editrec:
	end if
return ! /r
Reindex: ! r:
	execute 'Index [Q]\UBmstr\Ubinfo.h[cno]'&' '&'[Q]\UBmstr\ubinfoidx.h[cno] 1 23 Replace DupKeys -n' ioerr ignore
return ! /r
MaintainGlInfo: ! r:
	right=1: mylen=25: mypos=mylen+3
	fnTos
	fnLbl(1,1,'Service:',mylen,right)
	fnComboA('GLCmbSrv',1,mypos,mat option2$,'Set up records for every service and all rate codes within that service',20)
	for j=1 to udim(optio2$)
		if option2$(j)=service$ then resp$(1)=option2$(j)
	next j
	fnLbl(2,1,'Rate Code:',mylen,right)
	fnTxt(2,mypos,3,0,0,'30',0,'Set up the general ledger informatin for each rate code you have for each service.')
	resp$(2)=str$(ratecode)
	fnLbl(3,1,'Cash G/L Number:',mylen,right)
	fnQgl(3,mylen,0,2,1)
	resp$(3)=fnrgl$(gl$(1))
	fnLbl(4,1,'Receivable G/L Number:',mylen,right)
	fnQgl(4,mylen,0,2,1)
	resp$(4)=fnrgl$(gl$(2))
	fnLbl(5,1,'Revenue G/L Number:',mylen,right)
	fnQgl(5,mylen,0,2,1)
	resp$(5)=fnrgl$(gl$(3))
	fnCmdKey('&Save',1,1,0,'Saves any changes and returns to gl breakdown screen.')
	fnCmdKey('&Create Accounts',3,0,0,'Allows you to create a chart of account (limited to the accounts you need) if general ledger or checkbook is not installed.')
	fnCmdKey('&Cancel',5,0,1,'Return to main screen without saving any changes.')
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto ScreenGlInfo
	if ckey=3 then fnHamsterFio('UB GL Accounts') : fnchain(program$)
	service$=resp$(1)
	ratecode=val(resp$(2))
	gl$(1)=fnagl$(resp$(3))
	gl$(2)=fnagl$(resp$(4))
	gl$(3)=fnagl$(resp$(5))
	breakdownde$=resp$(2)
	if _edit=1 then rewrite #15,using 'form pos 1,c 20,n 3,3*c 12,3*n 10.2',rec=editrec: service$,ratecode,gl$(1),gl$(2),gl$(3),mat dollar
	if addone=1 then write #15,using 'form pos 1,c 20,n 3,3*c 12,3*n 10.2': service$,ratecode,gl$(1),gl$(2),gl$(3),mat dollar
goto ScreenGlInfo ! /r

include: ertn
