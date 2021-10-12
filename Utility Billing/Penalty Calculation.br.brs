! formerly S:\acsUB\ubPenCal
! Penalty Calculation

	autoLibrary
	on error goto Ertn

	dim resp$(8)*40,msgline$(1)*80,oldtg(11)
	dim z$*10,g(12),dat$*20,e$(4)*30,rt(10,3),transkey$*19
	dim ba(13),badr(2),bt1(14,2),bd1(5),bd2(5),tg(11),route(99)
	dim serviceName$(10)*20,tax_code$(10)*1,pencolumn(10)
	dim d(15)
	dim subjectto(10),gb(10),extra(23),a(7)
	dim columnhead$(10)*13,tmp$*220,coltot(10),basepenalty(10)

	right=1
	fnTop(program$)
	fnLastBillingDate(bildat)
	fndat(dat$)
! r:  get MinimumBal
	open #minbal:=5: "Name=[Q]\UBmstr\Minbal.h[cno],Use,RecL=10,Shr",i,outi,r
	read #minbal,using 'Form POS 1,n 10.2',rec=1,release: minimumbal noRec SET_DEFAULT_MINUMUMBAL
	goto EO_MINIMUMBAL
SET_DEFAULT_MINUMUMBAL: !
	minimumbal=1.00
	write #minbal,using 'Form POS 1,n 10.2',rec=1,release: minimumbal
EO_MINIMUMBAL: !
	close #minbal:
! /r
	fn_scr_main
	if ckey=5 then goto Xit
	fnAutomatedSavePoint('before')
	open #hCustomer:=1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed
	open #h_trans:=2: "Name=[Q]\UBmstr\ubTransVB.h[cno],KFName=[Q]\UBmstr\ubtrindx.h[cno],Shr",internal,outIn,keyed
	open #hTrans2=fnH: "Name=[Q]\UBmstr\ubTransVB.h[cno],KFName=[Q]\UBmstr\UBTrdt.h[cno],Shr",internal,outIn,keyed

	fnGetServices(mat serviceName$,mat service$,mat tax_code$,mat penalty$,mat subjectto)

	for j=1 to 10
		if uprc$(penalty$(j))="Y" then
			pencount=pencount+1
			column(pencount)=j
			columnhead$(pencount)=lpad$(rtrm$(serviceName$(j)(1:10)),10)
! count number of penalty columns needed
		end if
	next j
	if pencount<1 then pencount=1
	mat pencolumn(pencount)
	mat columnhead$(pencount)
	mat coltot(pencount)
	fn_bud1
	open #ratemst:=8: "Name=[Q]\UBmstr\ubData\RateMst.h[cno],KFName=[Q]\UBmstr\ubData\RateIdx1.h[cno],Shr",i,i,k
	fnopenprn
	fn_print_header
	do
READ_CUSTOMER: !
		read #hCustomer,using 'Form POS 1,C 10,4*C 30,POS 143,7*PD 2,POS 292,PD 4.2,PD 4,12*PD 4.2,POS 388,10*PD 5.2,POS 1741,N 2,N 7,2*N 6,N 9,PD 5.2,N 3,3*N 9,3*N 2,3*N 3,N 1,3*N 9,3*PD 5.2,pos 217,15*pd 5': z$,mat e$,mat a,bal,f,mat g,mat gb,mat extra,mat d eof EO_CUSTOMER
		! if env$('acsDeveloper')<>'' and trim$(z$)='100001.00' then debug_this_account=1 else debug_this_account=0
		if debug_this_account then show_math=1 else show_math=0
		route_number=extra(1)
!   if env$('client')="Divernon" and bal<0 then goto READ_CUSTOMER
		if bud1=1 then
			fn_bud2
			if env$('client')='White Hall' and bd1>0 then goto READ_CUSTOMER ! Never penalize if Budget Billing
			if env$('client')='Bethany' and bd1>0 then goto READ_CUSTOMER ! Never penalize if Budget Billing
			if totba>0 and bd1>0 and f=bildat then goto EO_READ ! Penalize if Budget Bill and Havent Paid Last Bill, Even If BAL <0
			if totba>0 and bd1=0 then goto READ_CUSTOMER ! have budget billing and have paid last bill
		end if
		if bal=0 or bal<minimumbal then goto READ_CUSTOMER
		if f<>bildat then goto READ_CUSTOMER
EO_READ: !
		fn_pencal
	loop
!
EO_CUSTOMER: !
	fn_print_totals
	close #hCustomer: ioerr ignore
	close #h_trans: ioerr ignore
	close #hTrans2: ioerr ignore
	fncloseprn
	goto Xit

PgOf: !
	pr #255: newpage
	fn_print_header
continue

Xit: fnXit
def fn_scr_main
	SM_ASK: !
	fnTos
	mylen=27
	mypos=mylen+2
	fnLbl(1,1,"Penalty Date:",mylen,right)
	fnTxt(1,mypos,10,0,1,"1003")
	resp$(1)=str$(pendat)
	fnLbl(2,1,"Last Billing Date:",mylen,right)
	fnTxt(2,mypos,10,0,1,"1003")
	resp$(2)=str$(bildat)
	fnLbl(3,1,"Report Heading Date:",mylen,right)
	fnTxt(3,mypos,20)
	resp$(3)=dat$
	fnChk(4,31,"Print Meter Address:",right)
	resp$(4)="False"
	fnChk(5,31,"Print Mailing Address:",right)
	resp$(5)="False"
	fnLbl(6,1,"Minimum Balance:",mylen,right)
	fnTxt(6,mypos,8,0,1,"10",0,"The customer's balance must be at least this amount before a penalty will be calculated.")
	resp$(6)=str$(minimumbal)
	fnFra(8,1,2,45,"Base for calculating penalty","The penalty can either be calculated on current bill or the total balance owed.",0)
	fnOpt(1,2,"Base penalty on current bill",0,1)
	resp$(7)="True"
	fnOpt(2,2,"Base penalty on total balance",0,1)
	resp$(8)="False"
	if env$('client')="Colyell" or env$('client')="Cerro Gordo V" then ! can change the default on that to: Base penalty on total balance
		resp$(7)="False"
		resp$(8)="True"
	end if  ! env$('client')=...
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey<>5 then
		pendat=val(resp$(1)(5:6)&resp$(1)(7:8)&resp$(1)(3:4))
		bildat=val(resp$(2)(5:6)&resp$(2)(7:8)&resp$(2)(3:4))
		dat$=resp$(3)
		fndat(dat$,2)
		if resp$(4)="True" then printadr=1 ! wants meter address printed
		if resp$(5)="True" then printmail=1 ! wants meter mailing address
		minimumbal=val(resp$(6))
		open #minbal:=5: "Name=[Q]\UBmstr\Minbal.h[cno],Use,RecL=10,Shr",i,outi,r
		rewrite #minbal,using 'Form POS 1,n 10.2',rec=1,release: minimumbal
		close #minbal:
		if resp$(7)="True" then penaltybase$="Bill" ! base penalties on current bill
		if resp$(8)="True" then penaltybase$="Balance" ! base penalties on current balance
		if pendat=0 then
			msgline$(1)="You must enter a valid Penalty Date"
			fnmsgbox(mat msgline$,pause$,'',48)
			goto SM_ASK
		else if bildat=0 then
			msgline$(1)="You must enter a valid Last Billing Date."
			fnmsgbox(mat msgline$,pause$,'',48)
			goto SM_ASK
		end if
		pendat=fndate_mmddyy_to_ccyymmdd(pendat)
	end if
fnend
def fn_pencal ! penalty calculation
	negatives=0 ! if breakdown is by mat gb and mat gb has some negatives, then do something special
	mat basepenalty=(0)
	if show_math then
		pr #255: ''
		pr #255: '*Math for Account: '&z$&'  Penalty Based on '&penaltybase$
	end if
	if env$('client')="Galena" then
		bdiff=sum(mat gb)-bal
		gb(8)=gb(8)-bdiff
	else if env$('client')='Colyell' or env$('client')='White Hall' then
		basepenalty(10)=bal
		goto GOT_BASEPENALTY
	! else if env$('client')='Lovington' and penaltybase$="Balance" then
	! 	waterpercent=round(g(1)/(g(1)+g(2)),2) ! sewerpercent=1-waterpercent ! lovington
	! 	basepenalty(9)=round((bal*waterpercent),2) ! logington
	! 	basepenalty(10)=bal-basepenalty(9) ! lovington allocate water and sewer penalty in ration of water to sewer
	! 	goto GOT_BASEPENALTY ! lovington
	end if
! if debug_this_account then pause
	if env$('client')="Franklinton" then fn_franklinton : goto L1370
	if penaltybase$="Bill" and bal<g(11) then ! use the balance to calculate penalty if balance less than last bill
		if show_math then pr #255: '     *** use the balance ('&str$(bal)&') to calculate penalty if balance less than last bill ('&str$(g(11))&')( usebalance=1 )'
		usebalance=1
	else
		usebalance=0
	end if
	! If SUM(GB)<>BAL Then bASE=0 : Goto 990 ! if mat gb screwed up, just use the balance to calculate the penalty  kj 20110  afraid to leave in
	! if trim$(z$)='100090.00' then pr z$ : pause
	for j=1 to 10
		! r: get Base
		if totba>0 and bd1>0 and subjectto(j)<>0 then ! have budget and have not paid last payment
			if show_math then pr #255: '     have budget and have not paid last payment'
			if show_math then pr #255: '       Base set to '&str$(g(j))
			base=g(j)
			!       goto L1060
		else if penaltybase$="Bill" and usebalance=0 then ! base on current bill
			if show_math then pr #255: '     Base on current bill ( if penaltybase$="Bill" and usebalance=0 )'
			if show_math then pr #255: '       usebalance='&str$(usebalance)
			if show_math then pr #255: '       Base set to '&str$(g(j))
			base=g(j)
			!       goto L1060
		else if (penaltybase$="Balance" or usebalance=1) and subjectto(j)<>0 and gb(j)>0 then
			if show_math then pr #255: '     (penaltybase$="Balance" or usebalance=1) and subjectto('&str$(j)&')<>0 and gb('&str$(j)&')'
			if show_math then pr #255: '       Base set to '&str$(gb(j))
			base=gb(j)
		else
			if show_math then pr #255: '     base set to zero'
			base=0 ! base on balance
		end if
		if (penaltybase$="Balance" or usebalance=1) and subjectto(j)<>0 and gb(j)<0 then
			negatives+=gb(j)
		end if
	! L1060: !
	! if debug_this_account then pr ' mat basepenalty should get set here subjectto('&str$(j)&')=';subjectto(j) : pause
		if subjectto(j)>0 then ! accumulate all charges by the penalty they are subject to
			if show_math then
				pr #255: '     accumulate all charges by the penalty they are subject to'
				pr #255: '       basepenalty('&str$(subjectto(j))&')=itself + base(which is '&str$(base)&') totalling '&str$(basepenalty(subjectto(j))+base)
			end if
			basepenalty(subjectto(j))=basepenalty(subjectto(j))+base
		end if
		if (penaltybase$="Balance" or usebalance=1) and subjectto(j)=0 and penalty$(j)="Y" then ! if the service is any of the penalties, add the previous penalty into the balance
			if show_math then
				pr #255: '     if the service is any of the penalties, add the previous penalty into the balance'
				pr #255: '       basepenalty('&str$(j)&')=itself + base(which is '&str$(base)&') totalling '&str$(basepenalty(j)+base)
			end if
			basepenalty(j)=basepenalty(j)+base
		end if
	next j
	if env$('client')="Kimberling" and g(2)>0 then basepenalty(10)-=g(1) ! no penalty on water if they have swewer
	if show_math and negatives then pr #255: '     negatives='&str$(negatives)&' doing fn_worryabout'
	if negatives<>0 then fn_worryabout
	GOT_BASEPENALTY: !
	if show_math then
		for j=1 to 10
			if basepenalty(j)<>0 then
				pr #255: '     Base Penalty ('&str$(j)&')= '&str$(basepenalty(j))
			end if
		next j
	end if
	! /r
	mat tg=(0) ! accumulate penalties in mat tg (transaction record)
	column=0
	mat pencolumn=(0)
	for j=1 to 10
		if uprc$(penalty$(j))="Y" then
			penaltycode$=uprc$(service$(j))
			if j<6 then pencode=a(j) ! rate codes in customer layout are not in order.  The first 5 a( match the services. The next three services are pulled from mat extra. 9 and 10 use a(6)&a(7)
			if j=6 then pencode=extra(11)
			if j=7 then pencode=extra(12)
			if j=8 then pencode=extra(13)
			if j=9 then pencode=a(6)
			if j=10 then pencode=a(7)
			if env$('client')="Pennington" and pencode=0 then pencode=1 ! default to one so codes don't have to be added to old customer records
			if env$('client')="Granby" and pencode=0 then pencode=1 ! default to one so codes don't have to be added to old customer records
			read #ratemst,using 'Form POS 55,32*G 10',key=penaltycode$&lpad$(str$(pencode),2): mc1,mu1,mat rt nokey L1230
			goto L1240
			L1230: !
			rt(1,3)=0
			mc1=0 ! nokey but still amount take up a column and calculate 0
			L1240: !
			if env$('client')="Riverside" and j=10 then ! r:
				g(10)=round(rt(1,3)*min(mc1,g(1)+g(2)+g(3)+g(4)+g(5)+g(6)+g(7)+g(8)),2)
				g(10)=g(10)+rt(2,3)*round(max(0,g(1)+g(2)+g(3)+g(4)+g(5)+g(6)+g(7)+g(8)-mc1),2)
				g(10)=max(0,g(10))
				tg(10)=g(10)
			else ! /r
				! if debug_this_account then pause
				! if env$('client')='Exeter' then basepenalty(9)=basepenalty(10) : basepenalty(10)=0  ! XXX  This is for the first month only!
				if show_math and round(basepenalty(j)*rt(1,3),2)<>0 then
					pr #255: '     calculated Penalty Base is '&penaltybase$&' '&str$(basepenalty(j))
					pr #255: '     Penalty: tg('&str$(j)&')=round('&str$(basepenalty(j))&'*'&str$(rt(1,3))&',2)'
					tg(j)=round(basepenalty(j)*rt(1,3),2)
					pr #255: '     tg('&str$(j)&')='&str$(tg(j))
				else
					tg(j)=round(basepenalty(j)*rt(1,3),2) ! penalty based on base amount that was accumulated for each penalty field * rate for that penalty code
				end if
				if show_math then
					pr #255: '     tg('&str$(j)&')=max('&str$(mc1)&','&str$(tg(j))&')'
				end if
				tg(j)=max(mc1,tg(j))
				if env$('client')="Pennington" then ! r:
					if a(6)=0 then a(6)=1 ! default tax code to 1
					read #ratemst,using 'Form POS 55,32*G 10',key="TX"&lpad$(str$(a(6)),2): mc1,mu1,mat rt nokey L1330 ! read Pennington's sales tax rate
					! tg(j)=round(tg(j)*(1+rt(1,3)),2) ! Pennington adds sales tax to penalty; but now they say they don't.  Not sure what happened there.
				end if  ! /r
			end if
			column+=1
			tg(j)=max(0,tg(j)) ! no negative penalties
			pencolumn(column)=tg(j)
			coltot(column)=coltot(column)+tg(j) ! place first penalty in first column, column totals, etc
		end if
	next j
	! if env$('client')="Kimberling" then fn_flat_percent_on_priorbal(.75) ! add .75% of previous balance
	! if env$('client')="Thayer" or env$('client')='Exeter' then fn_flat_percent_on_lastbill(10) ! add 10% of last month's bill
	if env$('client')="Thayer" then fn_flat_percent_on_lastbill(10) ! add 10% of last month's bill
	if env$('client')='Edison' then fn_flat_amt(5)
	if sum(mat tg)=0 then goto Xit_PENCAL ! skip if no penalty calculated prb 10/13/11
	if penaltybase$="Balance" then fn_check_rounding ! some times the penalty calculated at the time of the bill is off a penny because this program rounds each calculation for each service. Not always same as calculating on total!
	L1330: !
	bal+=sum(mat tg)
	tot+=sum(mat tg)
	tcode=2 ! penalty trans code
	for j=1 to 10
		if tg(j)<>0 then gb(j)+=tg(j) ! add new penalties into balance breakdown if here is a penalty
	next j
	L1370: !
	transkey$=z$&cnvrt$("pic(########)",pendat)&cnvrt$("pic(#)",tcode)
	tamount=sum(mat tg) ! add all penalties into total transaction amount
	read #h_trans,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1',key=transkey$: y$,olddate,oldcode,oldamount,mat oldtg nokey WRITE_2 ! check for recalk
	bal=bal-oldamount
	for j=1 to 10
		gb(j)=gb(j)-oldtg(j) ! take off of balance breakdown
	next j
	rewrite #h_trans,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': z$,pendat,2,tamount,mat tg,0,0,0,0,0,0,bal,pcode
	goto PAST_WRITE_2

	WRITE_2: !
	write #h_trans,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': z$,pendat,2,tamount,mat tg,0,0,0,0,0,0,bal,pcode
	PAST_WRITE_2: !
	rewrite #hCustomer,using 'Form POS 292,PD 4.2,POS 388,10*PD 5.2': bal,mat gb

	totb+=bal
	if route_number<0 or route_number>99 then route_number=99
	route(route_number)+=sum(pencolumn)
	fn_print_record
	XIT_PENCAL: !
fnend
def fn_print_header
	! p+=1
	pr #255: "\qc "&env$('cnam')
	pr #255: "\qc  {\f181 \fs28 \b "&env$('program_caption')&" - Listing}"
	pr #255: dat$
	pr #255: "\ql   "
	tmp$="{\ul Account No  } {\ul Customer Name and Address      }         {\ul "
	for j=1 to udim(columnhead$)
		tmp$=tmp$&columnhead$(j)&"} { \ul "
	next j
	! if env$('client')="Kimberling" then
	!   tmp$=tmp$&"} {\ul  Interest} {\ul     Balance}"
	if printadr=1 then
		tmp$=tmp$&"} {\ul   Balance}  {\ul  Meter Address}"
	else
		tmp$=tmp$&"} {\ul   Balance}"
	end if
	pr #255: tmp$
	pr #255: ""
fnend
def fn_print_record
	!
	! if env$('client')="Kimberling" then
	!  int_tmp=PENCOLUMN(1)-G(10) ! MAX(0,round((g(12)-g(11))*.0075,2))
	!  pen_tmp=G(10) ! MAX(0,round(pencolumn(1)-int_tmp,2))
	!  pr #255,using F_PRLINE_KIM: z$,e$(2),pen_tmp,int_tmp,bal pageoflow PgOf
	!  F_PRLINE_KIM: form pos 1,c 10,x 4,c 30,pos 52,2*pic(---------.##),x 2,pic(-------.##),x 2,c 25
	!  pen_accum+=pen_tmp
	!  int_accum+=int_tmp
	if printadr=1 then
		pr #255,using F_PRLINE: z$,e$(2),mat pencolumn,bal,e$(1)(1:25) pageoflow PgOf
	else
		pr #255,using F_PRLINE: z$,e$(2),mat pencolumn,bal pageoflow PgOf
	end if
	if printmail=1 then
		pr #255,using "Form POS 15,C 30": e$(3) pageoflow PgOf
		pr #255,using "Form POS 15,C 30": e$(4) pageoflow PgOf
	end if  ! printmail=1
	F_PRLINE: form pos 1,c 10,x 4,c 30,pos 52,pencount*pic(---------.##),x 2,pic(-------.##),x 2,c 25
fnend
def fn_print_totals
	tmp$=rpt$(" ",52)&"{\ul"&rpt$(" ",12)&"}"
	column_count=udim(mat columnhead$)
	! if env$('client')="Kimberling" then column_count+=1
	for j=1 to column_count
		tmp$=tmp$&" {\ul"&rpt$(" ",12)&"}"
	next j
	pr #255: tmp$
	! if env$('client')="Kimberling" then
	!   pr #255,using "Form POS 17,C 30,x 5,3*N 12.2": "Overall Totals",pen_accum,int_accum,totb
	! else
	pr #255,using "Form POS 17,C 30,x 5,pencount*N 12.2,N 12.2": "Overall Totals",mat coltot,totb
	! end if
	tmp$=rpt$(" ",52)&"{\ul \strike"&rpt$(" ",12)&"}"
	for j=1 to column_count
		tmp$=tmp$&" {\ul \strike"&rpt$(" ",12)&"}"
	next j
	pr #255: tmp$
	pr #255,using 'form skip 2,c 20': "Totals by Route"
	for j=1 to 99
		if route(j)<>0 then
			pr #255,using "form pos 1,c 10,pic(zzz,zzz.zz)": "Route "&cnvrt$("pic(zz)",j),route(j) pageoflow PgOf
		end if
	next j
	restore #1:
	do
		read #1,using "Form POS 292,PD 4.2": bal eof PT_EO1
		totalbal+=bal
	loop
	PT_EO1: !
	pr #255:
	balpos=51+(pencount*12)
	pr #255,using 'form pos 15,c 30,pos balpos,pic($-,---,---.##)': "Total Due from all Customers",totalbal
fnend
def fn_bud1
	bud1=0
	open #81: "Name=[Q]\UBmstr\BudMstr.h[cno],KFName=[Q]\UBmstr\BudIdx1.h[cno],Shr",internal,outIn,keyed ioerr EO_BUD1
	open #82: "Name=[Q]\UBmstr\BudTrans.h[cno],Shr",i,outi,r
	bud1=1
	EO_BUD1: !
fnend
def fn_bud2
	totba=bd1=bd2=0
	mat bd1(5)
	mat bd1=(0)
	mat bd2=(0)
	if bud1=0 then goto EO_BUD2
	read #81,using F_BUD_2A,key=z$: z$,mat ba,mat badr nokey EO_BUD2
	F_BUD_2A: form pos 1,c 10,pd 4,12*pd 5.2,2*pd 3
	for j=2 to 12
		totba=totba+ba(j)
	next j
	ta1=badr(1)
	do
		if ta1=0 then goto EO_BUD2
		read #82,using "form pos 1,c 10,2*pd 4,24*pd 5.2,2*pd 4,pd 3",rec=ta1: z$,mat bt1,nba noRec EO_BUD2
		if bt1(14,1)<=0 then
			if bt1(12,1)<>0 then ! don't allow blank records to go thru routine
				bd1=bd1+1
			end if
			if bd1>5 then goto EO_BUD2
		end if
		ta1=nba
	loop
	EO_BUD2: !
fnend
def fn_worryabout
	if sum(basepenalty)<>0 then
		negativepercent=round((sum(basepenalty)+negatives)/sum(basepenalty),2)
		for j=1 to 10
			basepenalty(j)=round(basepenalty(j)*negativepercent,2)
		next j
	end if
fnend
def fn_check_rounding
	!   make sure (if they are basing on balance due) that the individual calculations add to the same amount as the rate * the balance
	if round(bal*rt(1,3),2)=sum(mat tg) then goto CR_XIT
	adjustment=0
	if round(bal*rt(1,3),2)-.01=sum(mat tg) then adjustment=.01 : goto CR_DOIT ! add one cent to a penalty breakdown amount
	if round(bal*rt(1,3),2)-.02=sum(mat tg) then adjustment=.02 : goto CR_DOIT ! add one cent to a penalty breakdown amount
	if round(bal*rt(1,3),2)+.01=sum(mat tg) then adjustment=-.01 : goto CR_DOIT ! subtract one cent from a penalty breakdown amount
	if round(bal*rt(1,3),2)+.02=sum(mat tg) then adjustment=-.02 : goto CR_DOIT ! subtract one cent from a penalty breakdown amount
	goto CR_XIT
	CR_DOIT: !
	penx=0
	for j=1 to 10 ! put rounding adjustment in first service that has a penalty
		if basepenalty(j)>0 then
			tg(j)=tg(j)+adjustment
			penx+=1
			coltot(penx)=coltot(penx)+adjustment
			goto CR_XIT
		end if
	next j
	CR_XIT: !
fnend
def fn_franklinton
	pentot=0
	gb(1)=gb(1)+round(g(1)*.1,2)
	gb(2)=gb(2)+round(g(2)*.1,2)
	gb(5)=gb(5)+round(g(5)*.1,2)
	gb(8)=gb(8)+round(g(8)*.1,2)
	pentot=pentot+round(g(1)*.1,2)+round(g(2)*.1,2)+round(g(5)*.1,2)+round(g(8)*.1,2)
	if a(4)=3 then
		gb(4)=gb(4)+round(g(4)*.02,2)
		pentot=pentot+round(g(4)*.02,2)
	else
		gb(4)=gb(4)+round(d(11)*.005,2)
		pentot=pentot+round(d(11)*.005,2)
	end if
	mat tg=(0)
	tg(10)=pentot
	bal+=sum(mat tg)
	tot+=sum(mat tg)
	tcode=2 ! penalty trans code
	pencolumn(1)=tg(10)
	coltot(1)=coltot(1)+tg(10)
fnend
def fn_flat_percent_on_lastbill(percentage)
	percentage=percentage/100
	tg(10)=round(tg(10)+g(11)*percentage,2)
	tg(10)=max(0,tg(10))
	pencolumn(1)=tg(10)
	coltot(1)=coltot(1)+tg(10)
fnend
! def fn_flat_percent_on_priorbal(percentage) ! not really the prior balance - but sorta is
!  percentage=percentage/100
!  pbal=bal-g(11)
!  tg(10)=round(tg(10)+pbal*percentage,2)
!  tg(10)=max(0,tg(10))
!  pencolumn(1)=tg(10)
!  coltot(1)=coltot(1)+tg(10)
! fnend
def fn_flat_amt(penaltyAmount)
	if basepenalty>0 then
		tg(10)=penaltyAmount
	end if
fnend
include: ertn
