forceRollBackNotMostRecentRec=0
library 'S:\Core\Library': fnxit,fnopenprn,fncloseprn,fnerror,fnLastBillingDate,fngethandle,fntop,fnTos,fnLbl,fnTxt,fncmbact,fncmbrt2,fnCmdSet,fnAcs,fnmsgbox,fnOpt,fnget_services,fnAutomatedSavePoint
on error goto ERTN
! msgbox("Reverse Billing Cycle is currently under construction.","Reverse Billing Cycle Unavailable","OK","Inf") : if env$('ACSDeveloper')='' then goto XIT
fn_undobilling
goto XIT
def fn_undobilling ! main
	dim billingdate$*10,msgtext$(1)*1000,readings(12),charges(12),breakdown(10),readingdates(2),serviceName$(10)*20
	do_all=1 : do_route=2 : do_individual=3
	fntop(program$)
	fnget_services(mat serviceName$,mat serviceCode$,mat tax_code$,mat penalty$,mat subjectto,mat ordertoapply)
	! 
	ASK_OPTIONS: ! 
	cont=fn_options(route,billingdate$) ! collect user options
	if ckey=5 then goto XIT_FN_UNDOBILLING
	if trim$(billingdate$)="0" then 
		mat msgtext$(1)
		msgtext$(1)=("You must enter a billing date")
		fnmsgbox(mat msgtext$,answer$,"Invalid Entry",0)
		goto ASK_OPTIONS
	end if 
 ! 
	mat msgtext$(3)
	msgtext$(1) = "Warning: this action will reduce the balance and balance breakdown of all selected customers"
	msgtext$(2) = "with a matching billing date by the amount of the billing on that date."
	msgtext$(3) = "Are you sure?"
	fnmsgbox(mat msgtext$,answer$,"Confirm Action",4)
	if (answer$<>"Yes") then cont=0
	! 
	undoCount=0
	if cont then 
		dim acct$*10,custname$*30,trcust$(3)*10,trdate(3),tramt(3),srvamt1(11),srvamt2(11),srvamt3(11),srvread1(6),srvread2(6),srvread3(6),trbal(3)
		CUSTFORM: form c 10,x 30,c 30,pos 1741,n 2,pos 217,12*pd 5,pos 292,pd 4.2,pd 4,12*pd 4.2,pos 388,10*pd 5.2,pos 1750,2*n 6
		TRANSFORM: form c 10,n 8,x 1,12*pd 4.2,6*pd 5,pd 4.2
		fnAutomatedSavePoint('before')
		fn_openfiles ! open data files
		fnopenprn : fn_printheader
		do 
			NEXT_CUSTOMER: ! 
			if filter=do_individual then 
				read #h_customer,using CUSTFORM,key=lpad$(cust$,kln(h_customer)): acct$,custname$,custroute,mat readings,balance,chargedate,mat charges,mat breakdown,mat readingdates
			else 
				read #h_customer,using CUSTFORM: acct$,custname$,custroute,mat readings,balance,chargedate,mat charges,mat breakdown,mat readingdates eof CUSTDONE ! get every customer one at a time
			end if 
					! if trim$(acct$)='1000000.01' then pause
			if filter<>do_route or custroute=route then ! if a route was selected and customer doesn't match, skip customer
				if fn_get3trans(acct$,billingdate$,lastdate,priordate,priordate2) then ! get latest and 2 prior charge transactions for this customer
					! get the three latest transactions and their data; most recent is in first array element, prior in second, prior2 in third
					if priordate>0 then 
						read #h_trans,using TRANSFORM,key=lpad$(acct$,10)&str$(priordate)&"1": trcust$(2),trdate(2),tramt(2),mat srvamt2,mat srvread2,trbal(2) nokey NEXT_CUSTOMER
					end if 
					if priordate2>0 then 
						read #h_trans,using TRANSFORM,key=lpad$(acct$,10)&str$(priordate2)&"1": trcust$(3),trdate(3),tramt(3),mat srvamt3,mat srvread3,trbal(3) nokey NEXT_CUSTOMER
					end if 
					undoCount+=1
					read #h_trans,using TRANSFORM,key=lpad$(acct$,10)&str$(lastdate)&"1": trcust$(1),trdate(1),tramt(1),mat srvamt1,mat srvread1,trbal(1)
					if priordate>0 then 
						! update readings
						readings(4)-=readings(3) ! roll back YTD usage
						readings(8)-=readings(7)
						readings(12)-=readings(11)
						readings(1)=srvread2(1) ! update all current readings and usage
						readings(3)=srvread2(2)
						readings(5)=srvread2(3)
						readings(7)=srvread2(4)
						readings(9)=srvread2(5)
						readings(11)=srvread2(6)
						if priordate2>0 then ! update all prior readings
							readings(2)=srvread3(1)
							readings(6)=srvread3(3)
							readings(10)=srvread3(5)
						end if 
						! update charge date
						chargedate=val(date$(days(trdate(2),"ccyymmdd"),"mmddyy"))
						! update charges
						mat charges(1:11)=srvamt2(1:11)
						charges(12)=srvamt2(11)+srvamt2(10)
						! update breakdowns
						!             if trim$(acct$)='100480.01' then pr acct$ : pause
						for breakdown_item=1 to 9
							if penalty$(breakdown_item)<>'Y' then 
								breakdown(breakdown_item)=breakdown(breakdown_item)-srvamt1(breakdown_item)
							end if 
						next breakdown_item
						! mat breakdown(1:9)=breakdown(1:9)-srvamt1(1:9)
						! update current balance
						balance-=srvamt1(11)
						! update reading dates; guess at prior reading date
						readingdates(2)=readingdates(1)
						readingdates(1)=val(date$(days(priordate2,"ccyymmdd"),"mmddyy"))
						! update last billing date
						if priordate>lastbilling then lastbilling=priordate
					else 
						mat readings(1:12)=(0) : mat charges(1:12)=(0) : balance=0 : chargedate=0 : mat breakdown(1:10)=(0) : mat readingdates(1:2)=(0)
					end if 
					! rewrite customer master record
					rewrite #h_customer,using CUSTFORM: acct$,custname$,custroute,mat readings,balance,chargedate,mat charges,mat breakdown,mat readingdates
					! delete rolled-back transaction
					delete #h_trans: 
					pr #255,using "form pos 5,c 10,x 5,pic(zz/zz/zz)": trcust$(1),str$(trdate(1)) pageoflow PRINTPAGEOVERFLOW
				end if 
			end if 
		loop while filter<>do_individual
		goto CUSTDONE
		PRINTPAGEOVERFLOW: ! r:
		 pr #255: newpage
		 fn_printheader
		continue ! /r
		CUSTDONE: ! 
		if filter=do_all then 
			lastbilling=val(date$(days(lastbilling,"ccyymmdd"),"mmddyy"))
			fnLastBillingDate(lastbilling,1)
		end if 
		mat msgtext$(1)=("Customers reversed: "&str$(undoCount))
		fnmsgbox(mat msgtext$,answer$,"Report",0)
		fncloseprn
		fn_close_files
		if filter=do_individual then goto ASK_OPTIONS
	end if 
	! 
	XIT_FN_UNDOBILLING: ! 
fnend  ! fn_UndoBilling
XIT: fnxit
def fn_options(&route,&billingdate$) ! show options dialog to user and return selections
	dim screen_name$*100,resp$(20)*255
	fnLastBillingDate(lastbilling) ! get last billing date and use it for the default
	filter=0 : route=0 : cust$=''
	OPTIONS_TOS: ! 
	fnTos(screen_name$="UndoBillingOptions")
	rcnt=0 : lc=0 : pos_col2=16
	lc+=1
	fnLbl(lc+=1,2,"Warning: only the most recent billing date can be reversed for any account(s).")
	lc+=1
! billing date text box
	fnLbl(lc+=1,2,"Billing Date:",13,1)
	fnTxt(lc,pos_col2,8,0,0,"1001")
	resp_billing_date=rcnt+=1
	if resp$(resp_billing_date)='' then resp$(resp_billing_date)=str$(lastbilling)

	lc+=1
	lc+=1
	fnLbl(lc+=1,2,"Use only one of options below to limit the customers to reverse.")
	lc+=1

	fnOpt(lc+=1,1,'All') ! fnOpt(lyne,ps, txt$*196; align,contain,tabcon)
	resp_opt_all=rcnt+=1
	if resp$(resp_opt_all)='' then resp$(resp_opt_all)='True'
! 
	fnOpt(lc+=1,1,'Route:')
	resp_opt_route=rcnt+=1
	if resp$(resp_opt_route)='' then resp$(resp_opt_route)='False'
	fncmbrt2(lc,pos_col2,1)
	resp_route=rcnt+=1
! if resp$(resp_route)='' then resp$(resp_route)="[All]"
! 
	fnOpt(lc+=1,1,'Individual:')
	resp_opt_individual=rcnt+=1
	if resp$(resp_opt_individual)='' then resp$(resp_opt_individual)='False'
	fncmbact(lc,pos_col2) ! fncmbact(lyne,mypos; addall,c,a$*25)
	resp_individual=rcnt+=1
! if resp$(resp_individual)='' then resp$(resp_individual)="[All]"
! 
	fnCmdSet(2) ! show "Next" and "Cancel" buttons
	fnAcs(screen_name$,0,mat resp$,ckey) ! run the screen
	 ! if env$('ACSDeveloper')<>'' then pause
	if ckey=5 then ! if user pressed Cancel
		fn_options=0
	else 
		billingdate$=resp$(resp_billing_date)
		if resp$(resp_opt_individual)='False' and resp$(resp_individual)<>'[All]' and resp$(resp_individual)<>'' t
		  resp$(resp_opt_individual)='True'
			resp$(resp_opt_route)='False'
			resp$(resp_opt_all)='False'
		en if
		if resp$(resp_opt_all)='True' then 
			filter=do_all
		else if resp$(resp_opt_route)='True' then 
			filter=do_route
			route=val(resp$(resp_route))
			if route=0 then pr bell;'please select a route' : goto OPTIONS_TOS
		else if resp$(resp_opt_individual)='True' then 
			filter=do_individual
			cust$=trim$(resp$(resp_individual)(1:10))
			if trim$(cust$)='' then pr bell;'please select a customer' : goto OPTIONS_TOS
		end if 
!     pr 'answers retreived' : pause  !
		fn_options=1
	end if 
fnend  ! fn_Options
def fn_openfiles
	open #h_customer:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno]",internal,outIn,keyed 
	open #h_trans:=fngethandle: "Name=[Q]\UBmstr\ubtransvb.h[cno],KFName=[Q]\UBmstr\ubtrindx.h[cno]",internal,outIn,keyed 
fnend 
def fn_close_files
	close #h_customer: ioerr ignore
	close #h_trans: ioerr ignore
fnend 
def fn_get3trans(acct$,billingdate$,&lastdate,&priordate,&priordate2)
	dim transacct$*10,transacct2$*10
	CUSTTRANSFORM: form c 10,n 8,n 1
	! 
	lastdate=0 : priordate=0 : priordate2=0
	dateshouldbe=date(days(val(billingdate$),"mmddyy"),"ccyymmdd") : if str$(dateshouldbe)(1:2)="19" then dateshouldbe+=1000000
	! 
	! first, check for the transaction for this customer on the date specified for rollback; if not found, exit
	read #h_trans,using CUSTTRANSFORM,key=lpad$(acct$,10)&str$(dateshouldbe)&"1": transacct$,transdate,transcode eof GOTTRANS nokey GOTTRANS
	if forceRollBackNotMostRecentRec then goto NOLATERTRANS
	! next, see if there are any later charge transactions; if so, exit (cannot roll back any date except the most recent)
	read #h_trans,using CUSTTRANSFORM,next: transacct2$,transdate,transcode eof NOLATERTRANS
	if transcode=1 and transacct2$=transacct$ then goto GOTTRANS
	! 
	NOLATERTRANS: !
	do  ! finally, read back up file to get 2 prior transaction dates
		lastdate=dateshouldbe
		read #h_trans,using CUSTTRANSFORM,prior: transacct2$,transdate,transcode eof GOTTRANS
		if transacct2$=transacct2$ and transcode=1 and transdate<lastdate then 
			if priordate=0 then 
				priordate=transdate
			else if priordate2=0 then 
				priordate2=transdate
			else 
				goto GOTTRANS
			end if 
		end if 
	loop while transacct2$=transacct$ and (priordate=0 or priordate2=0)
	! 
	GOTTRANS: !
	if lastdate=0 then let fn_get3trans=0 else let fn_get3trans=1
fnend 
def fn_printheader
	pg+=1
	pr #255: "Reverse Calculation Status Report"
	pr #255: "Page "&str$(pg)
	pr #255: ""
	pr #255: "All accounts listed have been reversed."
	pr #255: ""
	pr #255: "Account           Billing Date"
	pr #255: "_______________   ____________"
fnend 
include: ertn