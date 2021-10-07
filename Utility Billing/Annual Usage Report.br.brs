! Formerly S:\acsUB\ubUsage2
 
	autoLibrary
	on error goto Ertn
	fnTop(program$)
! r: set constants, dims, etc
	dim cd1(12),e$*30,u1(13)
	dim total_usage_route(13)
	dim total_usage_route_code_date(10,13)
	dim total_count_route(13)
	dim total_count_route_code_date(10,13)
 
	dim total_usage_grand(13)
	dim total_usage_grand_code_date(10,13)
	dim total_count_grand(13)
	dim total_count_grand_code_date(10,13)
	dim msgline$(2)*40,tg(11)
 
 
 
	fnLastBillingDate(d1)
	magicdate=fndate_mmddyy_to_ccyymmdd(d1)-20000
 
	dim serviceName$(10)*20
	dim srv$(10)*2
	fnGetServices(mat serviceName$,mat srv$)
 
	dim opt_service_to_analyze$(3)*20,opt_accum_type$(2)
	opt_service_to_analyze$(1)="Water"
	if srv$(3)="EL" then opt_service_to_analyze$(2)=serviceName$(3)
	if srv$(4)="GA" then opt_service_to_analyze$(3)=serviceName$(4)
 
	dim opt_accum_type$(2)*60
	opt_accum_type$(accum_type_total:=1)='Total'
	opt_accum_type$(accum_type_average:=2)='Average'
! /r
! r: open files
	open #hTrans=fnH: "Name=[Q]\UBmstr\UBTransVB.h[cno],KFName=[Q]\UBmstr\UBTrIndx.h[cno],Shr",i,i,k
! open #hCustomer=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",i,i,k
	open #hCustomer=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx5.h[cno],Shr",i,i,k
! /r
! r: get default answers
	dim scr1_resp$(14)*80
	fnreg_read('Company.[cno].ubusage2.last_run_date',last_run_date$)
	if last_run_date$<>'' and days(last_run_date$,'ccyy/mm/dd')+30>days(date$) then ! if run within the last 30 days do this
		for resp_item=1 to udim(mat scr1_resp$)
			fnreg_read('Company.[cno].ubusage2.answer.'&str$(resp_item),scr1_resp$(resp_item))
		next resp_item
	else
BDR_READ: !
		read #hCustomer,using 'form pos 1,c 10',release: z$ eof EO_BUILD_DEFAULT_RESP ! get the first account number
		restore #hTrans,key>=z$&"         ": nokey BDR_READ
L230: !
		read #hTrans,using F_TRANS: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof EO_BUILD_DEFAULT_RESP
		! fn_apply_default_rates(wr,er,gr)     this would be historically inaccurate
		if p$<>z$ then goto L300 ! history record must belong to this customer
		if tcode<>1 then goto L230 ! charge transaction
		if tdate<magicdate then goto L230 ! only last two years
! L270: !
		j=j+1
		if j>12 then goto EO_BUILD_DEFAULT_RESP
		scr1_resp$(j)=str$(tdate)
		goto L230
L300: !
		if scr1_resp$(1)="" then goto BDR_READ ! make sure this customer has current charges
EO_BUILD_DEFAULT_RESP: !
	end if
! /r




!  functional well untested, but also unused       def fn_apply_default_rates(s1rateCode,s3rateCode,s4rateCode)
!  functional well untested, but also unused       dim extra(21),a(7)
!  functional well untested, but also unused       	a(1)=s1rateCode
!  functional well untested, but also unused       	a(3)=s3rateCode
!  functional well untested, but also unused       	a(4)=s4rateCode
!  functional well untested, but also unused       	fn_apply_default_rates=fnapply_default_rates(mat extra, mat a)
!  functional well untested, but also unused       	s1rateCode=a(1)
!  functional well untested, but also unused       	s3rateCode=a(3)
!  functional well untested, but also unused       	s4rateCode=a(4)
!  functional well untested, but also unused       fnend


SCREEN1: ! r:
	restore #hCustomer:
	fnTos(sn$="ubusage2")
	rc=0
	fnLbl(1,1,"Billing dates to be printed:",35,1)
	fnTxt(2,1,10,10,0,"3")
	rc+=1 : scr1_resp$(rc)=scr1_resp$(rc)
	fnTxt(2,15,10,10,0,"3")
	rc+=1 : scr1_resp$(rc)=scr1_resp$(rc)
	fnTxt(2,29,10,10,0,"3")
	rc+=1 : scr1_resp$(rc)=scr1_resp$(rc)
	fnTxt(2,43,10,10,0,"3")
	rc+=1 : scr1_resp$(rc)=scr1_resp$(rc)
	fnTxt(2,57,10,10,0,"3")
	rc+=1 : scr1_resp$(rc)=scr1_resp$(rc)
	fnTxt(2,71,10,10,0,"3")
	rc+=1 : scr1_resp$(rc)=scr1_resp$(rc)
	fnTxt(4,1,10,10,0,"3")
	rc+=1 : scr1_resp$(rc)=scr1_resp$(rc)
	fnTxt(4,15,10,10,0,"3")
	rc+=1 : scr1_resp$(rc)=scr1_resp$(rc)
	fnTxt(4,29,10,10,0,"3")
	rc+=1 : scr1_resp$(rc)=scr1_resp$(rc)
	fnTxt(4,43,10,10,0,"3")
	rc+=1 : scr1_resp$(rc)=scr1_resp$(rc)
	fnTxt(4,57,10,10,0,"3")
	rc+=1 : scr1_resp$(rc)=scr1_resp$(rc)
	fnTxt(4,71,10,10,0,"3")
	rc+=1 : scr1_resp$(rc)=scr1_resp$(rc)
	fnLbl(6,1,"Service to Analyze:",24,1,0)
	fncomboa("ubusage21",6,26,mat opt_service_to_analyze$,"",13)
	rc+=1
	if trim$(scr1_resp$(rc))='' then scr1_resp$(rc)=opt_service_to_analyze$(1)
	fnLbl(8,1,"Accumulation Type:",24,1,0)
	fncomboa("ubusage21",8,26,mat opt_accum_type$)
	rc+=1
	if trim$(scr1_resp$(rc))='' then scr1_resp$(rc)=opt_accum_type$(1)
	fnCmdSet(2)
	fnAcs(mat scr1_resp$,ckey)
	if ckey=5 then goto Xit
	for j=1 to 12
L560: !
		x=pos(scr1_resp$(j),"/",1)
		if x>0 then scr1_resp$(j)(x:x)="": goto L560
		cd1(j)=val(scr1_resp$(j)) conv INVALID_DATES_MSGBOX
	next j
	if cd1(1)=0 then goto INVALID_DATES_MSGBOX
	if scr1_resp$(13)="Water" then codepos=143: service=1
	if scr1_resp$(13)=trim$(opt_service_to_analyze$(2)) then codepos=147: service=3
	if scr1_resp$(13)=trim$(opt_service_to_analyze$(3)) then codepos=149 : service=4
	accum_type=max(1,srch(mat opt_accum_type$,scr1_resp$(14)))
! /r
! r: save answers
	fnreg_write('Company.[cno].ubusage2.last_run_date',date$('ccyy/mm/dd'))
	for resp_item=1 to udim(mat scr1_resp$)
		fnreg_write('Company.[cno].ubusage2.answer.'&str$(resp_item),scr1_resp$(resp_item))
	next resp_item
! /r
	fnopenprn
F_OUT: form pos 1,c 12,c 25,13*pic(----,---,---)
	gosub HDR
	do
		read #hCustomer,using F_CUSTOMER: z$,e$,s4_deposit_date,servicecode,route eof DONE
		if route<>route_prior and route_prior<>0 then
			fn_print_total("Route "&str$(route_prior)&" Totals",mat total_usage_route,mat total_usage_route_code_date,mat total_count_route,mat total_count_route_code_date)
			pr #255: newpage
			mat total_usage_route(13)=(0)
			mat total_usage_route_code_date(10,13)=(0)
			mat total_count_route(13)=(0)
			mat total_count_route_code_date(10,13)=(0)
		end if
		route_prior=route
F_CUSTOMER: form pos 1,c 10,x 30,c 30,pos 213,pd 4,pos codepos,pd 2,pos 1741,n 2
!   restore #hTrans,key>=z$&"         ": nokey TRANS_NOKEY
		restore #hTrans,key>=z$&rpt$(chr$(0),9): nokey TRANS_NOKEY
		accum_average_divider=0
READ_TRANSACTION: !
		read #hTrans,using F_TRANS: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof CUSTOMER_RECORD_FINIS
F_TRANS: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
! if trim$(z$)='101379.00' and tdate=20141001 then pr z$;tdate : pause !   restore #hTrans,key>=z$&"         ": nokey TRANS_NOKEY
		if p$<>z$ then goto CUSTOMER_RECORD_FINIS ! history record must belong to this customer
		if tcode<>1 then goto READ_TRANSACTION ! charge transactions only
! r: determine usage
		if service=1 then ! analyzing water
			usage=wu
		else if service=3 then ! analyzing electric
			usage=eu
		else if service=4 then ! analyzing gas
			usage=gu
		end if
! /r
! r: accumulate
		cd1_which=srch(mat cd1,tdate)
 
		if cd1_which>0 then
			if accum_type=accum_type_average then
				accum_average_divider+=1
			end if
 
			if usage<>0 then
				total_count_grand(cd1_which)=total_count_grand(cd1_which)+1
				total_count_grand(13)=total_count_grand(13)+1
				u1(cd1_which)=u1(cd1_which)+usage
				u1(13)=u1(13)+usage
			end if
		end if
! /r
		goto READ_TRANSACTION
CUSTOMER_RECORD_FINIS: !
		if accum_type=accum_type_average then
!     math$=str$(u1(13))&'/'&str$(max(1,accum_average_divider))
			u1(13)=u1(13)/max(1,accum_average_divider)
		end if
		if sum(u1)<>0 then
!     cd1_which=srch(mat cd1,tdate)
			for cd1_item=1 to 12
				total_usage_grand(cd1_item)+=u1(cd1_item) ! usage
				total_usage_grand(13)+=u1(cd1_item) ! usage
				total_usage_route(cd1_item)+=u1(cd1_item) ! usage
				total_usage_route(13)+=u1(cd1_item) ! usage
				if servicecode<1 or servicecode>9 then servicecode=10
 
				total_count_route_code_date(servicecode,13)+=1
				total_count_route_code_date(servicecode,cd1_item)+=1
				total_usage_route_code_date(servicecode,13)+=u1(cd1_item) ! usage
				total_usage_route_code_date(servicecode,cd1_item)+=u1(cd1_item) ! usage
 
				total_count_grand_code_date(servicecode,13)+=1
				total_count_grand_code_date(servicecode,cd1_item)+=1
				total_usage_grand_code_date(servicecode,13)+=u1(cd1_item) ! usage
				total_usage_grand_code_date(servicecode,cd1_item)+=u1(cd1_item) ! usage
			next cd1_item
			pr #255,using F_OUT: z$,e$(1:25),mat u1 pageoflow NEWPGE
		end if
		mat u1=(0)
	loop
TRANS_NOKEY: ! r:
	pr #255: z$&" has no transactions"
	goto CUSTOMER_RECORD_FINIS ! /r
NEWPGE: ! r:
	pr #255: newpage
	gosub HDR
	continue  ! /r
HDR: ! r:
	pg+=1
	pr #255: "\qc  {\f181 \fs18 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs22 \b "&env$('Program_Caption')&" - "&scr1_resp$(13)&"}"
	pr #255: "\qc  {\f181 \fs16 \b "&date$("Month DD, CCYY")&"}"
	pr #255: "\ql "
	if accum_type=accum_type_average then
		pr #255,using 'form pos 1,C 284': "{\ul Account No}  {\ul Customer Name            }  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(1))&"}  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(2))&"}  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(3))&"}  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(4))&"}  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(5))&"}  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(6))&"}  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(7))&"}  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(8))&"}  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(9))&"}  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(10))&"}  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(11))&"}  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(12))&"}  {\ul   Average  }"
	else if accum_type=accum_type_total then
		pr #255,using 'form pos 1,C 284': "{\ul Account No}  {\ul Customer Name            }  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(1))&"}  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(2))&"}  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(3))&"}  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(4))&"}  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(5))&"}  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(6))&"}  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(7))&"}  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(8))&"}  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(9))&"}  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(10))&"}  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(11))&"}  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(12))&"}  {\ul   Total    }"
	end if
return  ! /r
def fn_print_total(totals_heading$*80,mat total_usage_grand,mat total_usage_grand_code_date,mat total_count_grand,mat total_count_grand_code_date)
		pr #255: ""
		pr #255: totals_heading$
		pr #255: ""
		for j=1 to 10
			if total_count_grand_code_date(j,13)<>0 then
				pr #255,using F_OUT: "","  Code "&str$(j)&" Total Customers",total_count_grand_code_date(j,1),total_count_grand_code_date(j,2),total_count_grand_code_date(j,3),total_count_grand_code_date(j,4),total_count_grand_code_date(j,5),total_count_grand_code_date(j,6),total_count_grand_code_date(j,7),total_count_grand_code_date(j,8),total_count_grand_code_date(j,9),total_count_grand_code_date(j,10),total_count_grand_code_date(j,11),total_count_grand_code_date(j,12),total_count_grand_code_date(j,13)
				pr #255,using F_OUT: "","  Code "&str$(j)&" Total Usage",total_usage_grand_code_date(j,1),total_usage_grand_code_date(j,2),total_usage_grand_code_date(j,3),total_usage_grand_code_date(j,4),total_usage_grand_code_date(j,5),total_usage_grand_code_date(j,6),total_usage_grand_code_date(j,7),total_usage_grand_code_date(j,8),total_usage_grand_code_date(j,9),total_usage_grand_code_date(j,10),total_usage_grand_code_date(j,11),total_usage_grand_code_date(j,12),total_usage_grand_code_date(j,13)
			end if
		next j
		pr #255,using F_OUT: "","  Total Customers",mat total_count_grand
		pr #255,using F_OUT: "","  Total Usage",mat total_usage_grand
fnend
DONE: ! r:
	close #hCustomer:
	if file(255)=-1 then goto Xit ! printer never opened
	fn_print_total("Route "&str$(route_prior)&" Totals",mat total_usage_route,mat total_usage_route_code_date,mat total_count_route,mat total_count_route_code_date)
	fn_print_total("Grand Totals",mat total_usage_grand,mat total_usage_grand_code_date,mat total_count_grand,mat total_count_grand_code_date)
	fncloseprn
	goto Xit ! /r
Xit: fnXit
include: ertn
INVALID_DATES_MSGBOX: ! r:
	msgline$(1)="You have entered dates in an"
	msgline$(2)="invalid format.  Use mmddyy format."
	fnmsgbox(mat msgline$,resp$,'',1)
goto SCREEN1 ! /r
