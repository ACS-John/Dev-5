! formerly S:\acsUB\UBUsage
! r: setup library, on err, dims, constants, etc
autoLibrary
on error goto Ertn
fnTop(program$)

dim z$*10,name$*30,dx(15)
dim totalRoute(3,2),line$*212
dim serviceName$(10)*20,srv$(10)*2
dim resp$(11)*128

fnLastBillingDate(filterBillingDate)
fnGetServices(mat serviceName$,mat srv$)
if trim$(serviceName$(1))<>'' then service1enabled=1
if trim$(serviceName$(3))="Electric" or trim$(srv$(3))="EL" or trim$(serviceName$(3))="Lawn Meter" then service3enabled=1
if trim$(serviceName$(4))="Gas" or trim$(srv$(4))="GA" then service4enabled=1
sequenceRoute=1
sequenceAccount=2
open #h_trans=fnH: "Name=[Q]\UBmstr\UBTransVB.h[cno],KFName=[Q]\UBmstr\UBTrIndx.h[cno],Shr",i,i,k
open #h_customer_1=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",i,i,k ! 1
	! /r
SCREEN1: ! r:
	fnTos
	respc=0
	fnLbl(2,1,"Billing Date:",32,1)
	fnTxt(2,34,8,0,1,"1")
	resp$(resp_billingDate:=respc+=1)=str$(filterBillingDate)
	fnLbl(3,1,"Route Number:",32,1)
	fncmbrt2(3,34)
	resp$(respc_routeFilter:=respc+=1)="[All]"
	fnLbl(5,1,"Sequence:",32,1)
	fnOpt(5,34,'Route/Sequence  (includes totals by route)')
	resp$(respc_sequenceRoute:=respc+=1)='True'
	fnOpt(6,34,'Account')
	resp$(respc_sequenceAccount:=respc+=1)='False'
	! chkrow=6
	! only include only those billed for sewer
	! for servicecount=1 to udim(servicename$)
		! if trim$(Servicename$(servicecount))<>"" then
			! mat resp$(udim(resp$)+1)
			! chkrow+=2
			fnChk(8,34,"Only include those billed for sewer",1)
			resp$(respc_OnlySewer:=respc+=1)="False"
		! end if
	! next servicecount
	fnCmdSet(3)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	filterBillingDate=val(resp$(resp_billingDate))
	if trim$(resp$(respc_routeFilter))="[All]" then
		filterRoute=0
	else
		filterRoute=val(resp$(respc_routeFilter))
	end if
	if filterBillingDate<10100 or filterBillingDate>123199 then goto SCREEN1
	if resp$(respc_sequenceRoute)='True' then reportSequence=sequenceRoute
	if resp$(respc_sequenceAccount)='True' then reportSequence=sequenceAccount
	filterBillingDateCcyymdd=fndate_mmddyy_to_ccyymmdd(filterBillingDate)
	if reportSequence=sequenceAccount then
		open #hCustomerForReport=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",i,i,k
	else ! if reportSequence=sequenceRoute then
		open #hCustomerForReport=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx5.h[cno],Shr",i,i,k
	end if
	if filterRoute then
		if reportSequence=sequenceRoute then restore #hCustomerForReport,key>=lpad$(str$(filterRoute),2)&"       ": nokey SCREEN1
		! routePrior=filterRoute
	end if
! /r
fnopenprn
gosub HDR
do ! r: main report loop
	read #hCustomerForReport,using F_Customer: z$,name$,mat dx,CustomerLastBillingDate,route eof TOTAL_AND_FINISH
	F_Customer: form pos 1,c 10,pos 41,C 30,pos 217,15*pd 5,pos 296,pd 4,pos 1741,n 2
	if resp$(respc_OnlySewer)="True" and fn_hasSewer(z$)=0 then goto nextcustomer
	if reportSequence=sequenceAccount and filterRoute and route<>filterRoute then goto NextCustomer
	dx(1)=dx(3)=dx(5)=dx(7)=dx(9)=dx(11)=0
	restore #h_trans,key>=z$&"         ": nokey L570
	do
		read #h_trans,using 'Form POS 1,C 10,N 8,pos 68,6*PD 5': transAcct$,transDate,trans_s1reading,trans_s1usage,trans_s3reading,trans_s3usage,trans_s5reading,trans_s5usage eof L570
	loop until transDate=filterBillingDateCcyymdd or transAcct$<>z$
	if transDate=filterBillingDateCcyymdd then
		dx(1) =trans_s1reading
		dx(3) =trans_s1usage
		dx(5) =trans_s3reading
		dx(7) =trans_s3usage
		dx(9) =trans_s5reading
		dx(11)=trans_s5usage
	end if
	L570: !
	if filterRoute and route<>filterRoute then
			goto TOTAL_AND_FINISH
	end if
	if route<>routePrior then
		if routePrior<>0 then
			fn_printTotals(1)
		end if
		routePrior=route
	end if
	gosub PRINT_DETAILS
	NextCustomer: !
loop ! /r
FINIS: ! r:
	close #hCustomerForReport: ioerr ignore
	fncloseprn
goto Xit ! /r

NEWPGE: ! r:
	pr #255: newpage
	gosub HDR
continue  ! /r
HDR: ! r:
	if ~setupHeader then
		setupHeader=1
		! r: set heading$ and heading2$
		dim heading$*212
		dim heading2$*212
		if service1enabled+service3enabled+service4enabled=1 then
			heading$=rpt$(' ',16+7)   !  i don't know why, but it works.
		else
			heading$=rpt$(' ',16)
		end if
		heading2$=''
		reportWidth=32
		if service1enabled then
			heading$&=" {\ul            Water           }"
			heading2$&=" {\ul   Reading   Current       YTD}"
			reportWidth+=30
		end if
		if service3enabled then
			heading$&="   {\ul       "&serviceName$(3)(1:12)&"         }"
			heading2$&=" {\ul   Reading   Current       YTD}"
			reportWidth+=30
		end if
		if service4enabled then
			heading$&="   {\ul       "&serviceName$(4)(1:12)&"         }"
			heading2$&=" {\ul   Reading   Current       YTD}" ! gas used for something other than gas
			reportWidth+=30
		end if
		! /r
	end if
	pr #255: "\qc  {\f181 \fs18 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs24 \b "&env$('Program_Caption')&"}"
	pr #255: "\qc  {\f181 \fs16 \b "&date$("Month DD, CCYY")&"}"
	pr #255: "\qc  {\f181 \fs16 \b Billing Date: "&cnvrt$('pic(##/##/##)',filterBillingDate)&"}"
	pr #255,using 'form pos 1,Cr reportWidth': "Page "&str$(p2+=1)
	pr #255: heading$
	pr #255: "{\ul  Account  } {\ul Name                } "&heading2$
return  ! /r
PRINT_DETAILS: ! r:
	if sum(mat dx(3:12))<>0 and days(CustomerLastBillingDate,'mmddyy')=>days(filterBillingDate,'mmddyy') then
		line$=z$&' '&name$(1:21)
		if service1enabled then
			line$&=cnvrt$("n 10",dx(1))&cnvrt$("n 10",dx(3))&cnvrt$("n 10",dx(4))
		end if
		if service3enabled then
			line$&=cnvrt$("n 10",dx(5))&cnvrt$("n 10",dx(7))&cnvrt$("n 10",dx(8))
		end if
		if service4enabled then
			line$&=cnvrt$("n 10",dx(9))&cnvrt$("n 10",dx(11))&cnvrt$("n 10",dx(12))
		end if
		pr #255: line$ pageoflow NEWPGE
		oldroute=route
		totalRoute(1,1)+=dx(3)
		totalRoute(1,2)+=dx(4)
		totalRoute(2,1)+=dx(7)
		totalRoute(2,2)+=dx(8)
		totalRoute(3,1)+=dx(11)
		totalRoute(3,2)+=dx(12)
	end if
return  ! /r
TOTAL_AND_FINISH: ! r:
	fn_printTotals(0)
goto FINIS ! /r
def fn_printTotals(totalRoute)
	dim totalGrand(3,2)
	mat totalGrand=totalGrand+totalRoute
	if reportSequence=sequenceRoute then
		pr #255: '' ! pageoflow NEWPGE
		pr #255: '' ! pageoflow NEWPGE
		pr #255: tab(40);"Totals for Route Number ";routePrior;
		if ~totalRoute then pr #255: tab(75);"Grand Totals";
		pr #255: ''
		pr #255: tab(39);"Current    Year to Date";
		if ~totalRoute then pr #255: tab(69);"Current    Year to Date"

		line$=''
		if trim$(serviceName$(1))<>'' then
			line$=serviceName$(1)(1:11)&line$&cnvrt$("n 10",totalRoute(1,1))&"      "&cnvrt$("n 10",totalRoute(1,2))
			if ~totalRoute then
				line$&="    "&cnvrt$("n 10",totalGrand(1,1))&"      "&cnvrt$("n 10",totalGrand(1,2))
			end if
			pr #255,using "Form POS 25,C 120": line$
		end if

		line$=''
		if service3enabled then
			line$=serviceName$(3)(1:12)&line$&cnvrt$("n 10",totalRoute(2,1))&"      "&cnvrt$("n 10",totalRoute(2,2))
		end if
		if ~totalRoute then
			line$&="    "&cnvrt$("n 10",totalGrand(2,1))&"      "&cnvrt$("n 10",totalGrand(2,2))
		end if
		pr #255,using "Form POS 25,C 120": line$

		line$=''
		if service4enabled then
			line$=serviceName$(4)(1:11)&line$&cnvrt$("n 10",totalRoute(3,1))&"      "&cnvrt$("n 10",totalRoute(3,2))
		end if
		if ~totalRoute then
			line$&="    "&cnvrt$("n 10",totalGrand(3,1))&"      "&cnvrt$("n 10",totalGrand(3,2))
		end if
		pr #255,using "Form POS 25,C 120": line$

		if totalRoute=1 then
			pr #255: newpage
			gosub HDR
		end if
	end if  ! reportSequence=sequenceRoute
	mat totalRoute=(0)
fnend
def fn_hasSewer(acctno$)
	! this function returns 1 if the customer account has a sewer service1enabled
	dim hs_a(7)
	read #h_customer_1,using F_CUSTOMER_1,key=acctno$: hs_z$,mat hs_a
	F_CUSTOMER_1: form pos 1,c 10,x 132,7*pd 2
	if hs_a(2)<>0 then let fn_hasSewer=1 else let fn_hasSewer=0
fnend
Xit: fnXit
include: ertn
