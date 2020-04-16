! formerly S:\acsUB\ubBilJrn
! -- pr Billing Journal
! r: setup
library 'S:\Core\Library': fntop,fnxit
library 'S:\Core\Library': fnopenprn,fncloseprn
library 'S:\Core\Library': fnAcs2,fnLbl,fnTxt,fncmbrt2,fnTos
library 'S:\Core\Library': fnCmdSet,fnChk,fnFra,fnOpt
library 'S:\Core\Library': fndate_mmddyy_to_ccyymmdd
library 'S:\Core\Library': fnLastBillingDate
library 'S:\Core\Library': fncreg_read,fncreg_write
library 'S:\Core\Library': fnget_services
library 'S:\Core\Library': fnStatusPause
library 'S:\Core\Library': fnindex_it
library 'S:\Core\Library': fnpause
library 'S:\Core\Library': fngethandle
on error goto Ertn

dim z$*10,e$(4)*30,g(12)
dim a(7),t1(10,200,3),st$(10)
dim x2(5),d(15),extra(23)
dim hd1$*400,hd2$*400
dim px$(99)*30,px(99),tx(99),gx(99)
dim resp$(20)*128
dim tg(11),usages(3)

fntop(program$)
fnLastBillingDate(billing_date)
fncreg_read('Route Low',bkno1$) : route_number=val(bkno1$)
dim serviceName$(10)*20,tax_code$(10)*1,penalty$(10)*1
fnget_services(mat serviceName$,mat service$,mat tax_code$,mat penalty$)
hd1$='Account                             '
hd2$='{\ul Number   }  {\ul Name                   }  '
for j=1 to 10
	x2=pos(trim$(serviceName$(j)),' ',1)
	if x2>0 then serviceName$(j)=serviceName$(j)(1:2)&'-'&serviceName$(j)(x2+1:len(serviceName$(j)))
	if trim$(serviceName$(j))<>'' then 
		x1=pos (serviceName$(j),' ',1)
		x1=min(x1,7)
		hd1$=hd1$&'---------'
		hd2$=hd2$&'{\ul '&lpad$(trim$(serviceName$(j)(1:x1)),8)&'} '
		sz1+=1 : px$(sz1)=serviceName$(j)
	end if 
next j
! /r
open #h_trans:=2: 'Name=[Q]\UBmstr\UBTransVB.h[cno],KFName=[Q]\UBmstr\UBTrIndx.h[cno],Shr',internal,input,keyed 
open #hRate:=fngethandle: 'Name=[Q]\UBmstr\ubData\RateMst.h[cno],KFName=[Q]\UBmstr\ubData\RateIdx1.h[cno],Shr',internal,input,keyed 
! r: get default sequence
	fncreg_read('ubBilJrn.Sort_Option',sequence$)
	seq=val(sequence$) conv ignore
	if seq<1 then seq=1
! /r
MAIN: ! r: Screen 1
	fnTos
	respc=0
	fnLbl(2,1,'Billing Date:',25,1)
	fnTxt(2,27,8,0,1,'1')
	resp$(respc+=1)=str$(billing_date)
	fnFra(3,1,4,65,'Sort Order','The billing journal can be printed if route number sequence, account sequence or Alpha Sort Sequence.',0)
	fnOpt(1,2,'Route/Sequence Number (includes Subtotals by Route)',0,1)
	if seq=1 then resp$(respc+=1)='True' else resp$(respc+=1)='False'
	fnOpt(2,2,'Account',0,1)
	if seq=2 then resp$(respc+=1)='True' else resp$(respc+=1)='False'
	fnOpt(3,2,'Alpha Sort',0,1)
	if seq=3 then resp$(respc+=1)='True' else resp$(respc+=1)='False'
	fnOpt(4,2,'Customer Name',0,1)
	if seq=4 then resp$(respc+=1)='True' else resp$(respc+=1)='False'
	fnLbl(9,1,'Route Number:',25,1)
	fncmbrt2(9,27)
	resp$(resp_route:=respc+=1)='[All]'
	fnChk(10,27,'Print Usages:',1)
	fncreg_read('ubBilJrn.Print Usages',resp$(resp_print_usages:=respc+=1))
	fnCmdSet(3)
	fnAcs2(mat resp$,ckey)
	if ckey=5 then goto XIT
	billing_date=val(resp$(1))
	if resp$(2)='True' then seq=1 ! route sequence
	if resp$(3)='True' then seq=2 ! account sequence
	if resp$(4)='True' then seq=3 ! Alpha Sort Sequence
	if resp$(5)='True' then seq=4 ! Customer Name Sequence
	if uprc$(resp$(resp_route)) = uprc$('[All]') then resp$(resp_route) = '0'
	prtbkno=val(resp$(resp_route))
	prtusage$=resp$(resp_print_usages)(1:1)
	fncreg_write('ubBilJrn.Sort_Option',str$(seq))
	fncreg_write('ubBilJrn.Print Usages',resp$(resp_print_usages))
! /r
! r: initialize stuff
if seq=0 or seq=1 then ! route number
	open #hCustomer:=fngethandle: 'Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx5.h[cno],Shr',internal,input,keyed 
else if seq=2 then            ! account
	open #hCustomer:=fngethandle: 'Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr',internal,input,keyed 
else if seq=3 then ! Alpha Sort Sequence
	open #hCustomer:=fngethandle: 'Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\UBIndx2.h[cno],Shr',internal,input,keyed  
else if seq=4 then ! Customer Name
	fnindex_it('[Q]\UBmstr\Customer.h[cno]', env$('temp')&'\customer_name'&session$&'.h[cno]','41 30')
	open #hCustomer:=fngethandle: 'Name=[Q]\UBmstr\Customer.h[cno],KFName='&env$('temp')&'\customer_name'&session$&'.h[cno],Shr',internal,input,keyed  
end if
if trim$(serviceName$(1))='Water'                               then services+=1 : water=1
if trim$(serviceName$(3))='Electric' or trim$(service$(3))='LM' then services+=1
if serviceName$(3)(1:5)='Re-Se'                                 then services+=1 : reduc=1 
if trim$(serviceName$(4))='Gas'                                 then services+=1 : gas=1
mat usages(services)
hd1$=hd1$&'---------    Prior  Current'
hd2$=hd2$&'{\ul    Total} {\ul  Balance} {\ul  Balance}  '
x1=int((len(hd1$)-43)/2)+27
hd1$(x1:x1+17)=' Current Billing '
px$(sz1+=1)='   Total'
px$(sz1+=1)='Previous Balance'
px$(sz1+=1)='Current Balance'
mat px$(sz1) : mat tx(sz1) : mat gx(sz1) : mat px(sz1)
if prtusage$='T' and trim$(serviceName$(1))='Water'    then hd2$=hd2$&' {\ul  W-Usage}'
if prtusage$='T' and trim$(serviceName$(3))='Electric' then hd2$=hd2$&' {\ul  E-Usage}'
if prtusage$='T' and trim$(service$(3))='LM'           then hd2$=hd2$&' {\ul LM-Usage}'
if prtusage$='T' and trim$(serviceName$(4))='Gas'      then hd2$=hd2$&' {\ul  G-Usage}'
if prtusage$='T'                                       then hd2$=hd2$&' {\ul Meter Address}'
fnopenprn
gosub PrHeader
if prtbkno<>0 and seq=1 then 
	prtbkno$=rpad$(lpad$(str$(prtbkno),2),kln(hcustomer))
	startcd=1
	restore #hCustomer,key>=prtbkno$: nokey MAIN
end if
! /r
! r: main loop
MainLoopTop: !
	do
		matchFound=0
		read #hCustomer,using F_Customer: z$,mat e$,mat a,mat d,bal,f,mat g,route,estimatedate,mat extra eof EoCustomer
		F_Customer: form pos 1,c 10,4*c 30,pos 143,7*pd 2,pos 217,15*pd 5,pos 292,pd 4.2,pd 4,12*pd 4.2,pos 1741,n 2,pos 1831,n 9,pos 1741,n 2,n 7,2*n 6,n 9,pd 5.2,n 3,3*n 9,3*n 2,3*n 3,n 1,3*n 9,3*pd 5.2
		! if f=billing_date then 
		! 	matchFound=1
		! else ! if f><billing_date then 
			matchFound=fn_readFromHistory 
		! end if
	loop until matchFound
	if seq=1 then  !  route subtotals 
		if startcd=1 and prtbkno<>route then 
			goto EoCustomer
		else if (route_number>0 and route_number<prtbkno) or route_number=route then 
			goto L790
		else 
			if printedCustomersInBookCount>0 then
				printedCustomersInBookCount=0
				gosub PrRouteTotal
				pr #255: newpage
				gosub PrHeader
			end if
		end if
		L790: !
		route_number=route
	end if
	! gosub PrintCustomer
	! PrintCustomer: ! r:
	e=bal-g(11) : j1=0
	for j=1 to 10
		if trim$(serviceName$(j))<>'' then px(j1+=1)=g(j)
	next j
	px(j1+1)=g(11) : px(j1+2)=e : px(j1+3)=bal
	mat tx=tx+px : mat gx=gx+px
	x=0
	if water=1 then x+=1 : usages(x)=d(3)
	if reduc=1 then x+=1 : usages(x)=d(3)-d(7)
	if gas=1   then x+=1 : usages(x)=d(11)
	if estimatedate=billing_date then est$='E' else est$=''
	if prtusage$='T' then 
		pr #255,using L1020: z$,e$(2)(1:23),mat px,est$,mat usages,e$(1)(1:25) pageoflow PgOf 
		L1020: form pos 1,c 10,x 2,c 23,sz1*n 9.2,x 1,c 1,services*n 9,x 1,c 25
	else 
		pr #255,using L1020: z$,e$(2)(1:23),mat px,est$ pageoflow PgOf
	end if
	printedCustomersInBookCount+=1
	gosub AccumulateTotalsByCode
	! If CODEMIS=1 Then
	! pr #255: ' * The Previous Record has a charge, but does not have a matching code!'
	! end if
	codemis=0
	! return  ! /r

	totalBillsToBePrinted+=1
goto MainLoopTop
! /r
PrHeader: ! r:
	pr #255: '\qc  {\f181 \fs18 \b '&env$('cnam')&'}'
	pr #255: '\qc  {\f181 \fs24 \b '&env$('cap')&'}'
	pr #255: '\qc  {\f181 \fs16 \b Billing Date '&cnvrt$('pic(zz/zz/zz)',billing_date)&'  Page '&str$(pge+=1)&'}'
	pr #255: '\qc  {\f181 \fs16 \b '&date$('Month DD, CCYY')&'}'
	pr #255: '\ql   '
	pr #255: hd1$
	pr #255: hd2$
return  ! /r

PgOf: ! r:
	pr #255: newpage
	gosub PrHeader
continue  ! /r
PrRouteTotal: ! r:
	if sum(tx)>0 then
		pr #255: ''
		pr #255: tab(27);'{\ul Totals for Route Number '&str$(route_number)&'}'
		for j=1 to sz1
			!   if trim$(px$(j))<>'Penalty' then ! don't allow any penalties go thur totals
			pr #255,using 'Form POS 1,C 30,N 15.2': px$(j),tx(j)
			!   end if  ! trim$(px$(j))<>'Penalty'
		next j
	end if
	mat tx=(0)
return  ! /r
EoCustomer: ! r:
! close #hCustomer: ioerr ignore
	if sum(tx) or sum(gx) then
		if seq<>1 then 
			pr #255: ''
			pr #255: tab(27);'{\ul Totals for All Routes }       {\ul Grand Totals}'
		else
			pr #255: ''
			pr #255: tab(27);'{\ul Totals for Route Number '&str$(route_number)&'}       {\ul Grand Totals}'
		end if
		for j=1 to sz1
			! if trim$(px$(j))<>'Penalty' then ! don't allow any penalties go thur totals
			if tx(j) or gx(j) then
				pr #255,using 'Form POS 1,C 30,N 15.2,X 6,N 15.2': px$(j),tx(j),gx(j)
			end if
			! end if  ! trim$(px$(j))<>'Penalty' then
		next j
	end if
	gosub PrTotalsByCode
	close #hCustomer: ioerr ignore
	close #hRate : ioerr ignore
	fncloseprn
goto Xit ! /r
Xit: fnxit

AccumulateTotalsByCode: ! r: ACCUMULATE TOTALS BY CODE
	for j=1 to 10
		if trim$(serviceName$(j))='' or uprc$(penalty$(j))='Y' then goto L1720 ! don't allow any penalties go thur totals
		x2=1 : u2=0
		if j<=4 then 
			x2=a(j)
			if j=1 or j=2 then 
				u2=d(3)
			else if j=3 and (trim$(serviceName$(3))='Electric' or trim$(serviceName$(3))='Lawn Meter') then 
				u2=d(7)
			else if j=4 and trim$(serviceName$(4))='Gas' then 
				u2=d(11)
			end if 
		end if 
		if j=9 or j=10 then 
			x2=a(j-3)
		else if j=6 or j=7 or j=8 then 
			x2=extra(j+5)
		else if j=5 then 
			if env$('client')='French Settlement' then 
				x2=a(4)
			else 
				x2=a(5)
			end if 
		end if 
		! r: calculate U2 - U2 is the temporary Tax Base accumulator
			if env$('client')='French Settlement' and j=8 then u2=0 : goto L1690 ! if Other Charge do not add to tax base
			if env$('client')='Carrizo' and j=3 then u2=0 : goto L1520 ! allow canister rental to go thru tax
			if j=2 and reduc=1 then u2=d(3)-d(7)
			if j<5 then goto L1690 ! don't allow real usage to go thru taxable routine
			L1520: ! 
			for j3=1 to 10
				if env$('client')='Carrizo' then 
					if j=7 and extra(12)<>9 then u2+=g(3)+g(5)+g(6) : goto L1690
					if j=3 and extra(12)<>9 then u2+=g(3): goto L1690
					if j=5 and extra(12)<>9 then u2+=g(5): goto L1690
					if j=6 and extra(12)<>9 then u2+=g(6): goto L1690
				else if env$('client')='Franklinton' then 
					if j=5 then u2=0 : goto L1690
					if j=6 then u2+=g(6) : goto L1690
					if j=7 then u2+=g(1) : goto L1690
					if j=9 then u2+=g(4) : goto L1690
				else if env$('client')='Bethany' then 
					if j=7 then u2+=g(3) : goto L1690 ! bethany
					if j=9 then u2+=g(4) : goto L1690 ! gas taxable
					goto L1680 ! bethany & franklinton
				end if 
				if uprc$(tax_code$(j3))='Y' then u2=u2+g(j3) ! if service is taxable, add to total taxable dollars
				L1680: ! 
			next j3
		! /r
		L1690: ! 
		if g(j)><0 and x2=0 then x2=200: codemis=1
		if x2>200 then x2=200
		if (env$('client')='Billings' or env$('client')='Diamond') and j>5 and j<9 then goto L1720
			if x2<>0 then 
				t1(j,x2,1)+=1
				t1(j,x2,2)+=g(j)
				t1(j,x2,3)+=u2 : u2=0
			end if 
		L1720: ! 
	next j
return  ! /r
PrTotalsByCode: ! r: pr TOTALS BY CODE
	for st_item=1 to 10
		st$(st_item)=service$(st_item)
	next st_item
	pr #255: ''
	pr #255: '{\ul Service             }  {\ul Code}  {\ul Description                             }  {\ul Billed}  {\ul     Amount}  {\ul     Tax Base}  {\ul          Usage}'
	for j1=1 to 9
		for j2=1 to 200
			if t1(j1,j2,1) or t1(j1,j2,3) then 
				dim de$*40
				de$=''
				if j2<=99 then
					de$=fn_rateName$(st$(j1)(1:2)&cnvrt$('N 2',j2))
				end if
				if j2>99 or j1>4 or (env$('client')='Carrizo' and j1=3) then 
					goto PrCol2
					PrCol2: ! 
					pr #255,using Fpc2: st$(j1)(1:13),j2,de$,t1(j1,j2,1),t1(j1,j2,2),t1(j1,j2,3)
					Fpc2: form pos 1,c 21,n 4,x 3,c 40,n 8,n 12.2,pic(---,---,---.--)
				else
					PrCol1: !
					pr #255,using Fpc1: serviceName$(j1)(1:13),j2,de$,t1(j1,j2,1),t1(j1,j2,2),t1(j1,j2,3)
					Fpc1: form pos 1,c 21,n 4,x 3,c 40,n 8,n 12.2,x 14,pic(----,---,---,---)
				end if
			end if
		next j2
	next j1
	for j=1 to 10
		if t1(j,200,1)>0 or t1(j1,200,2)>0 then 
			pr #255: '   * All charges billed without a service code are summarized as Code 200'
			goto L2010
		end if 
	next j
	L2010: ! 
	if totalBillsToBePrinted then
	pr #255: ''
	pr #255: '    Count of Bills To Be Printed: '&str$(totalBillsToBePrinted)
	end if
return  ! /r
def fn_rateName$*40(rateCode$; ___,return$*40)
	return$=''
	read #hRate,using 'Form POS 5,C 40',key=rateCode$,release: return$ nokey ignore
	fn_rateName$=return$
fnend
def fn_readFromHistory(; ___,returnN)
	mat g=(0) : mat d=(0) : bal=0
	restore #h_trans,key>=z$&'         ': nokey PFH_XIT
	do
		read #h_trans,using 'form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1': p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof PFH_XIT
		if z$<>p$ then goto PFH_XIT
		if tcode=1 then 
			x=billing_date: x=fndate_mmddyy_to_ccyymmdd(x)
			if x=tdate then goto PFH_MATCH_FOUND ! FOUND MATCH
		end if
	loop
	PFH_MATCH_FOUND: !
		returnN=1
		d(1)=wr : d(3)=wu : d(5)=er
		d(7)=eu : d(9)=gr : d(11)=gu 
		bal=tbal
		for j=1 to 11 : g(j)=tg(j) : next j
	PFH_XIT: !
	fn_readFromHistory=returnN
fnend
include: Ertn
