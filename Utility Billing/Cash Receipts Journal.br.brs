autoLibrary
fnTop(program$)
on error goto Ertn

dim ro(20,4)
dim tg(11)
dim resp$(7)*40
dim ml$(3)*90

enableHeader=1 ! <--  disabling this is really a developer only option.
dim serviceName$(10)*20,srv$(10)
fnGetServices(mat serviceName$,mat srv$)

! r: Screen1
	ld1=val(date$(days(date$('ccyymm')&'01','ccyymmdd')-1,'ccyymm')&'01') ! low (beginning of last month)
	hd1=date(days(date$('ccyymm')&'01','ccyymmdd')-1,'ccyymmdd') ! high (end of last month)
	fnPcReg_Read('include details'      	,enableDetails$    	, 'True',1)
	fnPcReg_Read('include Route Totals'	,enableRouteTotals$	, 'False',1)
	fnPcReg_Read('include Rate Totals' 	,enableRateTotals$ 	, 'False',1)
	fnPcReg_Read('Collections Only'     	,collectionsOnly$  	, 'False',1)

	fnTos
	mylen=33 : mypos=mylen+2 : lc=rc=0
	lc+=1
	fnLbl(lc+=1,1,'Starting Date (blank for all):',mylen,1)
	fnTxt(lc,mypos,10,0,1,'3',0,'First day of the period to be printed. (ccyymmdd format)')
	resp$(rc_ld1=rc+=1)=str$(ld1)
	fnLbl(lc+=1,1,'Ending Date (blank for all):',mylen,1)
	fnTxt(lc,mypos,10,0,1,'3',0,'Last day of the period to be printed. (ccyymmdd format)')
	resp$(rc_hd1=rc+=1)=str$(hd1)
	fnChk(lc+=1,mypos,'Include Details:',1)
	resp$(rc_indludeDetails=rc+=1)=enableDetails$
	fnChk(lc+=1,mypos,'Show Totals by Route:',1)
	resp$(rc_TotalByRoute=rc+=1)=enableRouteTotals$
	fnChk(lc+=1,mypos,'Show Totals by Rate:',1)
	resp$(rc_TotalByRate=rc+=1)=enableRateTotals$
	fnChk(lc+=1,mypos,'Collections Only:',1)
	resp$(rc_collectionsOnly=rc+=1)=collectionsOnly$
	fnLbl(lc,40,'(filter Credit and Debit Memos)')
	fnCmdSet(3)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	ld1=val(resp$(rc_ld1))
	hd1=val(resp$(rc_hd1))
	enableDetails$=resp$(rc_indludeDetails)
	if enableDetails$='True' then enableDetails=1 else enableDetails=0
	enableTotals=1 ! traditional totals are always included
	enableRouteTotals$=resp$(rc_TotalByRoute)
	if enableRouteTotals$='True' then enableRouteTotals=1 else enableRouteTotals=0
	enableRateTotals$=resp$(rc_TotalByRate)
	if enableRateTotals$='True' then enableRateTotals=1 else enableRateTotals=0
	collectionsOnly$=resp$(rc_TotalByRate)
	if collectionsOnly$='True' then collectionsOnly=1 else collectionsOnly=0

	fnPcReg_write('include details'      	,enableDetails$)
	fnPcReg_write('include Route Totals'	,enableRouteTotals$)
	fnPcReg_write('include Rate Totals' 	,enableRateTotals$)


! /r
! r: Main Loop
dim c$(0)*200
dim cN(0)
hCustomer=fn_openFio('UB Customer',mat c$, mat cN, 1) ! open #hCustomer=fnH: 'Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr',i,i,k
open #hTrans:=fnH: 'Name=[Q]\UBmstr\UBTransVB.h[cno],KFName=[Q]\UBmstr\UBTrIndx.h[cno],Shr',i,i,k
fnopenprn
gosub PrHeader
r01count=r02count=r03count=r04count=r05count=r06count=r07count=r08count=r09count=r10count=0
do
	! read #hCustomer,using 'Form POS 1,C 10,POS 41,C 28,pos 1741,n 2',release: c$(c_account),c$(c_name),cN(c_route) eof PrTotals
	read #hCustomer,using form$(hCustomer),release: mat c$,mat cN eof PrTotals
	fnApplyDefaultRatesFio(mat cN)
	restore #hTrans,key>=c$(c_account)&'         ': nokey NextCustomer
	do
		! read #hTrans,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': p$,tDate,tCode,tAmount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof NextCustomer
		read #hTrans,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2': p$,tDate,tCode,tAmount,mat tg eof NextCustomer
		if p$<>c$(c_account) then goto NextCustomer
		if ld1<>0 and tDate<ld1 then goto NextTrans
		if hd1<>0 and tDate>hd1 then goto NextTrans

		if (collectionsOnly and tCode=3) or (tCode=>3 and tCode<=5) and tAmount then ! don't pr charges or penalties
			if enableTotals then ! r: accumulate mat ro
				if tCode=3 then ti2=1 ! REG.COLLECTION
				if tCode=4 then ti2=2 ! CREDIT MEMO
				if tCode=5 then ti2=3 ! DEBIT MEMO
				if tCode=5 then
					ro(1,1)-=tAmount
				else
					ro(1,1)+=tAmount
				end if
				ro(1,ti2+1)+=tAmount
				x=0
				for j=1 to 10
					if trim$(serviceName$(j))<>'' then
						alloc(x+=1)=tg(j)
						if tCode=5 then
							ro(x+3,1)-=tg(j)
						else
							ro(x+3,1)+=tg(j)
						end if
						ro(x+3,ti2+1)+=tg(j)
					end if
				next j
			end if ! /r

			if enableDetails then ! r: print the detail line
				dim cd$*4
				cd$=''
				if tCode=4 then
					cd$='CM'
				else if tCode=5 then
					cd$='DM'
				else
				end if
				pr #255,using 'Form pos 1,c 10,n 10.2,c 4,pic(zzzz/zz/zz),sz1*n 9.2,x 3,c 30': c$(c_account),tAmount,cd$,tDate,mat alloc,c$(c_name)(1:25) pageoflow PgOf
			end if ! /r

			if sum(alloc)<>tAmount then ! r: test transaction allocstions add up to total
				mat ml$(3)
				ml$(1)='The breakdown on a collection transaction dated '&str$(tDate)& ' for customer '&c$(c_account)
				ml$(2)='does not balance.  Your totals will be off by '& trim$(cnvrt$('pic($$$,$$$.## cr)',tAmount-sum(alloc)))&'.'
				ml$(3)='(transaction record number: '&str$(rec(hTrans))&')'
				fnmsgbox(mat ml$,resp$,'',49)
				if resp$='Cancel' then goto Xit
			end if ! /r

			if enableRouteTotals then ! r:
				dim routeIndex
				if cN(c_route)<0 or cN(c_route)>200 then
					routeIndex=200
				else
					routeIndex=cN(c_route)
				end if
				dim routeTotal(200)
				routeTotal(routeIndex)+=tAmount
			end if ! /r

			if enableRateTotals then ! r:
				dim r01(0) ! rate total accumulator
				dim r02(0) ! rate total accumulator
				dim r03(0) ! rate total accumulator
				dim r04(0) ! rate total accumulator
				dim r05(0) ! rate total accumulator
				dim r06(0) ! rate total accumulator
				dim r07(0) ! rate total accumulator
				dim r08(0) ! rate total accumulator
				dim r09(0) ! rate total accumulator
				dim r10(0) ! rate total accumulator

				dim r01count(0)
				dim r02count(0)
				dim r03count(0)
				dim r04count(0)
				dim r05count(0)
				dim r06count(0)
				dim r07count(0)
				dim r08count(0)
				dim r09count(0)
				dim r10count(0)

				if trim$(serviceName$(1) )<>'' then fn_addToRateTotals(mat r01,cN(c_s01rate),tg(1) ,tCode,mat r01count)
				if trim$(serviceName$(2) )<>'' then fn_addToRateTotals(mat r02,cN(c_s02rate),tg(2) ,tCode,mat r02count)
				if trim$(serviceName$(3) )<>'' then fn_addToRateTotals(mat r03,cN(c_s03rate),tg(3) ,tCode,mat r03count)
				if trim$(serviceName$(4) )<>'' then fn_addToRateTotals(mat r04,cN(c_s04rate),tg(4) ,tCode,mat r04count)
				if trim$(serviceName$(5) )<>'' then fn_addToRateTotals(mat r05,cN(c_s05rate),tg(5) ,tCode,mat r05count)
				if trim$(serviceName$(6) )<>'' then fn_addToRateTotals(mat r06,cN(c_s06rate),tg(6) ,tCode,mat r06count)
				if trim$(serviceName$(7) )<>'' then fn_addToRateTotals(mat r07,cN(c_s07rate),tg(7) ,tCode,mat r07count)
				if trim$(serviceName$(8) )<>'' then fn_addToRateTotals(mat r08,cN(c_s08rate),tg(8) ,tCode,mat r08count)
				if trim$(serviceName$(9) )<>'' then fn_addToRateTotals(mat r09,cN(c_s09rate),tg(9) ,tCode,mat r09count)
				if trim$(serviceName$(10))<>'' then fn_addToRateTotals(mat r10,cN(c_s10rate),tg(10),tCode,mat r10count)

			end if ! /r

		end if
		NextTrans: !
	loop
	NextCustomer: !
loop
! /r
def fn_addToRateTotals(mat rX,rCode,Amt,tCode,mat count; rIndex)
	rIndex=rCode+1
	if udim(mat rX)<rIndex then 
		mat rX(rIndex)
		mat count(rIndex)
	end if

	if tCode=5 then
		rX(rIndex)-=Amt
	else
		rX(rIndex)+=Amt
	end if
	if Amt<>0 then
		count(rIndex)+=1
	end if
fnend
PrHeader: ! r:
	if ~setup_header then ! r: initialize sz1, hd1$, mat scr1$ and mat alloc
		setup_header=1
		dim sz1
		sz1=0
		dim hd1$*255
		hd1$='{\ul  Account  }  {\ul    Total}    {\ul    Date   }'
		for j=1 to 10
			x2=pos(trim$(serviceName$(j)),' ',1)
			if x2>0 then serviceName$(j)=serviceName$(j)(1:2)&'-'&serviceName$(j)(x2+1:len(serviceName$(j))) ! if service name two words long, use part of both
			if trim$(serviceName$(j))<>'' then
				scr1$(sz1+=1)=serviceName$(j)
				hd1$&='  {\ul '&lpad$(rtrm$(serviceName$(j)(1:7)),7)&'}'
			end if
		next j
		hd1$&='  {\ul Customer Name               }'
		dim scr1$(10)*30
		mat scr1$(sz1)
		dim alloc(10)
		mat alloc(sz1)
	end if ! /r
	if enableHeader then ! r:
		pr #255: '\qc  {\f181 \fs20 \b '&env$('cnam')&'}'
		pr #255: '\qc  {\f181 \fs28 \b '&env$('program_caption')&'}'
		! need date$,time$
		if ld1 and hd1 then
			pr #255: '\qc  {\f181 \fs18 \b From '&cnvrt$('pic(zzzz/zz/zz)',ld1)& '  To '&cnvrt$('pic(zzzz/zz/zz)',hd1)&'}'
		end if
		pr #255: ''
		if enableDetails then
			pr #255: '\ql '&hd1$
		end if
	end if ! /r
return  ! /r
PgOf: ! r:
	pr #255: newpage
	gosub PrHeader
continue  ! /r
PrTotals: ! r:
	if enableTotals then ! r:
		pr #255: ''
		pr #255: '    ************ Totals ************'
		pr #255: tab(34);'{\ul       Total}  {\ul    Reg.Col}  {\ul   Cr.Memos}  {\ul   Db.Memos}'
		for j=1 to sz1
			pr #255,using 'Form POS 4,C 30,N 11.2,3*N 12.2': scr1$(j),ro(j+3,1),ro(j+3,2),ro(j+3,3),ro(j+3,4) pageoflow PgOf
		next j
		pr #255: ''
		pr #255,using 'Form POS 4,C 30,N 11.2,3*N 12.2': 'Total      ',ro(1,1),ro(1,2),ro(1,3),ro(1,4)
	end if ! /r
	if enableRouteTotals then ! r:
		pr #255: ''
		pr #255: '    ************ Route Totals ************'
		for j=1 to udim(mat routeTotal)
		if routeTotal(j) then
				pr #255,using 'form pos 1,c 10,pic(zzz,zzz,zzz.##)': 'Route '&cnvrt$('pic(zzz)',j),routeTotal(j) pageoflow PgOf
			end if
		next j
	end if ! /r
	if enableRateTotals then ! r:
		fn_prRateTotal(mat r01,srv$( 1),serviceName$( 1),mat r01count)
		fn_prRateTotal(mat r02,srv$( 2),serviceName$( 2),mat r02count)
		fn_prRateTotal(mat r03,srv$( 3),serviceName$( 3),mat r03count)
		fn_prRateTotal(mat r04,srv$( 4),serviceName$( 4),mat r04count)
		fn_prRateTotal(mat r05,srv$( 5),serviceName$( 5),mat r05count)
		fn_prRateTotal(mat r06,srv$( 6),serviceName$( 6),mat r06count)
		fn_prRateTotal(mat r07,srv$( 7),serviceName$( 7),mat r07count)
		fn_prRateTotal(mat r08,srv$( 8),serviceName$( 8),mat r08count)
		fn_prRateTotal(mat r09,srv$( 9),serviceName$( 9),mat r09count)
		fn_prRateTotal(mat r10,srv$(10),serviceName$(10),mat r10count)
	end if ! /r
	fncloseprn
goto Xit ! /r
def fn_prRateTotal(mat r0,serviceId$,serviceName$*128,mat count;___,x)
	if sum(mat r0) then
		pr #255: ''
		pr #255: '    ************ Rate Totals for '&trim$(serviceName$)&' ************'
		for x=1 to udim(mat r0)
			if r0(x) then
				pr #255: serviceId$&' '&str$(x-1)&':   '&cnvrt$('pic(----,---,--#.##)',r0(x))&' from '&str$(count(x))&' customers'
			end if
		nex x
		pr #255: 'Total for '&trim$(serviceName$)&' was '&trim$(cnvrt$('pic(----,---,--#.##)',sum(mat r0)))&' from '&str$(sum(mat count))&' customers'
	end if
fnend
Xit: fnXit
include: fn_open
include: ertn
