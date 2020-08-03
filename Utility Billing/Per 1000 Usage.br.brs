! formerly S:\acsUB\Per1000
! r: setup
	autoLibrary
	on error goto Ertn
	!
	dim range(16),cust(16)
	dim resp$(20)*40
	dim serviceName$(10)*20
	!
	fnTop(program$)
	fnLastBillingDate(d1)
	!
	fnGetServices(mat serviceName$,mat serviceCode$)
	fnGetServiceCodesMetered(mat serviceCodeMetered$)
! /r
! r: Ask Usage Ranges
	! r: read in prior setting or use defaults
	fncreg_read('Per 1000 Usage - Rate Code ',serviceSelected$, 'W')
	fncreg_read('Per 1000 Usage - Service for Analysis ',rateSelected$, '0') : rateFilter=val(rateSelected$)
	for rangeItem=1 to udim(mat range)
		fncreg_read('Per 1000 Usage - Range '&str$(rangeItem),tmp$) : range(rangeItem)=val(tmp$)
	nex rangeItem
	if sum(mat range)=0 then
		range(1) =    0 : range(2) = 1000 : range(3) = 2000
		range(4) = 3000 : range(5) = 4000 : range(6) = 5000
		range(7) = 6000 : range(8) = 7000 : range(9) = 8000
		range(10)= 9000 : range(11)=10000 : range(12)=15000
		range(13)=20000 : range(14)=30000 : range(15)=40000
		range(16)=50000
	end if
	! /r
MENU1: ! r:
fnTos(sn$:="Per1000")
mylen=26
mypos=mylen+2
respc=0
fnLbl(2,1,"Billing Date:" ,mylen,1) ! fnLbl(2,36,"(most recent billing date only)") ! ,31,0)
fnTxt(2,mypos,8,0,1,"1")
resp$(respc+=1)=str$(d1)
fnLbl(3,1,"Service for Analysis:",mylen,1)
fncomboa('servicCodessMetered',3,mypos,mat serviceCodeMetered$)
resp$(respc+=1)=serviceCodeMetered$(1)
fnLbl(4,1,"Rate Code (blank for all):",mylen,1)
fnTxt(4,mypos,2,0,1,"30")
resp$(respc+=1)="0"
fnLbl(6,1,"Usage Break Points:",mylen,1)
for a = 1 to udim(mat range)
	resp$(respc+=1) = str$(range(a))
next a
mypos(1)=mylen+2 : mypos(2)=mypos(1)+9
mypos(3)=mypos(2)+9 : mypos(4)=mypos(3)+9
fnTxt(6,mypos(1),7) : fnTxt(6,mypos(2),7)
fnTxt(6,mypos(3),7) : fnTxt(6,mypos(4),7)
fnTxt(7,mypos(1),7) : fnTxt(7,mypos(2),7)
fnTxt(7,mypos(3),7) : fnTxt(7,mypos(4),7)
fnTxt(8,mypos(1),7) : fnTxt(8,mypos(2),7)
fnTxt(8,mypos(3),7) : fnTxt(8,mypos(4),7)
fnTxt(9,mypos(1),7) : fnTxt(9,mypos(2),7)
fnTxt(9,mypos(3),7) : fnTxt(9,mypos(4),7)
fnCmdSet(3)
fnAcs(mat resp$,ckey)
! /r
if ckey=5 then
	goto Xit
else
	! r: move into local variables   and   save entries for next time
	d1=val(resp$(1))
	serviceSelected$=resp$(2)(1:1)
	serviceWhich=srch(mat serviceCode$,resp$(2))
	rateFilter=val(resp$(3))
	for a=1 to udim(mat range)
		range(a)=val(resp$(a+3))
	next a
	if serviceWhich<=0 then goto MENU1
	fncreg_write('Per 1000 Usage - Rate Code ',serviceSelected$)
	fncreg_write('Per 1000 Usage - Service for Analysis ',str$(rateFilter))
	for rangeItem=1 to udim(mat range)
		fncreg_write('Per 1000 Usage - Range '&str$(rangeItem),str$(range(rangeItem)))
	nex rangeItem
	! /r
end if
! return  ! /r
 
! r: main loop
on pageoflow goto PGOF
fnopenprn
dayFilter=days(d1,'mmddyy')
dim over(0)
mat over(0)
hTran2=fn_open('UB Transaction',mat tran$,mat tranN,mat form$,1,2)
restore #hTran2,search=>date$(dayFilter,'ccyymmdd'): nokey finis
do
	read #hTran2,using form$(hTran2): mat tran$,mat tranN eof Finis
	dayRead=days(tranN(trans_tdate),'ccyymmdd')
	if dayRead=dayFilter then
		rateRead=val(fnCustomerData$(tran$(trans_acct),'service '&str$(serviceWhich)&'.rate code',1))
		if rateFilter=0 or rateFilter=rateRead then
			if serviceWhich=1 then
				usage=tranN(trans_s1use)
			else if serviceWhich=3 then
				usage=tranN(trans_s3use)
			else if serviceWhich=4 then
				usage=tranN(trans_s4use)
			else
				pr 'unhandeled serviceWhich=';serviceWhich
				pause
			end if
			fn_usageChartAccum(usage,mat range,mat cust,mat over)
		end if
	end if
loop while dayRead=dayFilter
goto Finis ! /r
Finis: ! r:
	fn_usageChartPrint(dayFilter,rateFilter,serviceWhich,mat range,mat cust)
	fn_overageListPrint(mat over,range(udim(mat range)))
	close #hTran2: ioerr ignore
	fncloseprn
goto Xit ! /r
Xit: fnXit
def fn_usageChartAccum(usage,mat range,mat cust,mat over)
	if usage=>range(udim(mat range)) then
		cust(udim(mat range))+=1
		fnAddOneN(mat over,usage)
	else
		for j=1 to (udim(mat range)-1)
			if usage=>range(j) and usage<range(j+1) then
				cust(j)+=1
			end if
		next j
	end if
fnend
def fn_usageChartPrint(dayFilter,rateFilter,serviceWhich,mat range,mat cust)
! pr #255: "{\ul                                                                                }"
	pr #255: "\qc  {\f181 \fs20 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs24 \b "&env$('program_caption')&"}"
	pr #255: "\qc  {\f181 \fs16 \b "&date$("Month DD, CCYY")&"}"
	pr #255: "\qc  For the Billing Date of "&date$(dayFilter,'mm/dd/cyy')
	if rateFilter<>0 then
		pr #255: "\qc For "&serviceName$(serviceWhich)&" Rate Code "&str$(rateFilter)
	else
		pr #255: "\qc For "&serviceName$(serviceWhich)
	end if
	pr #255: "\ql "
	pr #255,using "Form POS 7,C 18,C 30": " Usage In","  Total"
	pr #255,using "Form POS 7,C 18,C 30": " Gallons ","Customers"
	for j=1 to udim(mat range)
		if j=1 then
			pr #255,using L990: "Under "&ltrm$(cnvrt$("PIC(ZZZZ,ZZ#)",range(2))),cust(j)
			L990: form pos 1,cr 19,x 1,pic(zz,zzz,zz#)
		else if range(j)>10 and j<udim(mat range) then
			pr #255,using L1000: cnvrt$("PIC(ZZZZ,ZZ#)",range(j))&" - "&ltrm$(cnvrt$("PIC(ZZZZ,ZZ#)",range(j+1)-1)),cust(j)
			L1000: form pos 1,cr 19,x 1,pic(zz,zzz,zz#)
		else if j=udim(mat range) then
			pr #255,using L1000: cnvrt$("PIC(ZZZZ,ZZ#)",range(j))&" or more",cust(j)
		else
			pr #255,using L1010: range(j),cust(j)
			L1010: form pos 7,pic(zzzz,zz#),x 6,pic(zz,zzz,zz#)
		end if
	next j
	pr #255: "{\ul                                                                                }"
fnend
def fn_overageListPrint(mat over,highestRange)
	if udim(mat over)>0 then
		pr #255: "Actual usages for customers over "&ltrm$(cnvrt$("PIC(ZZZZ,ZZ#)",highestRange))
		mat over(int((udim(mat over)+7)/8)*8)
		for j=1 to int((udim(mat over)+7)/8)*8 step 8
			if over(j)>0 then
				pr #255,using "form pos 1,Nz 9,x 1,Nz 9,x 1,Nz 9,x 1,Nz 9,x 1,Nz 9,x 1,Nz 9,x 1,Nz 9,x 1,Nz 9": over(j),over(j+1),over(j+2),over(j+3),over(j+4),over(j+5),over(j+6),over(j+7)
			end if
		next j
		pr #255: "{\ul                                                                                }"
	end if
fnend
PGOF: pr #255: newpage : continue
include:ertn
include:fn_open
