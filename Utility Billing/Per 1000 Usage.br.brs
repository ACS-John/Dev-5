! formerly S:\acsUB\Per1000
! r: setup
	library 'S:\Core\Library': fntop,fnxit, fnAcs,fnLbl,fnTxt,fnTos,fnerror,fnopenprn,fncloseprn,fnLastBillingDate,fncomboa,fnCmdSet
	library 'S:\Core\Library': fnget_services,fncreg_read,fncreg_write
	library 'S:\Core\Library': fngethandle
	on error goto ERTN
	! ______________________________________________________________________
	dim z$*10,e$(4)*30,g(12)
	dim a(7),d(15)
	dim range(16),excess(2999,2),cust(16),over(160)
	dim resp$(20)*40,text$*40
	dim serviceName$(10)*20
	! ______________________________________________________________________
	fntop(program$)
	fnLastBillingDate(d1)
	!
	fnget_services(mat serviceName$)
	mat opt$(3)
	opt$(1)="Water"
	opt$(2)="Gas"
	if trim$(serviceName$(3))="Lawn Meter" then
		stfn$="wgl"
		opt$(3)="Lawn Meter"
	else
		stfn$="wge"
		opt$(3)="Electric"
	end if
! /r
! gosub AskUsageRanges
! AskUsageRanges: ! r:
	! r: read in prior setting or use defaults
	fncreg_read('Per 1000 Usage - Rate Code ',serviceSelected$, 'W')
	fncreg_read('Per 1000 Usage - Service for Analysis ',rateSelected$, '0') : rateSelected=val(rateSelected$)
	for rangeItem=1 to 16
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
	mylen=22
	mypos=mylen+2
	respc=0
	fnLbl(2,1,"Billing Date:" ,mylen,1) ! fnLbl(2,36,"(most recent billing date only)") ! ,31,0)
	fnTxt(2,mypos,8,0,1,"1")
	resp$(respc+=1)=str$(d1)
	text$="Service for Analysis:"
	fnLbl(3,1,text$,mylen,1)
	fncomboa(stfn$,3,mypos,mat opt$)
	resp$(respc+=1)=opt$(1)
	fnLbl(4,1,"Rate Code:",mylen,1)
	fnTxt(4,mypos,2,0,1,"30")
	resp$(respc+=1)="0"
	text$="Usage Break Points:"
	fnLbl(6,1,text$,mylen,1)
	for a = 1 to 16
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
	fnAcs(sn$,win,mat resp$,ck)
	! /r
	if ck<>5 then
		! r: move into local variables   and   save entries for next time
		d1=val(resp$(1))
		serviceSelected$=resp$(2)(1:1)
		rateSelected=val(resp$(3))
		for a = 1 to 16
			range(a) = val(resp$(a+3))
		next a
		! no longer necessary   if serviceSelected$<>"W" and serviceSelected$<>"E" and serviceSelected$<>"G" and serviceSelected$<>"L" then
		! no longer necessary   	goto MENU1
		! no longer necessary   end if
		fncreg_write('Per 1000 Usage - Rate Code ',serviceSelected$)
		fncreg_write('Per 1000 Usage - Service for Analysis ',str$(rateSelected))
		for rangeItem=1 to 16
			fncreg_write('Per 1000 Usage - Range '&str$(rangeItem),str$(range(rangeItem)))
		nex rangeItem
		if serviceSelected$="W" then
			rtype$="Water"
		else if serviceSelected$="E" then
			rtype$="Electric"
		else if serviceSelected$="L" then
			rtype$="Lawn Meter"
		else if serviceSelected$="G" then
			rtype$="Gas"
		else
			goto MENU1
		end if
		! /r
	end if
! return  ! /r AskUsageRanges


if ck=5 then goto XIT
open #customer:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed
hTran=fn_open('UB Transaction',mat tran$,mat tranN,mat form$,1)
! on fkey 99 goto DONE
! on fkey 5 goto DONE
on pageoflow goto PGOF
fnopenprn
goto ReadCustomer
ReadCustomer: ! r:
	read #customer,using F_CUSTOMER: z$,mat e$,mat a,mat d,bal,f,mat g eof ENDUBM
F_CUSTOMER: form pos 1,c 10,4*c 30,pos 143,7*pd 2,pos 217,15*pd 5,pos 292,pd 4.2,pd 4,12*pd 4.2
	if serviceSelected$="W" and rateSelected<>a(1) and rateSelected<>0 then
		goto ReadCustomer
	else if serviceSelected$="E" and rateSelected<>a(3) and rateSelected<>0 then
		goto ReadCustomer
	else if serviceSelected$="L" and rateSelected<>a(3) and rateSelected<>0 then
		goto ReadCustomer
	else if serviceSelected$="G" and rateSelected<>a(4) and rateSelected<>0 then
		goto ReadCustomer
	else if f><d1 then
		goto TryTransaction ! will either return to TryTransactionSuccess (just below) or ReadCustomer
		TryTransactionSuccess: !
	end if
	gosub USAGE_CHART_ACCUMULATE
	if serviceSelected$="W" and d(3)>range(16) then
		ov=min(ov+1,160)
		over(ov)=d(3)
	else if serviceSelected$="E" or serviceSelected$="L" and d(7)>range(16) then
		ov=min(ov+1,160)
		over(ov)=d(7)
	else if serviceSelected$="G" and d(11)>range(16) then
		ov=min(ov+1,160)
		over(ov)=d(11)
	end if
goto ReadCustomer ! /r
TryTransaction: ! r:

		read #hTran,using form$(hTran),search=>z$&str$(f)&'1': mat tran$,mat tranN NoKey TryTransactionFail
		!d(3): s01UsageCur - Service 1 (Water) – Usage – Current	    ,PD 5,SPos= 227,EPos=231
		!d(7): s03UsageCur - Service 3 (Electric) – Usage – Current  ,PD 5,SPos= 247,EPos=251
		!d(11):s04usageCur - Service 4 (Gas) – Usage – Current       ,PD 5,SPos= 267,EPos=271
		if z$=tran$(trans_acct) then 
			d(3) =tranN(trans_s1use)
			d(7) =tranN(trans_s3use)
			d(11)=tranN(trans_s4use)
			got TryTransactionSuccess
		en if
TryTransactionFail: !
goto ReadCustomer
! /r
ENDUBM: ! r:
	close #customer:
	gosub USAGE_CHART
	gosub OVER_LIST
goto DONE ! /r
DONE: ! r:
	close #customer: ioerr ignore
	fncloseprn
goto XIT ! /r
XIT: fnxit
IGNORE: continue
USAGE_CHART_ACCUMULATE: ! r:
	for j=1 to 15

		if serviceSelected$="W" then
			appropriateD=d(3)
		else if serviceSelected$="E" or serviceSelected$="L" then
			appropriateD=d(7)
		else if serviceSelected$="G" then
			appropriateD=d(11)
		end if

		if appropriateD=>range(j) and appropriateD<range(j+1) then
			if serviceSelected$="W" or serviceSelected$="E" or serviceSelected$="L" or serviceSelected$="G" then
				cust(j)=cust(j)+1
				goto L860
			end if
		end if
	next j
	cust(16)+=1
	x+=1
	excess(x,1)=val(z$)
	if serviceSelected$="W" or serviceSelected$="E" or serviceSelected$="L" or serviceSelected$="G" then
		excess(x,2)=appropriateD
	end if
	L860: !
return ! /r
USAGE_CHART: ! r:
! pr #255: "{\ul                                                                                }"
	pr #255: "\qc  {\f181 \fs20 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs24 \b "&env$('program_caption')&"}"
	pr #255: "\qc  {\f181 \fs16 \b "&date$("Month DD, CCYY")&"}"
	pr #255: "\qc  For the Billing Date of "&cnvrt$("PIC(##/##/##)",d1)
	if rateSelected<>0 then
		pr #255: "\qc For "&rtype$&" Rate Code "&str$(rateSelected)
	else
		pr #255: "\qc For "&rtype$
	end if
	pr #255: "\ql "
	pr #255,using "Form POS 7,C 18,C 30": " Usage In","  Total"
	pr #255,using "Form POS 7,C 18,C 30": " Gallons ","Customers"
	for j=1 to 16
		if j=1 then
			pr #255,using L990: "Under "&ltrm$(cnvrt$("PIC(ZZZZ,ZZ#)",range(2))),cust(j)
			L990: form pos 1,cr 19,x 1,pic(zz,zzz,zz#)
		else if range(j)>10 and j<>16 then
			pr #255,using L1000: cnvrt$("PIC(ZZZZ,ZZ#)",range(j))&" - "&ltrm$(cnvrt$("PIC(ZZZZ,ZZ#)",range(j+1)-1)),cust(j)
			L1000: form pos 1,cr 19,x 1,pic(zz,zzz,zz#)
		else if j=16 then
			pr #255,using L1000: cnvrt$("PIC(ZZZZ,ZZ#)",range(j))&" or more",cust(j)
		else
			pr #255,using L1010: range(j),cust(j)
			L1010: form pos 7,pic(zzzz,zz#),x 6,pic(zz,zzz,zz#)
		end if
	next j
	pr #255: "{\ul                                                                                }"
return ! /r
OVER_LIST: ! r:
	if over(1)<>0 then
		pr #255: "Actual usages for customers over "&ltrm$(cnvrt$("PIC(ZZZZ,ZZ#)",range(16)))
	end if
	for j=1 to 160 step 8
		if over(j)>0 then
			pr #255,using "form pos 1,Nz 9,x 1,Nz 9,x 1,Nz 9,x 1,Nz 9,x 1,Nz 9,x 1,Nz 9,x 1,Nz 9,x 1,Nz 9": over(j),over(j+1),over(j+2),over(j+3),over(j+4),over(j+5),over(j+6),over(j+7)
		else
			goto L1110
		end if
	next j
	L1110: !
	if over(1)<>0 then
		pr #255: "{\ul                                                                                }"
	end if
return ! /r
PGOF: pr #255: newpage : continue
include:ertn
include:fn_open