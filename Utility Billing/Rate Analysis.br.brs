! formerly S:\acsUB\Analyze
! r: setup
	autoLibrary
	on error goto Ertn
 
	dim resp$(40)*80
 
	fnTop(program$)
	fnLastBillingDate(bdate) : billingDay=days(bdate,'mmddyy')
	dim srvnam$(10)*20,srv$(10)
	fnGetServices(mat srvnam$,mat srv$)
goto SCR1 ! /r
SCR1: ! r:
	fnTos(sn$="Anlyze-1")
	rc=0 : mylen=20 : mypos=mylen+2
	fnLbl(1,1,"Billing Date:",mylen,1)
	fnTxt(1,mypos,8,0,0,"1")
	resp$(rc+=1)=str$(bdate)
	fnLbl(2,1,"Rate for Analysis:",mylen,1)
	fncombof("nerd",2,mypos,55,"[Q]\UBmstr\ubData\RateMst.h[cno]",1,4,5,50,"[Q]\UBmstr\ubData\RateIdx1.h[cno]",1,usa)
	usa+=1
	resp$(rc+=1)="" ! just default to the first one
	fnCmdSet(2)
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	bdate=val(resp$(1)) : billingDay=days(bdate,'mmddyy')
	dim rateToAnalyze$*80
	rateToAnalyze$=trim$(resp$(2))
	dim svce$*11
	svce$=resp$(2)(1:4)
	cde=val(resp$(2)(3:4))
	svce=srch(mat srv$,svce$(1:2))
	if svce<>1 and svce<>3 and svce<>4 then
		dim ml$(0)*256
		fnaddone$(mat ml$,'Only the following services may be analyzed with '&env$('program_caption')&'.')
		if trim$(srvnam$(1))<>'' then let fnaddone$(mat ml$,tab$&trim$(srvnam$(1)))
		if trim$(srvnam$(3))<>'' then let fnaddone$(mat ml$,tab$&trim$(srvnam$(3)))
		if trim$(srvnam$(4))<>'' then let fnaddone$(mat ml$,tab$&trim$(srvnam$(4)))
		fnmsgbox(mat ml$)
		goto SCR1
	end if
goto SCR2 ! /r
SCR2: ! r: requires svce$, returns mat use_to, mat use_from, probably more
	dim rt$(32)
	dim use_to(11)
	dim use_from(11)
	open #20: "Name=[Q]\UBmstr\ubData\RateMst.h[cno],KFName=[Q]\UBmstr\ubData\RateIdx1.h[cno],Shr",internal,input,keyed
	read #20,using "Form POS 55,32*G 10",key=svce$: mat rt$ ioerr SCR1
	close #20:
	fnTos(sn$:="Anlyze-2b")
	rc=rtc=0 : mylen=20 : mypos=mylen+2
	fnLbl(1,1,"Analysis Based On:",mylen,1)
	fnTxt(1,mypos,55,0,0,"",1)
	resp$(rc+=1)=rateToAnalyze$
	fnLbl(2,1,"Minimum Charge:",mylen,1)
	fnTxt(2,mypos,9,0,1,"10")
	resp$(rc+=1)=rt$(rtc+=1)
	fnLbl(3,1,"Minimum Usage:",mylen,1)
	fnTxt(3,mypos,9,0,1,"30")
	resp$(rc+=1)=rt$(rtc+=1)
	fnFra(4,1,12,45,"Rate Breakdown")
	fnLbl(1,5,"Usage",10,2,0,1)
	fnLbl(1,17,"Usage",10,2,0,1)
	fnLbl(1,32,"Charge",15,2,0,1)
	fnLbl(2,5,"From",10,2,0,1)
	fnLbl(2,17,"To",10,2,0,1)
	fnLbl(2,32,"Per Unit",15,2,0,1)
	for j=1 to 10
		txt$=str$(j)&"." : fnLbl(j+2,1,txt$,3,1,0,1)
		fnTxt(j+2,05,10,0,1,"30",0,mt$,1)
		resp$(rc+=1)=rt$(rtc+=1)
		fnTxt(j+2,17,10,0,1,"30",0,mt$,1)
		resp$(rc+=1)=rt$(rtc+=1)
		fnTxt(j+2,34,10,0,1,"46",0,mt$,1)
		resp$(rc+=1)=rt$(rtc+=1)
	next j
	fnCmdSet(8)
	fnAcs(mat resp$,ckey)
	if ckey=2 then
		goto SCR1
	else if ckey=5 then
		goto Xit
	end if
	dim rate(11)
 
	use_from( 1)=0              : use_to( 1)=val(resp$(3))  : rate( 1)=val(resp$( 2))
	use_from( 2)=val(resp$( 4)) : use_to( 2)=val(resp$(5))  : rate( 2)=val(resp$( 6))
	use_from( 3)=val(resp$( 7)) : use_to( 3)=val(resp$(8))  : rate( 3)=val(resp$( 9))
	use_from( 4)=val(resp$(10)) : use_to( 4)=val(resp$(11)) : rate( 4)=val(resp$(12))
	use_from( 5)=val(resp$(13)) : use_to( 5)=val(resp$(14)) : rate( 5)=val(resp$(15))
	use_from( 6)=val(resp$(16)) : use_to( 6)=val(resp$(17)) : rate( 6)=val(resp$(18))
	use_from( 7)=val(resp$(19)) : use_to( 7)=val(resp$(20)) : rate( 7)=val(resp$(21))
	use_from( 8)=val(resp$(22)) : use_to( 8)=val(resp$(23)) : rate( 8)=val(resp$(24))
	use_from( 9)=val(resp$(25)) : use_to( 9)=val(resp$(26)) : rate( 9)=val(resp$(27))
	use_from(10)=val(resp$(28)) : use_to(10)=val(resp$(29)) : rate(10)=val(resp$(30))
	use_from(11)=val(resp$(31)) : use_to(11)=val(resp$(32)) : rate(11)=val(resp$(33))
goto Initialize ! /r
Initialize: ! r: Initialize
	fnopenprn
	dim tr$(0)*256
	dim trN(0)
	hTrans=fn_open('UB Transaction',mat tr$,mat trN,mat form$, 1,2)  ! input only, index 2 (tdate/acct)
	dim customer(18)
	mat customer=(0)
	dim rateTot(18)
	mat rateTot=(0)
	dim usagTot(18)
	mat usagTot=(0)
goto MainLoop ! /r
MainLoop: ! r:
	restore #hTrans,search=>date$(billingDay,'ccyymmdd'):
	do
		read #hTrans,using form$(hTrans): mat tr$,mat trN eof EoMainLoop
		customer_account$=tr$(trans_acct)
		transDay=days(trN(trans_tdate),'ccyymmdd')
		if trN(trans_tcode)=1 and transDay=billingDay then
			customer_serviceX_rateCode=val(fnCustomerData$(customer_account$,'service '&str$(svce)&'.rate code',1))
			if svce=1 then
				usageX=trN(trans_s1use)
			else if svce=3 then
				usageX=trN(trans_s3use)
			else if svce=4 then
				usageX=trN(trans_s4use)
			else
				pr 'unexepected svce value'
				pause
			end if
			if customer_serviceX_rateCode=cde and usageX>0 then
				usagTot(1)+=min(usageX,use_to(1)) ! add usage for minimun usages
				rateTot(1)+=rate(1) ! add minimum charges
				for k7=2 to 11
					if use_from(k7) then
						if usageX>=use_from(k7) and usageX<=use_to(k7) then  ! accumulate usage when usage stops between breaks
							usagTot(k7)+=max(0,(min(usageX-use_from(k7)+1,use_to(k7)-use_from(k7))))
						else if usageX>use_to(k7) then  ! accumulate                 useage between breaks
							usagTot(k7)+=(use_to(k7)-use_from(k7)+1)
						end if
					end if
				next k7
				customer(fn_whichTier(usageX,mat use_from,mat use_to))+=1
			end if
		end if
	loop while billingDay=transDay
	! /r
	EoMainLoop: ! r:
	for k5=2 to 11
		rateTot(k5)=usagTot(k5)*rate(k5)
	next k5
	if cde then
		pr #255: "\qc {\b Utility Billing  - "&env$('program_caption')&"}"
		pr #255: "Analysis for Rate Code: "&rateToAnalyze$
		pr #255: "Billing Date: "&date$(billingDay,'mm/dd/ccyy')
		pr #255: "\ql "
		pr #255: "{\ul    Usage From} {\ul      Rate} {\ul Dollars Gnrtd.} {\ul Num.Customers}"
		for k1=1 to 11
			if use_from(k1) or rate(k1) or customer(k1) then
				pr #255,using FprUrrc: use_from(k1),rate(k1),rateTot(k1),customer(k1)
				FprUrrc: form pos 1,pic(z,zzz,zzz,zz#),x 1,pic(zz#.#####),x 1,pic(zzz,zzz,zzz.##),x 1,pic(zzzzzzzzzzzzz)
			end if
		next k1
		pr #255: "\qc {\ul             Totals            }"
		pr #255: "\ql "
	end if
	pr #255: "              Total Customers: "&cnvrt$("PIC(ZZZ,ZZZ,ZZZ)",sum(mat customer))
	pr #255: "                Total Dollars: "&cnvrt$("PIC(zzZZ,ZZZ.ZZ)",sum(mat rateTot))
goto DONE ! /r
DONE: ! r:
	fnclosefile(hTrans,'UB Transaction')
	fnCustomerData$('','',0)
	fncloseprn
goto Xit ! /r
def fn_whichTier(value,mat fromN,mat toN; ___,returnN,x)
	for x=1 to udim(mat fromN)
		if value=>fromN(x) and value<=toN(x) then
			returnN=x
			goto WhichTierFinis
		end if
	nex x
	pr 'could not find a tier for '&str$(value) : pause
	WhichTierFinis: !
	fn_whichTier=returnN
fnend
Xit: fnXit
include: fn_open
include: ertn
