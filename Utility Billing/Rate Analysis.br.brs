! formerly S:\acsUB\Analyze
! ______________________________________________________________________
	library 'S:\Core\Library': fntop,fnxit, fnerror,fnopenprn,fncloseprn,fnTos,fnget_services,fncombof,fnLbl,fnTxt,fnFra,fnAcs,fnLastBillingDate,fnCmdSet
	on error goto ERTN
! ______________________________________________________________________
	dim rate(11),d(12),a(10)
	dim rateToAnalyze$*55
	dim usagtot(18),ratetot(18),customer(18)
	dim resp$(40)*80
	dim svce$*11
! ______________________________________________________________________
	fntop(program$)
	fnLastBillingDate(bdate)
	dim srvnam$(10)*20,srv$(10)
	fnget_services(mat srvnam$,mat srv$)
goto SCR1
SCR1: ! r:
	fnTos(sn$="Anlyze-1") 
	rc=0 : mylen=20 : mypos=mylen+2
	fnLbl(1,1,"Last Billing Date:",mylen,1)
	fnTxt(1,mypos,8,0,0,"1") 
	resp$(rc+=1)=str$(bdate)
	fnLbl(2,1,"Rate for Analysis:",mylen,1)
	fncombof("nerd",2,mypos,55,"[Q]\UBmstr\ubData\RateMst.h[cno]",1,4,5,50,"[Q]\UBmstr\ubData\RateIdx1.h[cno]",1,usa) 
	usa+=1 
	resp$(rc+=1)="" ! just default to the first one
	fnCmdSet(2)
	fnAcs(sn$,0,mat resp$,ckey)
	if ckey=5 then goto XIT 
	bdate=val(resp$(1)) 
	rateToAnalyze$=trim$(resp$(2)) 
	svce$=resp$(2)(1:4) 
	cde=val(resp$(2)(3:4))
	for j=1 to udim(srv$)
		if uprc$(resp$(2)(1:2))=uprc$(srv$(j)) then svce=j
	next j
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
	fnAcs(sn$,0,mat resp$,ckey)
	if ckey=2 then 
		goto SCR1 
	else if ckey=5 then 
		goto XIT
	end if
	rate(1)=val(resp$(2)) ! minimum charge
	use_to(1)=val(resp$(3)) ! minimum usage
	use_from(2)=val(resp$(4))  
	use_from(3)=val(resp$(7))  
	use_from(4)=val(resp$(10))  
	use_from(5)=val(resp$(13))  
	use_from(6)=val(resp$(16))  
	use_from(7)=val(resp$(19))  
	use_from(8)=val(resp$(22))  
	use_from(9)=val(resp$(25))  
	use_from(10)=val(resp$(28)) 
	use_from(11)=val(resp$(31))
	rate(2)=val(resp$(6))  
	rate(3)=val(resp$(9))  
	rate(4)=val(resp$(12)) 
	rate(5)=val(resp$(15)) 
	rate(6)=val(resp$(18)) 
	rate(7)=val(resp$(21)) 
	rate(8)=val(resp$(24)) 
	rate(9)=val(resp$(27)) 
	rate(10)=val(resp$(30))
	rate(11)=val(resp$(33))
	use_to(2)=val(resp$(5))  
	use_to(3)=val(resp$(8))  
	use_to(4)=val(resp$(11)) 
	use_to(5)=val(resp$(14))  
	use_to(6)=val(resp$(17))  
	use_to(7)=val(resp$(20))  
	use_to(8)=val(resp$(23))  
	use_to(9)=val(resp$(26))  
	use_to(10)=val(resp$(29)) 
	use_to(11)=val(resp$(32))
	goto Initialize ! /r
Initialize: ! r: Initialize
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed 
	fnopenprn
	mat customer=(0) 
	numbcust=0   
	totdol=0     
	mat ratetot=(0)  
	mat usagtot=(0)  
	ds9=0
goto READ_CUSTOMER ! /r
READ_CUSTOMER: ! r:
	read #1,using L590: mat a,mat d,f eof L840
	L590: form pos 143,7*pd 2,pos 1806,3*n 2,pos 217,12*pd 5,pos 296,pd 4
	if f<>bdate then goto READ_CUSTOMER
	if a(svce)=0 then goto READ_CUSTOMER
	s9=3
	if svce<3 then goto L670
	s9+=4
	if svce=3 then goto L670
	s9+=4
	L670: !
	if a(svce)=cde then
		if d(s9)<=0 then goto READ_CUSTOMER
		usagtot(1)=usagtot(1)+min(d(s9),use_to(1)) ! add usage for minimun usages
		ratetot(1)=ratetot(1)+rate(1) ! add minimum charges
		if d(s9)<=use_to(1) then  ! add mimimum customer user to first array item
			customer(1)=customer(1)+1 
		else
			for k7=2 to 11
				if use_from(k7)<>0 then 
					if d(s9)>=use_from(k7) and d(s9)<=use_to(k7) then  ! accumulate usage when usage stops between breaks
						usagtot(k7)=usagtot(k7)+max(0,(min(d(s9)-use_from(k7)+1,use_to(k7)-use_from(k7)))) 
					else if d(s9)>use_to(k7) then  ! accumulate                 useage between breaks
						usagtot(k7)=usagtot(k7)+use_to(k7)-use_from(k7)+1
					end if
					ds9=ds9+usagtot(k7)
				end if
			next k7
			ds9+=usagtot(1)
			customer(k7-1)+=1 ! add customers by rate break
		end if
		numbcust+=1
	end if
goto READ_CUSTOMER ! /r
L840: ! r:
	for k5=2 to 11 
		ratetot(k5)=usagtot(k5)*rate(k5) 
		totdol=totdol+ratetot(k5) 
	next k5
	totdol=totdol+ratetot(1) ! add minimum bills to total dollars
	if cde=0 then goto L960
	pr #255: "\qc {\b Utility Billing  - Rate Analysis}" 
	pr #255: "Analysis for Rate Code: "&rateToAnalyze$ 
	pr #255: "\ql "
	pr #255: "{\ul    Usage From} {\ul      Rate} {\ul Dollars Gnrtd.} {\ul Num.Customers}"
	for k1=1 to 11
		if use_from(k1)=0 and rate(k1)=0 then goto L930
		pr #255,using L920: use_from(k1),rate(k1),ratetot(k1),customer(k1)
		L920: form pos 1,pic(z,zzz,zzz,zz#),x 1,pic(zz#.#####),x 1,pic(zzz,zzz,zzz.##),x 1,pic(zzzzzzzzzzzzz)
		L930: !
	next k1
	pr #255: "\qc {\ul             Totals            }"
	pr #255: "\ql "
	L960: !
	pr #255: "              Total Customers: "&cnvrt$("PIC(ZZZ,ZZZ,ZZZ)",numbcust)
	pr #255: "                Total Dollars: "&cnvrt$("PIC(zzZZ,ZZZ.ZZ)",totdol)
goto DONE ! /r
DONE: ! r:
	close #1: ioerr ignore
	fncloseprn
goto SCR1 ! /r
XIT: fnxit
include: ertn
