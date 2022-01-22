! formerly S:\acsGL\VendorTransList
! Vendor(Payee)  File - Transaction List

autoLibrary
on error goto Ertn
fnTop(program$)

dim dat$*20
fndat(dat$)
open #hPayee=fnH: "Name=[Q]\GLmstr\PayMstr.h[cno],KFName=[Q]\GLmstr\PayIdx2.h[cno],Shr",i,outIn,k
open #hPayee2=fnH: "Name=[Q]\GLmstr\PayMstr.h[cno],KFName=[Q]\GLmstr\payidx2.h[cno],Shr",i,outIn,k
open #hTran=fnH: "Name=[Q]\GLmstr\GLTR1099.h[cno],KFName=[Q]\GLmstr\gltridx1.h[cno],Shr",i,outIn,k
if fnprocess=1 then goto GetStarted

ASKDAT: ! r:
	fnTos
	mylen=28 : mypos=mylen+2
	fnLbl(1,1,"Report Heading Date:",mylen,right=1)
	fnTxt(1,mypos,20) 
	resp$(1)=dat$
	fnLbl(3,1,"Transaction Starting Date:",mylen,right)
	fnTxt(3,mypos,8,0,left,'CCYYMMDD',0,'(Blank for All)  Normally you would enter the first day of the year, but you can analyze any time frame.')
	resp$(1)=str$(transactionstartingdate)
	fnLbl(4,1,"Transaction Ending Date:",mylen,right)
	fnTxt(4,mypos,8,0,left,'CCYYMMDD',0,'(Blank for All)  Normally you would enter the last day of the year, but you can analyze any time frame.')
	resp$(2)=str$(transactionendingdate)
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	dat$=resp$(1)
	fndat(dat$,2)
goto GetStarted ! /r

GetStarted: ! r: the main loop
	fnopenprn
	gosub PrHeader
ReadPayee: !
	dim vn$*8,nam$*35
	read #hPayee,using L420: vn$,nam$,ytdp eof Finis
	L420: form pos 1,c 8,c 30,pos 129,pd 5.2
	if ytdp<-200000 then ytdp=0
	fst=0
	ec$=""
	tot=0
	restore #hTran,key>=vn$: nokey ReadPayee
ReadTran: !
	dim rn$*12,de$*30
	read #hTran,using L520,release: trvn$,dt,am,rn$,de$ eof ReadPayee
	L520: form pos 1,c 8,n 6,pd 5.2,c 12,c 30
	if trim$(trvn$) <> trim$(vn$) then 
		nomore=1
	else
		if transactionstartingdate>0 and fndate_mmddyy_to_ccyymmdd(dt)<transactionstartingdate then goto ReadTran
		if transactionendingdate>0 and fndate_mmddyy_to_ccyymmdd(dt)>transactionendingdate then goto ReadTran
		if am<-2000000 then am=0
		tot+=am
		if fst=0 then 
			pr #255,using L550: vn$,nam$,dt,rn$,de$,am,tot,ec$ pageoflow PgOf 
			L550: form pos 1,c 8,pos 10,c 35,pos 46,pic(zz/zz/zz),pos 56,c 12,pos 70,c 30,pos 100,n 10.2,pos 115,n 10.2,pos 127,c 5
		else 
			pr #255,using L560: dt,rn$,de$,am,tot,ec$ pageoflow PgOf
			L560: form pos 46,pic(zz/zz/zz),pos 56,c 12,pos 69,c 30,pos 100,n 10.2,pos 115,n 10.2,pos 127,c 5
		end if
	end if
	fst=1
	if nomore=1 then pr #255,using "form pos 50,c 20,n 10.2": "Total Transactions",tot: nomore=0 else goto ReadTran
goto ReadPayee ! /r
PgOf: ! r:
	pr #255: newpage
	gosub PrHeader
continue ! /r
PrHeader: ! r:
	p2+=1
	pr #255,using L650: date$('mm/dd/yy'),env$('cnam'),time$,"Vendor Transaction Listing"
	L650: form skip 1,pos 1,c 8,pos 40,cc 40,skip 1,pos 1,c 8,pos 40,cc 40,skip 1
	p1=66-int((len(rtrm$(dat$))+6)/2)
	pr #255,using L680: rtrm$("As of "&dat$),"Page",p2
	L680: form pos 40,cc 40,pos 110,c 4,n 5,skip 1
	pr #255,using "form pos 118,c 5": "Total"
	pr #255,using L710: "Vendor #","Vendor Name","Date","Reference #","Description","Amount","    Purchases"
	L710: form pos 1,c 8,pos 10,c 11,pos 48,c 4,pos 56,c 11,pos 70,c 11,pos 104,c 6,pos 112,c 13,skip 2
return ! /r
Finis: !
fncloseprn
Xit: fnXit
 
include: ertn
 
