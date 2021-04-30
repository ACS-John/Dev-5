! Replace S:\acsGL\VendorTransList
! Vendor(Payee)  File - Transaction List
 
	autoLibrary
	on error goto Ertn
 
	dim vn$*8,nam$*35,ad1$*20,ad2$*20,csz$*20,ss$*11,holdvn$*8,vcode$*8
	dim cnam$*40,dat$*20,adr(2),id1$*25
	dim rn$*12,de$*30,adr(2),tvn$*8,cap$*128
	dim scid$*79
	dim sd$(8),se$(8)*30,pl$(8,2)*35
 
	fnTop("S:\acsGL\VendorTransList",cap$="Payee Transaction List")
	fnconsole(off=0)
	fncno(cno,cnam$) : _
	fndat(dat$)
	open #payee=1: "Name=[Q]\GLmstr\PayMstr.h[cno],KFName=[Q]\GLmstr\PayIdx2.h[cno],Shr",internal,outIn,keyed
	open #payee2=11: "Name=[Q]\GLmstr\PayMstr.h[cno],KFName=[Q]\GLmstr\payidx2.h[cno],Shr",internal,outIn,keyed
	open #trans=2: "Name=[Q]\GLmstr\GLTR1099.h[cno],KFName=[Q]\GLmstr\gltridx1.h[cno],Shr",internal,outIn,keyed
	namtab=66-int(len(rtrm$(cnam$))/2)
	dattab=66-int(len(rtrm$(dat$))/2)
	if fnprocess=1 then goto L380
! 
MENU1: !
ASKDAT: !
	fnTos
	mylen=28 : mypos=mylen+2
	fnLbl(1,1,"Report Heading Date:",mylen,right=1)
	fnTxt(1,mypos,20) 
	resp$(1)=dat$
	fnLbl(3,1,"Transaction Starting Date:",mylen,right)
	fnTxt(3,mypos,8,0,left,'CCYYMMDD',0,'(Blank for All)  Normally you would enter the first day of the year, but you can analyze any time frame.') : _
	resp$(1)=str$(transactionstartingdate)
	fnLbl(4,1,"Transaction Ending Date:",mylen,right)
	fnTxt(4,mypos,8,0,left,'CCYYMMDD',0,'(Blank for All)  Normally you would enter the last day of the year, but you can analyze any time frame.') : _
	resp$(2)=str$(transactionendingdate)
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	dat$=resp$(1)
	dattab=66-int(len(rtrm$(dat$))/2)
	fndat(dat$,2)
L380: fnopenprn
	gosub L630
L400: read #1,using L420: vn$,nam$,ytdp eof L730
	if ytdp<-200000 then ytdp=0
L420: form pos 1,c 8,c 30,pos 129,pd 5.2
	fst=0
	ec$=""
	tot=0
	restore #trans,key>=vn$: nokey L400
L470: read #trans,using L520,release: trvn$,dt,am,rn$,de$ eof L400
	if trim$(trvn$) <> trim$(vn$) then nomore=1: goto L590
	if transactionstartingdate>0 and fndate_mmddyy_to_ccyymmdd(dt)<transactionstartingdate then goto L470
	if transactionendingdate>0 and fndate_mmddyy_to_ccyymmdd(dt)>transactionendingdate then goto L470
	if am<-2000000 then am=0
L520: form pos 1,c 8,n 6,pd 5.2,c 12,c 30
	tot=tot+am
	if fst=0 then pr #255,using L550: vn$,nam$,dt,rn$,de$,am,tot,ec$ pageoflow L580 else pr #255,using L560: dt,rn$,de$,am,tot,ec$ pageoflow L580
L550: form pos 1,c 8,pos 10,c 35,pos 46,pic(zz/zz/zz),pos 56,c 12,pos 70,c 30,pos 100,n 10.2,pos 115,n 10.2,pos 127,c 5
L560: form pos 46,pic(zz/zz/zz),pos 56,c 12,pos 69,c 30,pos 100,n 10.2,pos 115,n 10.2,pos 127,c 5
	goto L590
L580: gosub L620
L590: fst=1
	if nomore=1 then pr #255,using "form pos 50,c 20,n 10.2": "Total Transactions",tot: nomore=0 else goto L470
	goto L400
L620: pr #255: newpage
L630: p2=p2+1
	pr #255,using L650: date$('mm/dd/yy'),cnam$,time$,"Vendor Transaction Listing"
L650: form skip 1,pos 1,c 8,pos 40,cc 40,skip 1,pos 1,c 8,pos 40,cc 40,skip 1
	p1=66-int((len(rtrm$(dat$))+6)/2)
	pr #255,using L680: rtrm$("As of "&dat$),"Page",p2
L680: form pos 40,cc 40,pos 110,c 4,n 5,skip 1
	pr #255,using "form pos 118,c 5": "Total"
	pr #255,using L710: "Vendor #","Vendor Name","Date","Reference #","Description","Amount","    Purchases"
L710: form pos 1,c 8,pos 10,c 11,pos 48,c 4,pos 56,c 11,pos 70,c 11,pos 104,c 6,pos 112,c 13,skip 2
return
L730: fncloseprn
Xit: fnXit
 
include: ertn
 
