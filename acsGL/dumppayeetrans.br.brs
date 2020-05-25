! Replace S:\acsGL\DumpPayeeTrans
! Vendor(Payee)  Dump old Transactions
 
	autoLibrary
	on error goto Ertn
 
	dim vn$*8,nam$*35,ad1$*20,ad2$*20,csz$*20,ss$*11,holdvn$*8,vcode$*8
	dim cnam$*40,dat$*20,adr(2),id1$*25
	dim rn$*12,de$*30,adr(2),tvn$*8,cap$*128
	dim scid$*79
	dim sd$(8),se$(8)*30,pl$(8,2)*35
 
	fnTop("S:\acsGL\VendorTransList",cap$="Dump Old Payee Transactions")
	fncno(cno,cnam$) : _
	fndat(dat$)
	open #payee=1: "Name=[Q]\GLmstr\paymstr.h[cno],KFName=[Q]\GLmstr\PayIdx2.h[cno],Shr",internal,outIn,keyed
	open #payee2=11: "Name=[Q]\GLmstr\paymstr.h[cno],KFName=[Q]\GLmstr\payidx2.h[cno],Shr",internal,outIn,keyed
	open #trans=2: "Name=[Q]\GLmstr\GLTR1099.H[cno],KFName=[Q]\GLmstr\gltridx1.h[cno],Shr",internal,outIn,keyed
	namtab=66-int(len(rtrm$(cnam$))/2)
	dattab=66-int(len(rtrm$(dat$))/2)
! _________________________
MENU1: !
ASKDAT: !
	fnTos(sn$="DumpVendorTrans") : _
	mylen=35 : mypos=mylen+2
	fnLbl(1,1,"Oldest Transaction Date to Retain:",mylen,right)
	fnTxt(1,mypos,8,0,left,'CCYYMMDD',0,'All payee transactions older than the date you enter here will be removed.') : _
	resp$(1)=str$(oldestdate)
	fnLbl(1,50,"")
	fnCmdSet(2)
	fnAcs2(mat resp$,ckey)
	if ckey=5 then goto Xit
	oldestdate=val(resp$(1))
L300: read #trans,using L320: trvn$,dt,am,rn$,de$ eof Xit
	if oldestdate > fndate_mmddyy_to_ccyymmdd(dt) then goto DELETEIT else goto L300
L320: form pos 1,c 8,n 6,pd 5.2,c 12,c 30
	goto L350
DELETEIT: !
L350: delete #trans:
	goto L300
Xit: fnXit
 
include: Ertn
 
