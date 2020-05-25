! Replace S:\acsGL\VendorReas
! Vendor File - Transaction List
 
	autoLibrary
	on error goto Ertn
 
	dim vn$*8,nam$*35,ad1$*20,ad2$*20,csz$*20,ss$*11,holdvn$*8,vcode$*8
	dim cnam$*40,dat$*20,adr(2),id1$*25,cap$*128
	dim rn$*12,de$*30,adr(2),tvn$*8
	dim flit$(4)*16,scrt$(4)*20,scid$*79,desc$(6)*14
	dim sc$(8),sd$(8),se$(8)*30,pl$(8,2)*35,fl2$(7),sc2$(7)*38
 
	fnTop("S:\acsGL\VendorReas",cap$="Reassign Vendor Transaction Addresses")
	fncno(cno,cnam$) : _
	fndat(dat$)
 
	for j=1 to 7: fl2$(j)=str$(j+3)&",2,C 38": next j
	sc2$(1)="1. Initial File Preparation"
	sc2$(2)="2. Add"
	sc2$(3)="3. Edit"
	sc2$(4)="4. pr Proof List"
	fl2$(3)="6,2,C 38,C,N"
	sc2$(5)="5. pr Transaction Listing"
	sc2$(6)="6. Reassign Transaction Addresses"
	sc2$(7)="7. pr 1099 Forms"
	cnam$=rtrm$(cnam$)
	sc$(1)="5,30,C 8,UT,N"
	sc$(2)="6,30,C 35,CUT,N"
	sc$(3)="7,30,C 20,UT,N"
	sc$(4)="8,30,C 20,UT,N"
	sc$(5)="9,30,C 20,UT,N"
	sc$(6)="10,30,N 10.2,UT,N"
	sc$(7)="11,30,N 2,UT,N"
	sc$(8)="12,30,C 11,UT,N"
	for j=1 to udim(se$)
		sd$(j)=str$(j+4)&",9,Cr 20"
	next j
	se$(1)="Vendor Number:"
	se$(2)="Name:"
	se$(3)="Address:"
	se$(4)="Address:"
	se$(5)="City,State Zip Code:"
	se$(6)="YTD Purchases:"
	se$(7)="1099 Type:"
	se$(8)="Federal ID or SS #:"
	gosub L3200
	open #1: "Name=[Q]\GLmstr\GL1099.h[cno],KFName=[Q]\GLmstr\GL109IDX.h[cno],Shr",internal,outIn,keyed ioerr L4750
	open #11: "Name=[Q]\GLmstr\GL1099.h[cno],KFName=[Q]\GLmstr\VNINDX2.h[cno],Shr",internal,outIn,keyed ioerr L4090
L480: open #2: "Name=[Q]\GLmstr\GLTR1099.H[cno],Shr",internal,outIn,relative ioerr L500
	goto L530
L500: close #2: ioerr L510
L510: open #2: "Name=[Q]\GLmstr\GLTR1099.H[cno],RecL=64,Replace",internal,outIn,relative
	write #2,using L850,rec=1: "",0,0,"","",1
L530: pr newpage
	if fnprocess=1 then ti=4 else goto MENU1
	goto L600
 
MENU1: !
	ti=6
	if ti>3 then restore #1,key>="        ",release: nokey L600 eof L600
L600: goto L3330
	pr newpage
	close #102: ioerr L630
L630: open #102: "SROW=4,SCOL=20,EROW=12,ECOL=59,Border=Sr,Caption=<"&cap$,display,outIn
	pr #102: newpage
	pr #102,fields "1,1,Cc 41,R,N": cnam$
	pr #102,fields "2,1,Cc 40,R,N": "Company Number [cno]"
	pr #102,fields "4,1,Cc 40,R,N": "* * *   Warning   * * *"
	pr #102,fields "5,1,Cc 40,N": "This selection will destroy all"
	pr #102,fields "6,1,Cc 40,N": "existing records in the Vendor File."
	pr f "13,34,C 11,B,5": "Cancel (F5)"
	pr #102,fields "8,2,C 24,N": "Enter ERASE to continue:"
L720: input #102,fields "8,27,Cu 5,UT,N": pas$
	if cmdkey=5 then goto L530
	if pas$><"ERASE" then goto L720
	close #1: ioerr L760
L760: close #11: ioerr L770
L770: open #1: "Name=[Q]\GLmstr\GL1099.h[cno],KFName=[Q]\GLmstr\GL109IDX.h[cno]",internal,outIn,keyed ioerr L790
	close #1,free: ioerr L790
L790: open #1: "Name=[Q]\GLmstr\GL1099.h[cno],RecL=127,Replace",internal,outIn,relative ioerr L820
	close #2: ioerr L810
L810: open #2: "Name=[Q]\GLmstr\GLTR1099.H[cno]",internal,outIn,relative ioerr L830
L820: close #2,free: ioerr L830
L830: open #2: "Name=[Q]\GLmstr\GLTR1099.H[cno],RecL=64,Replace",internal,outIn,relative ioerr L840
L840: write #2,using L850,rec=1: "",0,0,"","",1
L850: form pos 1,c 8,n 6,pd 5.2,c 12,c 30,pd 3
	new1=1
	goto L2600
L880: form pos 1,c 8,c 35,3*c 20,pd 5.2,n 2,c 11,2*pd 3
	new1=1
L900: pr newpage
	close #101: ioerr L920
L920: open #101: "SROW=3,SCOL=8,EROW=15,ECOL=66,BORDER=SR,Caption=<"&cap$,display,outIn
	pr f "3,8,Cc 59,R,N": "Enter Vendor Number as blank when completed"
	pr f mat sd$: mat se$
	pr f "16,25,C 09,B,1": "Next (F1)"
	pr f "16,35,C 09,B,5": "Stop (F5)"
! pr f "16,45,C 09,B,5": "Help (F6)" ! didn't work anyway
	if ti=3 or ce>0 then goto L1070
L990: input fields "5,30,C 8,UT,N": vcode$ conv L990
	if ltrm$(rtrm$(vcode$))="0" or rtrm$(vcode$)="" then goto L530
	vcode$=lpad$(rtrm$(vcode$),8)
	read #1,using L880,key=vcode$: vn$,nam$,ad1$,ad2$,csz$,ytdp,typ,ss$,mat adr nokey L1080
	oldti=2
	ti=3
	holdvn$=vn$
	holdytdp=ytdp
L1070: if ti=3 or ce>0 then pr f mat sc$: vn$,nam$,ad1$,ad2$,csz$,ytdp,typ,ss$
L1080: pr f "16,25,C 09,B,1": "Next (F1)"
	pr f "16,35,C 09,B,5": "Stop (F5)"
L1100: input fields mat sc$: vn$,nam$,ad1$,ad2$,csz$,ytdp,typ,ss$ conv CONVC
	if ce>0 then sc$(ce)(ce1:ce2)="U": ce=0
	if cmdkey>0 then goto L1190 else ce=curfld+1
	if ce>udim(sc$) then ce=1
L1140: sc$(ce)=rtrm$(uprc$(sc$(ce))) : ce1=pos(sc$(ce),"U",1)
	ce2=ce1+1 : sc$(ce)(ce1:ce1)="UC" : goto L1100
CONVC: if ce>0 then sc$(ce)(ce1:ce2)="U"
	ce=cnt+1
ERRC: pr f "24,78,C 1": bell : goto L1140
L1190: if cmdkey=5 then goto MENU1
	vn$=lpad$(rtrm$(vn$),8)
	if ti=3 then goto L1460
	if rtrm$(vn$)="" or ltrm$(rtrm$(vn$))="0" then goto L530
	read #1,using L1240,key=vn$: vn$ nokey L1270
L1240: form pos 1,c 8
	pr f "5,35,c 30,H,N": "Duplicate Vendor Number"
	goto ERRC
L1270: mat adr=(0)
	if ytdp=0 then goto L1320
	rec2=lrec(2)+1
	write #2,using L850,rec=rec2: vn$,dat,ytdp,"","Beginning Balance",0
	mat adr=(rec2)
L1320: write #1,using L880: vn$,nam$,ad1$,ad2$,csz$,ytdp,typ,ss$,mat adr
	goto L600
L1340: pr newpage
	pr f "10,15,C 37,N": "Vendor Number (blank when completed):"
L1360: input fields "10,60,C 8,UT,N": vcode$ conv L1360
	if ltrm$(rtrm$(vcode$))="0" or rtrm$(vcode$)="" then goto L530
	vcode$=lpad$(rtrm$(vcode$),8)
	read #1,using L880,key=vcode$: vn$,nam$,ad1$,ad2$,csz$,ytdp,typ,ss$,mat adr nokey L1360
L1400: holdvn$=vn$
	holdytdp=ytdp
L1420: pr newpage
	pr f "2,13,C 45,N": "*** Review Vendor Records ***"
	pr f "3,10,c 60,n": "Enter Vendor Number as blank to Delete"
	goto L900
L1460: if ltrm$(vn$)="" or rtrm$(ltrm$(vn$))="0" then goto L1470 else goto L1650
L1470: pr newpage
	pr f "10,10,c 60,n": "Vendor Number "&holdvn$&" will be Deleted"
	pr f "12,10,C 31,N": "Do you wish to continue? (Y/N):"
L1500: input fields "12,43,Cu 1,UT,N": yn$ conv L1500
	if yn$="Y" then goto L1550
	if yn$><"N" then goto L1500
	vn$=holdvn$
	goto L1420
L1550: delete #1,key=holdvn$:
	new1=1
	rec4=adr(1)
	if adr(1)=0 then goto L1340
L1590: read #2,using L1610,rec=rec4: tvn$, am,nta
	rewrite #2,using L1610,rec=rec4: "", 0,nta
L1610: form pos 1,c 8,pos 15,pd 5.2,pos 62,pd 3
	if nta=0 then goto L1340
	rec4=nta
	goto L1590
L1650: if holdvn$=vn$ then goto L1780 else goto L1660
L1660: read #1,using L1240,key=vn$: vn$ nokey L1680
	goto L1340
L1680: delete #1,key=holdvn$:
	new1=1
	rec5=adr(1)
	if adr(1)=0 then goto L1780
L1720: read #2,using L1730,rec=rec5: holdvn$,nta
L1730: form pos 1,c 8,pos 62,pd 3
	rewrite #2,using L1730,rec=rec5: vn$,nta
	if nta=0 then goto L1780
	rec5=nta
	goto L1720
L1780: if holdytdp=ytdp then goto L1800
	goto L2710
L1800: pr newpage
	if adr(1)=0 then goto L2710
	pr f "10,10,C 33,N": "Review Detail Transactions (Y/N):"
	yn$="N"
L1840: rinput fields "10,45,Cu 1,UT,N": yn$ conv L1840
	if yn$="Y" then goto L2710
	if yn$<>"N" then goto L1840
L1870: if holdvn$=vn$ then goto L1890
	goto L1320
L1890: rewrite #1,using L880,key=vn$: vn$,nam$,ad1$,ad2$,csz$,ytdp,typ,ss$,mat adr
	if oldti=2 then ti=2
	oldti=0
	goto L600
 
	pr newpage
	namtab=66-int(len(rtrm$(cnam$))/2)
	dattab=66-int(len(rtrm$(dat$))/2)
	if fnprocess<>1 then gosub ASKDAT
	goto L2130
! _________________________
ASKDAT: !
	pr newpage
	close #102: ioerr L2030
L2030: open #102: "SRow=11,SCol=18,ERow=13,ECol=61,Border=Sr,Caption=<"&cap$,display,outIn
	pr #102: newpage
	pr #102,fields "2,2,C 20,N": "Report Heading Date:"
	pr f "14,34,C 11,B,5": "Cancel (F5)"
L2070: rinput #102,fields "2,23,C 20,UT,N": dat$ conv L2070
	if cmdkey=5 then goto MENU1
	dattab=66-int(len(rtrm$(dat$))/2)
	close #102: ioerr L2120
return
L2120: !
L2130: pr newpage
	close #101: ioerr L2150
L2150: open #101: "SROW=08,SCOL=18,EROW=12,ECOL=58,Border=SR,Caption=<"&cap$,display,outIn
	pr #101: newpage
	pr f "08,18,Cc 41,R,N": cnam$
	pr f "09,18,Cc 41,R,N": "Company Number [cno]"
	pr f "11,18,Cc 41,N": "Printing..."
	pr f "13,34,C 11,B,5": "Cancel (F5)"
	on fkey 5 goto L2560
	fnopenprn
L2230: j=0
	eofc=0
L2250: read #1,using L880,release: vn$,nam$,ad1$,ad2$,csz$,ytdp,typ,ss$,mat adr eof L2530
	j=j+1
	pl$(1,j)=vn$
	pl$(2,j)=nam$
	pl$(3,j)=ad1$
	pl$(4,j)=ad2$
	pl$(5,j)=csz$
	pl$(6,j)=str$(ytdp)
	pl$(7,j)=str$(typ)
	pl$(8,j)=ss$
	if j=2 then goto L2370
	goto L2250
L2370: if pcnt><0 then goto L2400
	pr #255,using L2390: date$('mm/dd/yy'),cnam$,time$,"Vendor Proof List",dat$
L2390: form skip 3,pos 1,c 8,pos namtab,c 40,skip 1,pos 1,c 8,pos 57,c 20,skip 1,pos dattab,c 20,skip 2
L2400: for i=1 to 8
		pr #255,using L2420: se$(i),pl$(i,1),pl$(i,2)
L2420: form pos 1,c 30,pos 35,c 35,pos 75,c 35,skip 1
	next i
	pr #255:
	mat pl$=(" ")
	if eofc=1 then goto L2560
	pcnt=pcnt+1
	if pcnt=6 then goto L2500
	goto L2230
L2500: pr #255: newpage
	pcnt=0
	goto L2230
L2530: if j=0 then goto L2560
	eofc=1
	goto L2370
L2560: fncloseprn
	on fkey 5 ignore
	if fnprocess=1 then goto Xit
	goto L530
L2600: close #1:
	rewrite #2,using L3510,rec=1: lrec(2)
	close #2:
	if new1=1 then goto L2650
	if ti=0 and i2=0 then goto Xit
L2650: close #11: ioerr L2660
L2660: execute "Index [Q]\GLmstr\GL1099.h[cno]"&' '&"[Q]\GLmstr\GL109IDX.h[cno] 1 8 Replace DupKeys -n"
	execute "Index [Q]\GLmstr\GL1099.h[cno]"&' '&"[Q]\GLmstr\VNINDX2.h[cno] 9 25 Replace DupKeys -n"
	if i2=1 then goto L2690 else goto Xit
L2690: fnchain("S:\acsGL\GLBld109")
	goto L2710
L2710: tt=0
	rec3=adr(1)
	if adr(1)=0 then goto L3010
L2740: read #2,using L2760,rec=rec3: tvn$,dt,am,rn$,de$,nta
	tt=tt+am
L2760: form pos 1,c 8,n 6,pd 5.2,c 12,c 30,pd 3
	scid$="Vendor #: "&ltrm$(vn$)&"   Balance: "&str$(ytdp)&"   Transactions: "&str$(tt)
	pr newpage
	close #101: ioerr L2800
L2800: open #101: "SROW=5,SCOL=8,EROW=13,ECOL=62,Border=SR,Caption=<"&cap$,display,outIn
	pr f mat desc$: mat scrt$
	pr f "14,30,C 09,B,1": "Next (F1)"
	pr f "14,41,C 09,B,5": "Stop (F5)"
L2840: rinput fields mat flit$: dt,am,rn$,de$ conv CONVT
	if ce>0 then flit$(ce)(ce1:ce2)="U": ce=0
	if cmdkey>0 then goto L2930 else ce=curfld+1
	if ce>udim(flit$) then ce=1
L2880: flit$(ce)=rtrm$(uprc$(flit$(ce))) : ce1=pos(flit$(ce),"U",1)
	ce2=ce1+1 : flit$(ce)(ce1:ce1)="UC" : goto L2840
CONVT: if ce>0 then flit$(ce)(ce1:ce2)="U"
	ce=cnt+1
ERRT: pr f "24,78,C 1": bell : goto L2880
L2930: if cmdkey=5 then goto L530
	if dt=0 then goto L2960
	if dt<10100 or dt>123199 then goto L2840
L2960: !
	rewrite #2,using L2760,rec=rec3: tvn$,dt,am,rn$,de$,nta
	if nta=0 then goto L3010
	rec3=nta
	goto L2740
L3010: if tt><ytdp then goto L3030
	goto L1870
L3030: pr newpage
! Dim IOX$(2)
	pr f "8,10,Cc 60,H,N": "YTD Purchases do not agree with Total Transactions."
	pr f "10,21,C 78": "YTD Purchases:  "
	pr f "10,40,N 12.2,N": ytdp
	pr f "11,18,C 60": "Total Transactions:  "
	pr f "11,40,N 12.2,N": tt
	iox$(1)="13,28,C 24,N"
	iox$(2)="14,28,C 24,N"
L3120: rinput select mat iox$,attr "H": "1. Correct YTD Purchases","2. Correct Transactions"
	j1=curfld
	on j1 goto L3150,L2710 none L3120
L3150: pr newpage
	pr f "10,2,c 25,n": "New YTD Purchases:"
	ytdp=tt
L3180: rinput fields "10,35,Nz 11.2,UT,N": ytdp conv L3180
	goto L3010
L3200: flit$(1)="6,30,Nz 6,UT,N"
	flit$(2)="8,30,n 10.2,UT,N"
	flit$(3)="10,30,C 12,UT,N"
	flit$(4)="12,30,c 30,UT,N"
	desc$(1)="6,9,Cr 20"
	desc$(2)="8,9,Cr 20"
	desc$(3)="10,9,Cr 20"
	desc$(4)="12,9,Cr 20"
	scrt$(1)="Date:"
	scrt$(2)="Amount:"
	scrt$(3)="Reference Number:"
	scrt$(4)="Description:"
return
L3330: pr newpage
	pr f "10,15,Cc 60,N": "Reassigning Transaction Addresses..."
	restore #1,key>="        ": nokey L3360
L3360: read #1,using L3370: mat adr eof L3400
L3370: form pos 122,2*pd 3
	rewrite #1,using L3370: 0,0
	goto L3360
L3400: lr2=lrec(2)
	rewrite #2,using L3510,rec=1: lr2
	for j=1 to lr2
		read #2,using L3440,rec=j: vn$,nta noRec L3520
L3440: form pos 1,c 8,pos 62,pd 3
		read #1,using L3370,key=vn$: mat adr nokey L3520
		if adr(1)=0 then adr(1)=j
		if adr(2)>0 then rewrite #2,using L3510,rec=adr(2): j
		adr(2)=j
		rewrite #1,using L3370,key=vn$: mat adr
		rewrite #2,using L3510,rec=j: 0
L3510: form pos 62,pd 3
L3520: next j
	goto Xit
	gosub ASKDAT
	pr newpage
	close #101: ioerr L3570
L3570: open #101: "SROW=08,SCOL=18,EROW=12,ECOL=58,BORDER=SR,CAPTION=<"&cap$,display,outIn
	pr #101: newpage
	pr #101,fields "1,1,Cc 41,R,N": cnam$
	pr #101,fields "2,1,Cc 41,R,N": "Company Number [cno]"
	pr #101,fields "4,1,Cc 41,N": "Printing..."
	pr f "13,34,C 11,B,5": "Cancel (F5)"
	on fkey 5 goto L2560
	fnopenprn
	gosub L3870
L3660: read #1,using L3670: vn$,nam$,ytdp,mat adr eof L2560
L3670: form pos 1,c 8,c 35,pos 104,pd 5.2,pos 122,2*pd 3
	fst=0
	ec$=""
	tot=0
	if adr(1)=0 and ytdp=0 then goto L3660
	nta=adr(1)
L3730: read #2,using L3740,rec=nta,release: dt,am,rn$,de$,nta noRec L3660
L3740: form pos 9,n 6,pd 5.2,c 12,c 30,pd 3
	tot=tot+am
	if nta=0 then goto L3770 else goto L3780
L3770: if ytdp=tot then goto L3780 else ec$="ERROR"
L3780: if fst=0 then pr #255,using L3790: vn$,nam$,dt,rn$,de$,am,ytdp,ec$ pageoflow L3820 else pr #255,using L3800: dt,rn$,de$,am,ec$ pageoflow L3820
L3790: form pos 1,c 8,pos 10,c 35,pos 46,pic(zz/zz/zz),pos 56,c 12,pos 69,c 30,pos 100,n 10.2,pos 115,n 10.2,pos 127,c 5,skip 1
L3800: form pos 46,pic(zz/zz/zz),pos 56,c 12,pos 69,c 30,pos 100,n 10.2,pos 127,c 5,skip 1
	goto L3830
L3820: gosub L3860
L3830: fst=1
	if nta=0 then pr #255: else goto L3730
	goto L3660
L3860: pr #255: newpage
L3870: p2=p2+1
	pr #255,using L3890: date$('mm/dd/yy'),cnam$,time$,"Vendor Transaction Listing"
L3890: form skip 1,pos 1,c 8,pos 41,cc 40,skip 1,pos 1,c 8,pos 53,c 40,skip 1
	p1=66-int((len(rtrm$(dat$))+6)/2)
	pr #255,using L3920: rtrm$("As of "&dat$),"Page",p2
L3920: form pos p1,c 30,pos 110,c 4,n 5,skip 2
	pr #255,using L3940: "Vendor #","Vendor Name","Date","Reference #","Description","Amount","YTD Purchases"
L3940: form pos 1,c 8,pos 10,c 11,pos 48,c 4,pos 56,c 11,pos 70,c 11,pos 104,c 6,pos 112,c 13,skip 2
return
! _________________________________________
	ce=curfld
	ce1=pos(uprc$(sc$(ce)),"U",1)
	ce2=ce1+1
	sc$(ce)(ce1:ce2)="UC"
	read #10,using L4020,rec=ce: mat hlp$ noRec L4080
L4020: form pos 1,20*c 78
	pr f mat flh$: mat hlp$,se$(ce),"Enter 0 to Continue or 1 to Update Help Screen:"
L4040: input fields "24,69,N 1,EUT,N": j2 conv L4040
	if j2<>1 then goto L4080
	input fields mat flh$: mat hlp$
	rewrite #10,using L4020,rec=ce: mat hlp$
L4080: if ti=3 then goto L1420 else goto L900
L4090: close #1: ioerr L4100
L4100: close #11: ioerr L4110
L4110: execute "Index [Q]\GLmstr\GL1099.h[cno]"&' '&"[Q]\GLmstr\VNINDX2.h[cno] 9 25 Replace DupKeys -n"
	open #1: "Name=[Q]\GLmstr\GL1099.h[cno],KFName=[Q]\GLmstr\GL109IDX.h[cno],Shr",internal,outIn,keyed
	open #11: "Name=[Q]\GLmstr\GL1099.h[cno],KFName=[Q]\GLmstr\VNINDX2.h[cno],Shr",internal,outIn,keyed
	goto L480
! SEARCH  VENDOR FILE
	pr newpage
	close #101: ioerr L4180
L4180: open #101: "SROW=10,SCOL=27,EROW=13,ECOL=52,BORDER=SR,CAPTION=<"&cap$,display,outIn
	pr #101: newpage
	pr #101,fields "2,2,C,N": "Search by: "
	pr f "14,35,C 09,B,5": "Stop (F5)"
L4220: rinput #101,select "2,13,C 13,N;3,13,C 13,N",attr "H": "Vendor Name","Vendor Number"
	ti2=curfld
	if cmdkey=5 then close #101: : goto MENU1
L4250: on ti2 goto L4260,L4370 none L4220
L4260: close #101: ioerr L4270
L4270: open #101: "SROW=10,SCOL=10,EROW=12,ECOL=69,BORDER=SR,CAPTION=<"&cap$,display,outIn
	pr #101,fields "2,2,C 32,N": "Search Criteria (blank for all):"
	pr f "13,35,C 09,B,5": "Stop (F5)"
L4300: input #101,fields "2,35,C 25,UE,N": id1$
	if cmdkey=5 then close #101: : goto MENU1
	id1$=rtrm$(id1$)
	l1=len(id1$)
	s1=11
	restore #s1,search>=id1$,release: nokey L4300
	goto L4480
L4370: close #101: ioerr L4380
L4380: open #101: "SROW=10,SCOL=15,EROW=12,ECOL=65,BORDER=SR,CAPTION=<"&cap$,display,outIn
	pr #101: newpage
	pr #101,fields "2,2,C 40": "Beginning Vendor Number (blank for all):"
	pr f "13,35,C 09,B,5": "Stop (F5)"
L4420: input #101,fields "2,43,Nz 8,UT,N": vn1 conv L4420
	if cmdkey=5 then close #101: : goto MENU1
	vn$=lpad$(str$(vn1),8)
	restore #1,search>=vn$,release: nokey L4420
	s1=1
	l1=0
L4480: close #101: ioerr L4490
L4490: open #101: "SROW=2,SCOL=2,EROW=23,ECOL=79,BORDER=SR,CAPTION=<"&cap$,display,outIn
	ln=0
	pr #101: newpage
	pr f "2,3,C 8,R,N": "Vendor #"
	pr f "2,13,C 35,R,N": "Vendor Name"
	pr f "2,50,C 12,R,N": " Phone #"
	pr f "2,64,C 10,R,N": " Sales YTD"
L4560: read #s1,using L880,release: vn$,nam$,ad1$,ad2$,csz$,ytdp,typ,ss$,mat adr eof L4650
	if l1=0 then goto L4590
	if v$(1)(1:l1)><id1$ then goto L4650
L4590: ln=ln+1
	pr f str$(ln+2)&",3,C 8,N": vn$
	pr f str$(ln+2)&",13,C 35,N": nam$
	pr f str$(ln+2)&",50,C 12,N": ss$
	pr f str$(ln+2)&",64,N 10.2,N": ytdp
	if ln<20 then goto L4560
L4650: pr f "23,13,C 33,N": "or enter Vendor Number to modify:"
	pr f "23,3,C 09,B,5": "Stop (F5)"
L4670: input fields "23,47,Cu 8,UT,N": vcode$ conv L4670
	if cmdkey=5 then goto L4250
	if rtrm$(vcode$)="" then goto L4730
	vcode$=lpad$(rtrm$(vcode$),8)
	read #1,using L880,key=vcode$,release: vn$,nam$,ad1$,ad2$,csz$,ytdp,typ,ss$,mat adr nokey L4670
	ti=3: goto L1400
L4730: if ln<20 then goto L4250
	goto L4480
L4750: if err=4152 then goto L790
Xit: fnXit
 
include: Ertn
 
