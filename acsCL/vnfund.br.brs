! Replace S:\acsCL\VnFund
! pr Vendor Fund Listing
 
	autoLibrary
	on error goto Ertn
 
	dim cnam$*40,cap$*128,p$(20)*50
	dim tr$(5)*35,tr(2),p1$*30,gl(3),f1(1000),in1(3),f2(1000)
 
	fncno(cno,cnam$)
	fnTop(program$, cap$="Payee Fund Listing")
	cancel=99
 
	fnTos(sn$="vnfund") : _
	respc=0
	fnLbl(1,40,"",1,1)
	fnLbl(1,1,"Starting Date:",25,1)
	fnTxt(1,27,8,0,1,"1") : _
	resp$(respc+=1)=""
	fnLbl(2,1,"Ending Date:",25,1)
	fnTxt(2,27,8,0,1,"1") : _
	resp$(respc+=1)=""
	fnLbl(3,1,"Minimum Amount to Print:",25,1)
	fnTxt(3,27,10,0,1,"10") : _
	resp$(respc+=1)=""
	fnCmdSet(2): fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	in1(1)=val(resp$(1))
	in1(2)=val(resp$(2))
	in1(3)=val(resp$(3))
	open #paymstr=13: "Name=[Q]\CLmstr\PayMstr.H[cno],KFName=[Q]\CLmstr\PayIdx1.H[cno],Shr",internal,outIn,keyed
	open #trmstr1=1: "Name=[Q]\CLmstr\TrMstr.H[cno],KFName=[Q]\CLmstr\TrIdx1.H[cno],Shr",internal,outIn,keyed
	open #trmstr2=2: "Name=[Q]\CLmstr\TrMstr.H[cno],KFName=[Q]\CLmstr\TrIdx2.H[cno],Shr",internal,outIn,keyed
	open #tralloc=3: "Name=[Q]\CLmstr\TrAlloc.H[cno],Version=2,KFName=[Q]\CLmstr\TrAlloc-Idx.h[cno],Shr",internal,outIn,keyed
	fnopenprn
	gosub HDR
READ_TRMSTR2: !
	read #trmstr2,using 'Form POS 1,N 2,N 1,C 8,G 6,pd 10.2,C 8,C 35,N 1,N 6,N 1': bank_code,tcde,tr$(1),tr$(2),tr3,tr$(4),tr$(5),pcde,clr,scd eof END1 : _
	tr$(3)=str$(tr3)
	if tcde=1 or tcde=4 then goto L350 else goto READ_TRMSTR2
L350: if scd=1 or scd=8 then goto L360 else goto READ_TRMSTR2
L360: d1=fndate_mmddyy_to_ccyymmdd(val(tr$(2)))
! If BANK_CODE=77 Then Pause
	if d1<fndate_mmddyy_to_ccyymmdd(in1(1)) or d1>fndate_mmddyy_to_ccyymmdd(in1(2)) then goto READ_TRMSTR2
	if trim$(tr$(4))="" then goto READ_TRMSTR2 : _
		! don't try to analyze any transaction that does not have a vendor #
	if rtrm$(vn$)="" then goto L410
	if vn$><tr$(4) then gosub PRINTARRAY
L410: vn$=tr$(4)
	p1$=tr$(5)(1:30)
	key$=cnvrt$("Pic(zz)",bank_code)&str$(tcde)&tr$(1) : _
	restore #tralloc,key>=key$: nokey READ_TRMSTR2
READ_TRALLOC: !
	read #tralloc,using 'Form Pos 1,C 11,N 3,N 6,N 3,PD 5.2': newkey$,mat gl,amt eof READ_TRMSTR2 : _
	if newkey$<>key$ then goto READ_TRMSTR2
	if gl(1)=0 then : _
		f1(1000)+=amt else : _
		f1(gl(1))+=amt
	goto READ_TRALLOC
 
PRINTARRAY: ! pr ARRAY
	if sum(f1)<in1(3) then goto RESET_F1
	if trim$(vn$)<>'' then : _
		read #paymstr,using 'Form POS 9,C 30',key=vn$: p1$ nokey L520
L520: pr #255,using 'Form POS 1,C 10,C 30': vn$,p1$
PRINTARRAY_2: !
	for j=1 to 1000
		if f1(j)<>0 then : _
			pr #255,using 'Form POS 10,C 12,PIC(----,---,---,---.##)': "Fund: "&str$(j),f1(j) pageoflow NEWPGE
	next j
	pr #255,using 'Form POS 10,C 12,PIC(----,---,---,---.##)': " Total  ",sum(f1)
	pr #255: ""
	mat f2=f2+f1
RESET_F1: mat f1=(0)
return
 
NEWPGE: !
	pr #255: newpage
	gosub HDR
	continue
 
HDR: !
	pr #255,using 'Form POS 1,C 4,N 4,Cc 72': "Page",pg+=1,cnam$
	pr #255,using 'Form POS 1,c 8,Cc 72': date$,"Date From: "&cnvrt$("pic(zz/zz/zz)",in1(1))&" To: "&cnvrt$("pic(zz/zz/zz)",in1(2))
	pr #255,using 'Form POS 9,Cc 72': "Vendor Fund Listing (Minimum Amount: "&ltrm$(cnvrt$("PIC($$$$,$$$,$$$.##BCR)",in1(3)))&")" : _
	pr #255: ""
return
 
END1: gosub PRINTARRAY
	pr #255,using 'Form POS 1,C 10,C 30': "  Grand","Total"
	mat f1=f2
	gosub PRINTARRAY_2
	fncloseprn
	goto Xit
 
Xit: fnXit
 
include: ertn
