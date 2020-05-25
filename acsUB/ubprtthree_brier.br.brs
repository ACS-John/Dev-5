! Replace S:\acsUB\ubprtthree_Brier
! pr bills for Village of Monticello (full page)
 
	autoLibrary
	on error goto Ertn
 
	dim resp$(12)*60,txt$*100,mg$(3)*60,fb$(3)*60
	dim z$*10,e$(4)*30,f$*12,g(12),d(15),b(11),extra1$*30
	dim gb(10),pe$(4)*30,ba$(4)*30,at$(3)*40
	dim prebal$*30
 
	fnLastBillingDate(d1)
	open #21: "Name=[Q]\UBmstr\Company.h[cno],Shr",internal,input  : _
	read #21,using "Form POS 41,2*C 40": at$(2),at$(3) : _
	close #21:
	open #ratemst:=8: "Name=[Q]\UBmstr\ubData\RateMst.h[cno],KFName=[Q]\UBmstr\ubData\RateIdx1.h[cno],Shr",internal,input,keyed
	at$(1)=env$('cnam') : _
	z=21 : _
	at$(1)=trim$(at$(1))(1:z) : _
	x=len(at$(1)) : y=z-x : _
	at$(1)=rpt$(" ",int(y/2))&at$(1)
	z=26 : _
	for j=2 to udim(at$) : _
		at$(j)=trim$(at$(j))(1:z) : _
		x=len(at$(j)) : y=z-x : _
		at$(j)=rpt$(" ",int(y/2))&at$(j) : _
	next j
	linelength=62
 
	gosub BULKSORT
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed  ! open in Account order
	open #2: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx5.h[cno],Shr",internal,input,keyed  ! open in route-sequence #
	open #ubtransvb=15: "Name=[Q]\UBmstr\UBTransVB.h[cno],KFName=[Q]\UBmstr\UBTrIndx.h[cno],Shr",internal,outIn,keyed
 
	prebal$="10:00 AM, xxxxxxx  xx"
SCREEN1: !
	a$="" : prtbkno=0
	fnTos(sn$="UBPrtBl1-1") : _
	pf=26 : ll=24 : _
	respc=0
	fnLbl(1,1,"Current Reading Date:",ll,1)
	fnTxt(1,pf,8,8,1,"1",0,tt$) : _
	resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d2)
	fnLbl(2,1,"Previous Reading Date:",ll,1)
	fnTxt(2,pf,8,8,1,"1",0,tt$) : _
	resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d3)
	fnLbl(3,1,"Penalty Due Date:",ll,1)
	fnTxt(3,pf,8,8,1,"1",0,tt$) : _
	resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d4)
	fnLbl(4,1,"Message on Bill:",ll,1)
	fnTxt(4,pf,60,60) : _
	resp$(respc+=1)=mg$(1)
	fnTxt(5,pf,60,60) : _
	resp$(respc+=1)=mg$(2)
	fnTxt(6,pf,60,60) : _
	resp$(respc+=1)=mg$(3)
	fnLbl(7,1,"Date of Billing:",ll,1)
	fnTxt(7,pf,8,8,1,"1") : _
	resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d1)
	fnLbl(8,1,"Starting Account:",ll,1)
	fncombof("ubm-act-nam",8,pf,40,"[Q]\UBmstr\Customer.h[cno]",1741,9,41,30,"[Q]\UBmstr\ubindx5.h[cno]",2) : _
	resp$(respc+=1)="[All]"
	fnLbl(9,1,"Route Number:",ll,1)
	fncmbrt2(9,pf) : _
	resp$(respc+=1)="[All]"
	fnChk(10,pf,"Select Accounts to Print",1) : _
	resp$(respc+=1)="False"
	fnCmdSet(3) : _
	fnAcs2(mat resp$,ck)
	if ck=5 then goto ENDSCR
	d2x= val(resp$(1)) : _
	d3x= val(resp$(2)) : _
	d4 = val(resp$(3)) : _
	mg$(1) = resp$(4) : _
	mg$(2) = resp$(5) : _
	mg$(3) = resp$(6) : _
	d1 = val(resp$(7))
	if resp$(8)="[All]" then : _
		a$="" else : _
		a$ = lpad$(trim$(resp$(8)(1:9)),9)
	if resp$(9)="[All]" then : _
		prtbkno=0 else : _
		prtbkno = val(resp$(9))
	if resp$(10)="True" then sl1=1: z$="" else sl1=0
	if trim$(a$)<>"" then read #2,using L540,key=a$: z$,route,sequence nokey SCREEN1 : _
		holdz$=z$: begin=1 : _
		st1=1
L540: form pos 1,c 10,pos 1741,n 2,n 7
	if trim$(a$)="" and prtbkno=0 then restore #2,key>="         ": ! if no beginning account or starting route #, start at beginning of file
	if trim$(a$)<>"" then restore #2,key=cnvrt$("pic(zz)",route)& cnvrt$("pic(zzzzzzz)",sequence): nokey SCREEN1
	if trim$(a$)="" and prtbkno>0 then restore #2,key>=cnvrt$("pic(zz)",prtbkno)&"       ": ! selected a route and no beginning Account
 
	open #3: "Name=[Q]\UBmstr\UBAdrBil.h[cno],KFName=[Q]\UBmstr\adrIndex.h[cno],Shr",internal,input,keyed
	fnpa_open
 
	on fkey 5 goto RELEASE_PRINT
L630: if sl1=1 then goto SCREEN3
L640: read #6,using L670: z$ eof RELEASE_PRINT
	if trim$(a$)<>"" and begin=1 and z$<>holdz$ then goto L640 ! start with
	begin=0 ! cancel starting account
L670: form pos 22,c 10
	read #1,using L690,key=z$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate nokey L640
L690: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1750,2*n 6,pos 1942,c 12,pos 1864,c 30,pos 1831,n 9,pos 1821,n 1
	if prtbkno=0 then goto L720
	if prtbkno><route then goto RELEASE_PRINT
L720: if f><d1 then goto L630
L730: e1=0 : mat pe$=("")
	for j=2 to 4
		if rtrm$(e$(j))<>"" then : _
			e1=e1+1 : pe$(e1)=e$(j)
	next j
	if st1=0 then goto READALTADR
! If ST1$=Z$ Then sT1=0 Else Goto 560
READALTADR: !
! read alternate billing address
	read #3,using L820,key=z$: mat ba$ nokey L910
	if trim$(ba$(1))="" and trim$(ba$(2))="" and trim$(ba$(3))="" and trim$(ba$(4))="" then goto L910
L820: form pos 11,4*c 30
	e1=0 : mat pe$=("")
	for j=1 to 4
		if rtrm$(ba$(j))<>"" then : _
			e1=e1+1 : pe$(e1)=ba$(j)
	next j
	if trim$(pe$(2))="" then pe$(2)=pe$(3): pe$(3)=""
	if trim$(pe$(3))="" then pe$(3)=pe$(4): pe$(4)=""
	goto L1030
 
L910: !
	if trim$(extra1$)<>"" then pe$(4)=pe$(3): pe$(3)=extra1$ ! set third address line to extra1$ (2nd address)
	goto L1030
 
RELEASE_PRINT: !
	close #1: ioerr ignore
	close #3: ioerr ignore
	fnpa_finis
	goto ENDSCR
 
L1030: !
	pb=bal-g(11)
	if bal<=0 then g(9)=g(10)=0 ! don't show penalty if balance 0 or less
	fb$(1)=mg$(1)
	fb$(2)=mg$(2)
	fb$(3)=mg$(3)
	if c4>0 then fb$(1)="          Final Bill" : fb$(2)="": fb$(3)=""
! ______________print bill routine______________________________________
	gosub VBPRINT
! _____________end of pr routine______________________________________
	bct(2)=bct(2)+1 : _
	! accumulate totals
	goto L630
 
SCREEN3: !
	fnTos(sn$:= "UBPrtBl1-2")
	fnLbl(1,1,"Account (blank to stop)",31,1)
	if trim$(z$)<>"" then : _
		fnLbl(3,1,"Last Account entered was "&z$,44,1)
	fncmbact(1,17) : _
	resp$(1)=a$
	fnCmdSet(3): fnAcs2(mat resp$,ck)
	a$ = lpad$(trim$(resp$(1)(1:10)),10) : _
	if trim$(a$)="" then goto RELEASE_PRINT
	if ck=5 then goto RELEASE_PRINT
	read #1,using L690,key=a$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate,final nokey SCREEN3
	goto L730
 
ENDSCR: ! pr totals screen
	if sum(bct)=0 then pct=0 else pct=bct(2)/sum(bct)*100
	fnTos(sn$="Bills-Total") : _
	mylen=23 : mypos=mylen+2 : _
	respc=0
	fnLbl(1,1,"Total Bills Printed:",mylen,1)
	fnTxt(1,mypos,8,0,1,"",1) : _
	resp$(respc+=1)=cnvrt$("N 8",sum(bct))
	fnCmdSet(52) : _
	fnAcs2(mat resp$,ck)
Xit: fnXit
IGNORE: continue
ERTN: fnerror(program$,err,line,act$,"Xit")
	if uprc$(act$)<>"PAUSE" then goto L1740
	execute "list -"&str$(line) : pause : goto L1740
	pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause
L1740: execute act$
	goto ERTN
 
VBPRINT: ! r: Brier Lake - 3 per page  Utility Bills  requires: (z$,fb$(1),mat d, mat g,mat pe$,d2,d3,pb)
	fnpa_fontsize
	fnpa_txt(trim$(z$),62,lyne+7)
	fnpa_txt(trim$(fb$(1)),95,lyne+7)
	if pb<>0 then de$="Prv" else de$="   "
	fnpa_txt(rpt$(' ',23)&de$&cnvrt$("pic(-----.--)",pb),1,lyne+25)
	if g(1)>0 then de$="Wat" else de$="   "
	fnpa_txt(cnvrt$("pic(zzzzzzzz)",d(1))&cnvrt$("pic(zzzzzzzz)",d(2))&cnvrt$("pic(zzzzzz)",d(3))&" "&de$&cnvrt$("pic(-----.--)",g(1))&"     "&pe$(1)(1:22),1,lyne+30)
	if g(2)>0 then de$="Sew" else de$="   "
	fnpa_txt(rpt$(' ',23)&de$&cnvrt$("pic(-----.##)",g(2))&"     "&pe$(2)(1:22),1,lyne+35)
	if g(3)>0 then de$="Fee" else de$="   "
	fnpa_txt(rpt$(' ',23)&de$&cnvrt$("pic(-----.--)",g(3))& "     "&pe$(3)(1:22),1,lyne+40)
	if g(5)>0 then
		fnpa_txt(rpt$(' ',23)&"P/T"&cnvrt$("pic(-----.##)",g(5))&"     "&pe$(4)(1:22) ,1,lyne+45)
	end if
	if g(6)>0 then
		fnpa_txt(rpt$(' ',23)&'DEQ'&cnvrt$("pic(-----.##)",g(6)),1,lyne+50)
	end if
	if g(8)>0 then
		fnpa_txt("Other Charge"&cnvrt$("pic(-------.##)",g(8)),1,lyne+55)
	end if
	if g(9)>0 then de$="Tax" else de$="   "
	if d2=0 then d2=d3x
	if d3=0 then d3=d2x
	txt$=cnvrt$("pic(zzbzzbbzz)",d3x)&" "&cnvrt$("pic(zzbzzbbzz)",d2x)&cnvrt$("pic(----.##)",g(12)+pb) &cnvrt$("pic(------.##)",g(11)+pb)&"    "&cnvrt$("pic(-----.##)",g(12)+pb)&"   "&cnvrt$("pic(zzbzzbzz)",d4)&cnvrt$("pic(-----.##)",g(11)+pb)
	fnpa_txt(txt$,1,lyne+76)
	bills+=1
	if int(bills/3)=bills/3 then
		fnpa_newpage
		lyne=0
	else
		lyne=lyne+90
	end if
return ! /r
BULKSORT: ! r: bulk sort order
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed  ! open in Account order
	open #6: "Name="&env$('Temp')&"\Temp."&session$&",Replace,RecL=31",internal,output
	L2190: read #1,using "Form POS 1,C 10,pos 1741,n 2,pos 1743,n 7,pos 1942,c 12": z$,route,seq,bulk$ eof L2220
	write #6,using "Form POS 1,C 12,n 2,n 7,c 10": bulk$,route,seq,z$
	goto L2190
	L2220: close #1: ioerr ignore
	close #6: ioerr ignore
	execute "Index "&env$('Temp')&"\Temp."&session$&" "&env$('Temp')&"\Tempidx."&session$&" 1,19,Replace,DupKeys -n" ioerr L2260
	open #6: "Name="&env$('Temp')&"\Temp."&session$&",KFName="&env$('Temp')&"\Tempidx."&session$,internal,input,keyed
L2260: return ! /r
