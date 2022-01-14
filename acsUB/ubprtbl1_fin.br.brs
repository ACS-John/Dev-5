! Replace S:\acsUB\ubprtbl1_fin
! pr bills for Village of Findlay
!
	autoLibrary
		on error goto Ertn

		dim resp$(10)*40,txt$*45,mg$(3)*30,rw(22,13),cap$*128,a(7)
		dim z$*10,e$(4)*30,f$*12,g(12),d(15),w$*31,y$*39,x$*70,b(11),extra1$*30
		dim gb(10),pe$(4)*30,ba$(4)*30,at$(3)*40,datafile$*256,indexfile$*256
!
	fnLastBillingDate(d1)
	open #21: "Name=[Q]\UBmstr\Company.h[cno],Shr",internal,input
	read #21,using "form pos 41,2*C 40": at$(2),at$(3)
	close #21:
	penalty_rate=.1 ! if env$('client')='Findlay' then penalty_rate=.1 else penalty_rate=.05
	at$(1)=env$('cnam')
	z=21
	at$(1)=trim$(at$(1))(1:z)
	x=len(at$(1)) : y=z-x
	at$(1)=rpt$(" ",int(y/2))&at$(1)
	z=26
	for j=2 to udim(at$)
		at$(j)=trim$(at$(j))(1:z)
		x=len(at$(j)) : y=z-x
			at$(j)=rpt$(" ",int(y/2))&at$(j)
		next j
		linelength=62
	!
		gosub BULKSORT
		open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",i,i,k  ! open in Account order
		open #2: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx5.h[cno],Shr",i,i,k  ! open in route-sequence #
	!
	SCREEN1: !
		a$="" : prtbkno=0
		fnTos(sn$="UBPrtBl1-1")
		pf=27 : ll=25
		respc=0
		fnLbl(3,1,"Penalty Due Date:",ll,1)
		fnTxt(3,pf,8,8,1,"1",0,tt$)
		resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d4)
		fnLbl(4,1,"Message on Bill:",ll,1)
		fnTxt(4,pf,30,30)
		resp$(respc+=1)=mg$(1)
		fnTxt(5,pf,30,30)
		resp$(respc+=1)=mg$(2)
		fnTxt(6,pf,30,30)
		resp$(respc+=1)=mg$(3)
		fnLbl(7,1,"Date of Billing:",ll,1)
		fnTxt(7,pf,8,8,1,"1")
		resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d1)
		fnLbl(8,1,"Starting Route/Sequence:",ll,1)
		fe$="ubm-act-nam"
		datafile$="[Q]\UBmstr\Customer.h[cno]"
		indexfile$="[Q]\UBmstr\ubindx5.h[cno]"
		kp=1741: kl=9 : dp=41 : dl=30
		fncombof(fe$,8,pf,40,datafile$,kp,kl,dp,dl,indexfile$,2)
		resp$(respc+=1)="[All]"
		fnLbl(9,1,"Route Number:",ll,1)
		fncmbrt2(9,pf)
		resp$(respc+=1)="[All]"
		fnChk(10,pf,"Select Accounts to Print",1)
		resp$(respc+=1)='False'
		fnLbl(12,1,"Service From Date:",ll,1)
		fnTxt(12,pf,8,8,1,"1")
		resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d2_override)
		fnLbl(13,1,"Service To Date:",ll,1)
		fnTxt(13,pf,8,8,1,"1")
		resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d3_override)
		fnCmdSet(3)
		ckey=fnAcs(mat resp$)
		if ckey=5 then goto ENDSCR
		d1 = val(resp$(5))
		d4 = val(resp$(1))
		mg$(1) = resp$(2)
		mg$(2) = resp$(3)
		mg$(3) = resp$(4)
		d2_override=val(resp$(9))
		d3_override=val(resp$(10))
		if resp$(6)="[All]" then a$="" else a$ = lpad$(trim$(resp$(6)(1:9)),9)
		if resp$(7)="[All]" then prtbkno=0 else prtbkno = val(resp$(7))
		if resp$(8)='True' then sl1=1: z$="" else sl1=0
		if trim$(a$)<>"" then read #2,using 'form pos 1,c 10,pos 1741,n 2,n 7',key=a$: z$,route,sequence nokey SCREEN1
		holdz$=z$: begin=1
		st1=1
		if trim$(a$)="" and prtbkno=0 then restore #2,key>="         ": ! if no beginning account or starting route #, start at beginning of file
		if trim$(a$)<>"" then restore #2,key=cnvrt$("pic(zz)",route)& cnvrt$("pic(zzzzzzz)",sequence): nokey SCREEN1
		if trim$(a$)="" and prtbkno>0 then restore #2,key>=cnvrt$("pic(zz)",prtbkno)&"       ": ! selected a route and no beginning Account
	!
		open #3: "Name=[Q]\UBmstr\UBAdrBil.h[cno],KFName=[Q]\UBmstr\adrIndex.h[cno],Shr",i,i,k
		gosub BUD1
		gosub VBOPENPRINT
	!
		on fkey 5 goto RELEASE_PRINT
	L560: if sl1=1 then goto SCREEN3
	L570: read #6,using L600: z$ eof RELEASE_PRINT
		if trim$(a$)<>"" and begin=1 and z$<>holdz$ then goto L570 ! start with
		begin=0 ! cancel starting account
	L600: form pos 22,c 10
		read #1,using L620,key=z$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate,energy$,mat a,extra11,extra12 nokey L570
	L620: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1750,2*n 6,pos 1942,c 12,pos 1864,c 30,pos 1831,n 9,pos 1954,c 12,pos 143,7*pd 2,pos 1806,2*n 2
		if prtbkno=0 then goto L650
		if prtbkno><route then goto RELEASE_PRINT
	L650: if f><d1 then goto L560
	L660: gosub BUD2 ! determine if budget customer
		gas=0
		energy=0: energy=val(energy$) conv L690
	L690: if st1=0 then goto READALTADR
	! If ST1$=Z$ Then sT1=0 Else Goto 560
	READALTADR: !
	! read alternate billing address
		read #3,using L740,key=z$: mat ba$ nokey L810
	L740: form pos 11,4*c 30
		e1=0 : mat pe$=("")
		for j=1 to 4
			if rtrm$(ba$(j))<>"" then
				e1=e1+1 : pe$(e1)=ba$(j)
				end if
		next j
		goto L960
	!
	L810: e1=0 : mat pe$=("")
		for j=2 to 4
			if rtrm$(e$(j))<>"" then e1=e1+1 : pe$(e1)=e$(j)
		next j
		if trim$(extra1$)<>"" then pe$(4)=pe$(3): pe$(3)=extra1$ ! set third address line to extra1$ (2nd address)
		goto L960
	!
	RELEASE_PRINT: !
		close #1: ioerr L900
	L900: close #3: ioerr L910
	L910: !
		fnpa_finis
		goto ENDSCR
	!
	L960: !
		pb=bal-g(11)
		if bal<=0 then g(5)=g(6)=g(7)=0 ! don't show penalty if balance 0 or less
		if d2_override<>0 then d2=d2_override
		if d3_override<>0 then d3=d3_override
	! print bill routine
		gosub VBPRINT
	! end of pr routine
		bct(2)=bct(2)+1
		! accumulate totals
		goto L560
	!
	SCREEN3: !
		sn$ = "UBPrtBl1-2"
		fnTos(sn$)
		txt$="Account (blank to stop)"
		fnLbl(1,1,txt$,31,1)
	! If TRIM$(A$)="" Then Goto 1030 Else Goto 1040 ! kj 7/12/05
		if trim$(z$)<>"" then
			txt$="Last Account entered was "&z$
			fnLbl(3,1,txt$,44,1)
		else
			txt$=""
			fnLbl(3,1,txt$,44,1)
		end if
		fncmbact(1,17) !
		resp$(1)=a$
		fnCmdSet(3): ckey=fnAcs(mat resp$)
		a$ = lpad$(trim$(resp$(1)(1:10)),10)
		if trim$(a$)="" then goto RELEASE_PRINT
		if ckey=5 then goto RELEASE_PRINT
		read #1,using L620,key=a$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate,energy$,mat a,extra11,extra12 nokey SCREEN3
		goto L660
	!
	SORT1: ! SELECT & SORT
		open #5: "Name=[Q]\UBmstr\Cass1.h[cno],KFName=[Q]\UBmstr\Cass1Idx.h[cno],Shr",i,i,k ioerr L1430
		open #6: "Name=[Temp]\Temp.[Session],Replace,RecL=19",internal,output
		s5=1
		if prtbkno=0 then routekey$="" else routekey$=cnvrt$("N 2",prtbkno)&"       "
			! key off first record in route (route # no longer part of customer #)
		restore #2,search>=routekey$:
	L1230: read #2,using L1240: z$,f,route eof END5
	L1240: form pos 1,c 10,pos 296,pd 4,pos 1741
		if prtbkno=0 then goto L1270
		if prtbkno><route then goto END5
	L1270: if f><d1 then goto L1230
		zip5$=cr$=""
		read #5,using "form pos 96,C 5,pos 108,C 4",key=z$: zip5$,cr$ nokey L1300
	L1300: write #6,using "form pos 1,C 5,C 4,C 10": zip5$,cr$,z$
		goto L1230
	!
	END5: close #6:
		open #9: "Name=[Temp]\Control.[Session],Size=0,RecL=128,Replace",internal,output
	L1350: form pos 1,c 128
		write #9,using L1350: "File [Temp]\Temp.[Session],,,[Temp]\Addr.[Session],,,,,A,N"
		write #9,using L1350: "Mask 1,19,C,A"
		close #9:
		execute "Free [Temp]\Addr.[Session] -n" ioerr L1400
	L1400: execute "Sort [Temp]\Control.[Session] -n"
		open #6: "Name=[Temp]\Temp."&session$,i,i,r
		open #7: "Name=[Temp]\Addr."&session$,i,i,r
	L1430: return
	!
	ENDSCR: ! pr totals screen
		if sum(bct)=0 then pct=0 else pct=bct(2)/sum(bct)*100
		fnTos(sn$="Bills-Total")
		mylen=23 : mypos=mylen+2
		respc=0
		fnLbl(1,1,"Total Bills Printed:",mylen,1)
		fnTxt(1,mypos,8,0,1,"",1)
		resp$(respc+=1)=cnvrt$("N 8",sum(bct))
		fnCmdSet(52)
		ckey=fnAcs(mat resp$)
	Xit: fnXit
	!

	VBOPENPRINT: !
		fnPa_open("Landscape")
		lyne=3
		return
	!
	VBPRINT: !
	! -- Standard 4 Per Page Even Perferated Card Stock Bills
		checkcounter+=1
		if checkcounter=1 then xmargin=0 : ymargin=0
		if checkcounter=2 then xmargin=139 : ymargin=0
		if checkcounter=3 then xmargin=0 : ymargin=108
		if checkcounter=4 then
			xmargin=139
			ymargin=108
			checkcounter=0
		end if
	!
		pr #20: 'Call Print.AddLine('&str$(xmargin+5)&','&str$(ymargin+2)&',57,'&str$(lyne*3+3)&',True)'
		pr #20: "Call Print.MyFontBold(True)"
		pr #20: 'Call Print.MyFontSize(12)'
		pr #20: 'Call Print.MyFont("Courier New")'
		fnpa_txt(at$(1),xmargin+8,lyne*1-1+ymargin) ! pr #20: 'Call Print.AddText("'&at$(1)&'",'&str$(xmargin+8)&','&str$(lyne*1-1+ymargin)&')'
		pr #20: 'Call Print.MyFont("Lucida Console")'
		pr #20: 'Call Print.MyFontSize(10)'
		pr #20: 'Call Print.MyFontBold(False)'
		fnpa_txt(at$(2),xmargin+6,lyne*2+1+ymargin-.2) !  pr #20: 'Call Print.AddText("'&at$(2)&'",'&str$(xmargin+6)&','&str$(lyne*2+1+ymargin-.2)&')'
		fnpa_txt(at$(3),xmargin+6,lyne*3+1+ymargin) ! pr #20: 'Call Print.AddText("'&at$(3)&'",'&str$(xmargin+6)&','&str$(lyne*3+1+ymargin)&')'
		pr #20: 'Call Print.AddText("#'&trim$(z$)&'  '&bulk$&'",'&str$(xmargin+4)&','&str$(lyne*5+ymargin)&')'
		pr #20: 'Call Print.AddText("'&e$(1)&'",'&str$(xmargin+4)&','&str$(lyne*6+ymargin)&')'
		pr #20: 'Call Print.AddText("From: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d2)&'  To: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d3)&'",'&str$(xmargin+2)&','&str$(lyne*7+ymargin)&')'
	! pr #20: 'Call Print.AddText("Is due now and payable.",'&STR$(XMARGIN+2)&','&STR$(LYNE*8+YMARGIN)&')'
		pr #20: 'Call Print.AddText("Billing Date: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d1)&'",'&str$(xmargin+2)&','&str$(lyne*11+ymargin)&')'
		pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*12+1+ymargin)&',62,0)'
		pr #20: 'Call Print.AddText("Reading",'&str$(xmargin+10)&','&str$(lyne*13+ymargin)&')'
		pr #20: 'Call Print.AddText("Usage",'&str$(xmargin+33)&','&str$(lyne*13+ymargin)&')'
		pr #20: 'Call Print.AddText("Charge",'&str$(xmargin+50)&','&str$(lyne*13+ymargin)&')'
	!
	PRINTGRID: meter=14
		pr #20: 'Call Print.MyFontSize(8)'
		if havebudget=1 then payby=bal-gb(4)+budgetpb
		if havebudget=1 then gas=ba(5) else gas=g(4)
		if havebudget=1 then currentcharges=g(1)+g(2)+g(3)+gas+g(8)+g(9)
		if havebudget=0 then currentcharges=g(1)+g(2)+g(3)+g(4)+g(8)+g(9)
		if havebudget=1 then pb=payby-currentcharges
		if pb=0 then
				goto L2110
		else
			pr #20: 'Call Print.AddText("Previous Balance",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
			pr #20: 'Call Print.AddText("'&fnformnumb$(pb,2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
		end if
	L2110: if g(1)=0 then
					goto L2120
			else
				pr #20: 'Call Print.AddText("WTR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
				pr #20: 'Call Print.AddText("'&fnformnumb$(d(1),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')'
				pr #20: 'Call Print.AddText("'&fnformnumb$(d(3),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')'
				pr #20: 'Call Print.AddText("'&fnformnumb$(g(1),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
			end if
	L2120: if g(2)=0 then
				goto L2130
			else
				pr #20: 'Call Print.AddText("SWR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
				pr #20: 'Call Print.AddText("'&fnformnumb$(d(3),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')'
				pr #20: 'Call Print.AddText("'&fnformnumb$(g(2),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
			end if
	L2130: if g(3)=0 and d(7)=0 then
		goto L2150
	else
			pr #20: 'Call Print.AddText("Pool R",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
			! pr #20: 'Call Print.AddText("'&FNFORMNUMB$(D(5),0,9)&'",'&STR$(XMARGIN+6)&','&STR$(LYNE*METER+YMARGIN)&')'
			pr #20: 'Call Print.AddText("'&fnformnumb$(d(7),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')'
		end if
	!  pr #20: 'Call Print.AddText("'&FNFORMNUMB$(G(3),2,9)&'",'&STR$(XMARGIN+45)&','&STR$(LYNE*METER+YMARGIN)&')'
		L2150: !
		if a4=1 then
			gcode$="RSGS"
		else
			if a4=2 then
				gcode$="CMGS"
			else
				if a4=3 then
						gcode$="INGS"
				else
					gcode$="GAS"
				end if
			end if
		end if
		if g(4)=0 then
			goto L2230
		else
			pr #20: 'Call Print.AddText("'&gcode$&'",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
			pr #20: 'Call Print.AddText("'&fnformnumb$(d(9),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')'
			pr #20: 'Call Print.AddText("'&fnformnumb$(d(11),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')'
		end if
		if havebudget=0 then
			pr #20: 'Call Print.AddText("'&fnformnumb$(g(4),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
		else
			pr #20: 'Call Print.AddText("'&fnformnumb$(ba(5),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
		end if
		if havebudget=1 then pr #20: 'Call Print.AddText("Actual Gas Charge: '&trim$(cnvrt$("pic($$$,$$$.##",g(4)))&'",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
		if havebudget=1 and gb(4)>=0 then pr #20: 'Call Print.AddText("Level billing behind '&trim$(cnvrt$("pic($$$,$$$.##",abs(gb(4)-g(4))))&'",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
		if havebudget=1 and gb(4)<0 then pr #20: 'Call Print.AddText("Level billing ahead '&trim$(cnvrt$("pic($$$,$$$.##",abs(gb(4)-g(4))))&'",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
		if havebudget=1 and gb(4)<>0 then pr #20: 'Call Print.AddText("before paying this bill. ",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
		if havebudget=1 and energy<>0 then pr #20: 'Call Print.AddText("Less CEFS Applied:             '&trim$(cnvrt$("pic(zzz,zzz.##",energy))&'",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
	! IF HAVEBUDGET=1 THEN bAL=G(1)+G(2)+G(3)+GAS+G(8)+G(9)+BUDGETPB ! change balance on bill if they have a budget (show as current months charges plus andy prior budget amounts not paid
	L2230: if g(8)=0 then
		goto L2240
		else
			pr #20: 'Call Print.AddText("MISC",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
			pr #20: 'Call Print.AddText("'&fnformnumb$(g(8),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
		end if
	L2240: if g(9)=0 then
			goto L2250
		else
			pr #20: 'Call Print.AddText("TRA",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
			pr #20: 'Call Print.AddText("'&fnformnumb$(g(9),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
		end if
		L2250: ! If PB><0 Then pr #20: 'Call Print.AddLine('&STR$(XMARGIN+46)&','&STR$(LYNE*(METER+=1)+YMARGIN)&',15,0)'
		pr #20: 'Call Print.MyFontSize(10)'
	!
		if estimatedate=d1 then pr #20: 'Call Print.AddText("Bill estimated!",'&str$(xmargin+1)&','&str$(lyne*21+ymargin)&')'
		pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*24+1+ymargin)&',63,0)'
		pr #20: 'Call Print.AddText("   Pay By  '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+1)&','&str$(lyne*25+ymargin)&')'
		if havebudget=1 then pr #20: 'Call Print.AddText("'&fnformnumb$(payby,2,9)&'",'&str$(xmargin+42)&','&str$(lyne*25+ymargin)&')': goto L2350
		pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+42)&','&str$(lyne*25+ymargin)&')'
	L2350: pr #20: 'Call Print.AddText("Pay After  '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+1)&','&str$(lyne*26+ymargin)&')'
		if havebudget=1 and payby<=0 then pr #20: 'Call Print.AddText("'&fnformnumb$(payby,2,9)&'",'&str$(xmargin+42)&','&str$(lyne*26+ymargin)&')' : goto L2420 ! owe current gas budget plus other services Plus any old budgets not paid
		if havebudget=1 and payby>0 then pr #20: 'Call Print.AddText("'&fnformnumb$((payby)+round((currentcharges-gas+g(4))*penalty_rate,2),2,9)&'",'&str$(xmargin+42)&','&str$(lyne*26+ymargin)&')' : goto L2420 ! owe current gas budget plus other services Plus any old budgets not paid
		if bal<=0 then pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+42)&','&str$(lyne*26+ymargin)&')' : goto L2420
		if a(5)=0 and extra11=0 and extra12=0 then pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+42)&','&str$(lyne*26+ymargin)&')' : goto L2420
		if havebudget=0 then pr #20: 'Call Print.AddText("'&fnformnumb$(bal+max(0,round(currentcharges*penalty_rate,2)),2,9)&'",'&str$(xmargin+42)&','&str$(lyne*26+ymargin)&')'
		!                                                      fnformnumb$(bal+round(currentcharges*penalty_rate,2),2,9)
		if havebudget=1 and bal>0 and g(7)>0 then pr #20: 'Call Print.AddText("'&fnformnumb$((currentcharges+budgetpb)+round((currentcharges-gas+g(4))*penalty_rate,2),2,9)&'",'&str$(xmargin+42)&','&str$(lyne*26+ymargin)&')'
		if havebudget=1 and bal>0 and g(7)=0 then pr #20: 'Call Print.AddText("'&fnformnumb$((currentcharges+budgetpb),2,9)&'",'&str$(xmargin+42)&','&str$(lyne*26+ymargin)&')'
	L2420: pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*27+1+ymargin)&',63,0)'
		pr #20: 'Call Print.AddText("Office 756-8997 Fire 911",'&str$(xmargin+1)&','&str$(lyne*28+ymargin)&')'
		pr #20: 'Call Print.AddText("      Police 756-3311",'&str$(xmargin+1)&','&str$(lyne*29.5+ymargin)&')'
	!
		special=28
	!
		fnpa_fontsize(7)
		pr #20: 'Call Print.AddLine('&str$(xmargin+97)&','&str$(ymargin+0)&',29,'&str$(lyne*5+2)&',TRUE)'
		pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+0)&',7,0)'
		pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+2.8)&',7,0)'
		pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+5.6)&',7,0)'
		pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+8.4)&',7,0)'
		pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+11.2)&',7,0)'
		pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+14)&',7,0)'
		pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+17)&',7,0)'
		pr #20: 'Call Print.AddText("   Pre-Sorted",'&str$(xmargin+100)&','&str$(lyne*1-1+ymargin)&')'
		pr #20: 'Call Print.AddText("First Class Mail",'&str$(xmargin+100)&','&str$(lyne*2-1+ymargin)&')'
		pr #20: 'Call Print.AddText("  U.S. Postage  ",'&str$(xmargin+100)&','&str$(lyne*3-1+ymargin)&')'
		pr #20: 'Call Print.AddText("      Paid",'&str$(xmargin+100)&','&str$(lyne*4-1+ymargin)&')'
		pr #20: 'Call Print.AddText("  Permit No 1",'&str$(xmargin+100)&','&str$(lyne*5-1+ymargin)&')'
		fnpa_fontsize(9)
	! pr #20: 'Call Print.AddText("Address Service Requested",'&STR$(XMARGIN+68)&','&STR$(LYNE*7+YMARGIN-6)&')'
		pr #20: 'Call Print.AddText("Please return this",'&str$(xmargin+68)&','&str$(lyne*7+ymargin)&')'
		pr #20: 'Call Print.AddText("side with payment to:",'&str$(xmargin+68)&','&str$(lyne*8+ymargin)&')'
		fnpa_txt(env$('cnam'),xmargin+68,lyne*9+ymargin) ! pr #20: 'Call Print.AddText("'&env$('cnam')&'",'&str$(xmargin+68)&','&str$(lyne*9+ymargin)&')'
		fnpa_fontsize ! pr #20: 'Call Print.MyFontSize(10)'
		fnpa_txt('Pay By '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':',xmargin+68,lyne*11+ymargin) ! pr #20: 'Call Print.AddText("Pay By '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+68)&','&str$(lyne*11+ymargin)&')'
		if havebudget=1 then
			pr #20: 'Call Print.AddText("'&fnformnumb$(payby,2,9)&'",'&str$(xmargin+106)&','&str$(lyne*11+ymargin)&')'
		else
			pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+106)&','&str$(lyne*11+ymargin)&')'
		end if
		L2710: !
		pr #20: 'Call Print.AddText("After  '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+68)&','&str$(lyne*12+ymargin)&')'
		if havebudget=1 and payby=<0 then pr #20: 'Call Print.AddText("'&fnformnumb$(payby,2,9)&'",'&str$(xmargin+106)&','&str$(lyne*12+ymargin)&')': goto L2750
		if havebudget=1 and payby>0 and g(7)>0 then pr #20: 'Call Print.AddText("'&fnformnumb$((payby)+round((currentcharges-gas+g(4))*penalty_rate,2),2,9)&'",'&str$(xmargin+106)&','&str$(lyne*12+ymargin)&')' : goto L2750
		if currentcharges*penalty_rate>0 then pr #20: 'Call Print.AddText("'&fnformnumb$(bal+round(currentcharges*penalty_rate,2),2,9)&'",'&str$(xmargin+106)&','&str$(lyne*12+ymargin)&')'
	L2750: fnpa_fontsize(9)
		addy=14
		fnpa_txt(mg$(1),xmargin+68,(addy+=1)*lyne+ymargin)
		fnpa_txt(mg$(2),xmargin+68,(addy+=1)*lyne+ymargin)
		fnpa_txt(mg$(3),xmargin+68,(addy+=1)*lyne+ymargin)
		addy+=1
		pr #20: 'Call Print.MyFontSize(10)'
		if df$="Y" then pr #20: 'Call Print.AddText("Drafted",'&str$(xmargin+1)&','&str$(lyne*(addy+=1)+ymargin)
		if c4>0 then pr #20: 'Call Print.AddText("Final Bill",'&str$(xmargin+1)&','&str$(lyne*(addy+=1)+ymargin)
		pr #20: 'Call Print.AddText("#'&trim$(z$)&' '&bulk$&'",'&str$(xmargin+68)&','&str$(lyne*(addy+=1)+ymargin)&')'
		if pe$(1)<>"" then pr #20: 'Call Print.AddText("'&trim$(pe$(1))&'",'&str$(xmargin+68)&','&str$(lyne*(addy+=1)+ymargin)&')'
		if pe$(2)<>"" then pr #20: 'Call Print.AddText("'&trim$(pe$(2))&'",'&str$(xmargin+68)&','&str$(lyne*(addy+=1)+ymargin)&')'
		if pe$(3)<>"" then pr #20: 'Call Print.AddText("'&trim$(pe$(3))&'",'&str$(xmargin+68)&','&str$(lyne*(addy+=1)+ymargin)&')'
		if pe$(4)<>"" then pr #20: 'Call Print.AddText("'&trim$(pe$(4))&'",'&str$(xmargin+68)&','&str$(lyne*(addy+=1)+ymargin)&')'
		if checkcounter=1 then checkx=1.375 : checky=3.6875
		if checkcounter=2 then checkx=6.75 : checky=3.6875
		if checkcounter=3 then checkx=1.375 : checky=7.9375
		if checkcounter=0 then checkx=6.75 : checky=7.9375
		if checkcounter=0 then
			fnpa_newpage
		end if
		return
	!
	BULKSORT: ! bulk sort order
		open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",i,i,k  ! open in Account order
		open #6: "Name=[Temp]\Temp.[Session],Replace,RecL=31",internal,output
		do
			read #1,using "form pos 1,C 10,pos 1741,n 2,pos 1743,n 7,pos 1942,c 12": z$,route,seq,bulk$ eof L3040
			write #6,using "form pos 1,C 12,n 2,n 7,c 10": bulk$,route,seq,z$
		loop
	L3040: close #1: ioerr ignore
		close #6: ioerr ignore
		execute "Index [Temp]\Temp.[session] [Temp]\Tempidx.[session] 1,19,Replace,DupKeys -n" ioerr L3080
		open #6: "Name=[Temp]\Temp.[session],KFName=[Temp]\Tempidx.[session]",i,i,k
	L3080: return
	
	BUD1: bud1=0
		dim ba(13),badr(2),bt1(14,2),bd1(5),bd2(5),bd3(5)
		open #81: "Name=[Q]\UBmstr\BudMstr.h[cno],KFName=[Q]\UBmstr\BudIdx1.h[cno],Shr",internal,outIn,keyed ioerr EO_BUD1
		open #82: "Name=[Q]\UBmstr\BudTrans.h[cno],Shr",i,outi,r
		bud1=1
	EO_BUD1: return
	!
	BUD2: !
		totba=bd1=bd2=budgetpb=havebudget=00
		mat bd1(5) : mat bd1=(0) : mat bd2=(0)
		if bud1=0 then goto EO_BUD2
		read #81,using L3230,key=z$: z$,mat ba,mat badr nokey EO_BUD2
		havebudget=1
		for j=2 to 12: totba=totba+ba(j): next j
		L3230: form pos 1,c 10,pd 4,12*pd 5.2,2*pd 3
		if totba=0 then havebudget=0: goto EO_BUD2
		ta1=badr(1)
		L3260: if ta1=0 then goto EO_BUD2
		read #82,using L3280,rec=ta1: z$,mat bt1,nba noRec EO_BUD2
		L3280: form pos 1,c 10,2*pd 4,24*pd 5.2,2*pd 4,pd 3
		if bt1(14,1)>0 then goto L3340
		! IF BT1(1,2)=F THEN GOTO 3350 ! ignore current budget billing record
		budgetpb=budgetpb+bt1(5,1) ! add up prior balance for budget billing customers (any unpaid not counting current bill
		bd1=bd1+1
		if bd1>5 then goto EO_BUD2
		L3340: ta1=nba : goto L3260
		EO_BUD2: !
	return
include: ertn
