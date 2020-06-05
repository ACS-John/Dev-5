! Replace S:\acsUB\ubprtbl1
! pr bills (new format)
 
	autoLibrary
	fnTop("S:\acsUB\ubprtbl1",cap$="Print Bills")
	on error goto Ertn
 
	dim resp$(11)*40,txt$*40,mg$(3)*30,rw(22,13),cap$*128
	dim z$*10,e$(4)*30,f$*12,g(12),d(15),w$*31,y$*39,x$*70,b(11)
	dim gb(10),pe$(4)*30,ba$(4)*30,at$(3)*40,cnam$*40
 
	fncno(cno,cnam$)
	fnLastBillingDate(d1)
	open #21: "Name=[Q]\UBmstr\Company.h[cno],Shr",internal,input
	read #21,using "Form POS 41,2*C 40": at$(2),at$(3)
	close #21:
	at$(1)=cnam$
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
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed  ! open in account order
	open #2: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx5.h[cno],Shr",internal,input,keyed  ! open in route-sequence
 
SCREEN1: !
	a$="" : prtbkno=0
	fnTos(sn$="UBPrtBl1-1")
	pf=26 : ll=24 : respc=0
! fnLbl(1,1,"Service From:",LL,1)
!  fnTxt(1,PF,8,8,1,"1",0,TT$)
!  rESP$(RESPC+=1)=CNVRT$("pic(zzzzzz)",D2)
! fnLbl(2,1,"Service To:",LL,1)
!  fnTxt(2,PF,8,8,1,"1")
!  rESP$(RESPC+=1)=CNVRT$("pic(zzzzzz)",D3)
	fnLbl(1,1,"Penalty Due Date:",ll,1)
	fnTxt(1,pf,8,8,1,"1",0,tt$)
	resp$(d4_i=respc+=1)=cnvrt$("pic(zzzzzz)",d4)
	fnLbl(2,1,"Meter Reading Date:",ll,1)
	fnTxt(2,pf,8,8,1,"1",0,tt$)
	resp$(d5_i=respc+=1)=cnvrt$("pic(zzzzzz)",d5)
	fnLbl(4,1,"Message on Bill:",ll,1)
	fnTxt(4,pf,30,30)
	resp$(mg1_i=respc+=1)=mg$(1)
	fnTxt(5,pf,30,30)
	resp$(mg2_i=respc+=1)=mg$(2)
	fnTxt(6,pf,30,30)
	resp$(mg3_i=respc+=1)=mg$(3)
	fnLbl(7,1,"Date of Billing:",ll,1)
	fnTxt(7,pf,8,8,1,"1")
	resp$(d1_i=respc+=1)=cnvrt$("pic(zzzzzz)",d1)
	fnLbl(8,1,"Starting Account:",ll,1)
	fncombof("ubm-act-nam",8,pf,40,"[Q]\UBmstr\Customer.h[cno]",1,10,41,30,"[Q]\UBmstr\ubindx5.h[cno]",2)
	resp$(acct_i=respc+=1)="[All]"
	fnLbl(9,1,"Route Number:",ll,1)
	fncmbrt2(9,pf)
	resp$(rt_i=respc+=1)="[All]"
	fnChk(10,pf,"Select Accounts to Print",1)
	resp$(selacct_i=respc+=1)="False"
	fnCmdSet(3)
	fnAcs2(mat resp$,ckey)
	if ckey=5 then goto ENDSCR
	d1=val(resp$(d1_i))
	d4=val(resp$(d4_i))
	d5=val(resp$(d5_i))
	mg$(1)=resp$(mg1_i)
	mg$(2)=resp$(mg2_i)
	mg$(3)=resp$(mg3_i)
	if resp$(acct_i)="[All]" then
		a$=""
	else
		a$=lpad$(trim$(resp$(acct_i)(1:10)),10)
		st1$=a$
	end if
	if resp$(rt_i)="[All]" then
		prtbkno=0
	else
		prtbkno=val(resp$(rt_i))
	end if
	if resp$(selacct_i)="True" then sl1=1 else sl1=0
	if trim$(a$)<>"" then
		read #1,using L480,key=a$: z$,route,sequence nokey SCREEN1
		st1=1
	end if
L480: form pos 1,c 10,pos 1741,n 2,n 7
	if trim$(a$)="" and prtbkno=0 then restore #2,key>="         ": ! if no beginning account or starting route #, start at beginning of file
	if trim$(a$)<>"" then restore #2,key=cnvrt$("pic(zz)",route)& cnvrt$("pic(zzzzzzz)",sequence): nokey SCREEN1
	if trim$(a$)="" and prtbkno>0 then restore #2,key>=cnvrt$("pic(zz)",prtbkno)&"       ": ! selected a route and no beginning Account
 
	open #3: "Name=[Q]\UBmstr\ubAdrBil.h[cno],KFName=[Q]\UBmstr\AdrIndex.h[cno],Shr",internal,input,keyed
! fnOPENPRN
 
! IF SL1=0 THEN GOSUB SORT1
L570: if sl1=1 then goto SCREEN3
	if s5=0 then goto L640
L590: read #7,using L600: r6 eof F5_CANCEL
L600: form pos 1,pd 3
	read #6,using "Form POS 1,C 5,C 4,C 10",rec=r6: zip5$,cr$,z$ noRec L590
	read #1,using L650,key=z$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,estimatedate nokey L590
	goto L650
L640: read #2,using L650: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,estimatedate eof F5_CANCEL
L650: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1831,n 9
	if prtbkno=0 then goto L680
	if prtbkno><route then goto F5_CANCEL
L680: if f><d1 then goto L570
	if st1=0 then goto HERE
	if st1$=z$ then st1=0 else goto L570
HERE: !
! read alternate billing address
	mat ba$=("") : read #3,using L740,key=z$: mat ba$ nokey L810
L740: form pos 11,4*c 30
 
L810: e1=0 : mat pe$=("")
	for j=2 to 4
		if rtrm$(e$(j))<>"" then
			e1=e1+1
			pe$(e1)=e$(j)
		end if
	next j
	goto L950
 
F5_CANCEL: !
	close #1: ioerr L890
L890: close #3: ioerr L900
L900: !
L920: fnpa_finis
	goto ENDSCR
 
L950: !
	pb=bal-g(11)
! ______________print bill routine______________________________________
	fn_vbprint
! _____________end of pr routine______________________________________
	bct(2)=bct(2)+1
! accumulate totals
	goto L570
 
SCREEN3: !
	sn$="UBPrtBl1-2"
	fnTos(sn$)
	txt$="Account (blank to stop)"
	fnLbl(1,1,txt$,31,1)
	if trim$(a$)="" then goto L1070 else goto L1080
L1070: if z$<>"" then
		txt$="Last Account entered was "&z$
		fnLbl(3,1,txt$,44,1)
	else
		txt$=""
		fnLbl(3,1,txt$,44,1)
	end if
L1080: fncmbact(1,17) !
	resp$(1)=a$
	fnCmdSet(11): fnAcs2(mat resp$,ckey)
	if ckey=5 then goto F5_CANCEL
	a$=lpad$(trim$(resp$(1)(1:10)),10)
	if trim$(a$)="" then goto F5_CANCEL
	read #1,using L650,key=a$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,estimatedate nokey SCREEN3
	goto HERE
 
SORT1: ! SELECT & SORT
	open #5: "Name=[Q]\UBmstr\Cass1.h[cno],KFName=[Q]\UBmstr\Cass1Idx.h[cno],Shr",internal,input,keyed ioerr L1410
	open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",Replace,RecL=19",internal,output
	s5=1
	if prtbkno=0 then routekey$="" else routekey$=cnvrt$("N 2",prtbkno)&"       " ! key off first record in route (route # no longer part of customer #)
	restore #2,search>=routekey$:
L1210: read #2,using L1220: z$,f,route eof END5
L1220: form pos 1,c 10,pos 296,pd 4,pos 1741
	if prtbkno=0 then goto L1250
	if prtbkno><route then goto END5
L1250: if f><d1 then goto L1210
	zip5$=cr$=""
	read #5,using "Form POS 96,C 5,POS 108,C 4",key=z$: zip5$,cr$ nokey L1280
L1280: write #6,using "Form POS 1,C 5,C 4,C 10": zip5$,cr$,z$
	goto L1210
 
END5: close #6:
	open #9: "Name="&env$('Temp')&"\Control."&session$&",Size=0,RecL=128,Replace",internal,output
L1330: form pos 1,c 128
	write #9,using L1330: "File "&env$('Temp')&"\Temp."&wsid$&",,,"&env$('Temp')&"\Addr."&session$&",,,,,A,N"
	write #9,using L1330: "Mask 1,19,C,A"
	close #9:
	execute "Free "&env$('Temp')&"\Addr."&session$ ioerr L1380
L1380: execute "Sort "&env$('Temp')&"\Control."&session$
	open #6: "Name="&env$('Temp')&"\Temp."&wsid$,internal,input,relative
	open #7: "Name="&env$('Temp')&"\Addr."&session$,internal,input,relative
L1410: return
 
ENDSCR: ! pr totals screen
	if sum(bct)=0 then pct=0 else pct=bct(2)/sum(bct)*100
	fnTos(sn$="Bills-Total")
	mylen=23 : mypos=mylen+2
	respc=0
	fnLbl(1,1,"Total Bills Printed:",mylen,1)
	fnTxt(1,mypos,8,0,1,"",1)
	resp$(respc+=1)=cnvrt$("N 8",sum(bct))
	fnCmdSet(52)
	fnAcs2(mat resp$,ckey)
Xit: fnXit
 
ERTN: fnerror(program$,err,line,act$,"Xit")
	if uprc$(act$)<>"PAUSE" then goto L1550
	execute "List -"&str$(line) : pause : goto L1550
	pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause
L1550: execute act$
	goto ERTN
 
def fn_vbprint
		if file(20)=-1 then
			fnPa_open("Landscape")
			lyne=3
		end if
! -- Standard 4 Per Page Even Perferated Card Stock Bills
		billcounter+=1
		if billcounter=1 then xmargin=1 : ymargin=10
		if billcounter=2 then xmargin=140 : ymargin=10
		if billcounter=3 then xmargin=1 : ymargin=118
		if billcounter=4 then xmargin=140 : ymargin=118 : billcounter=0
 
! pr #20: 'Call Print.AddLine('&str$(xmargin+5)&','&str$(ymargin+2)&',57,'&str$(lyne*3+3)&',True)'
		if reading_date_cur_s1=0 then reading_date_cur=d3 else reading_date_cur=reading_date_cur_s1
		if reading_date_prior_s1=0 then reading_date_prior=d2 else reading_date_prior=reading_date_prior_s1
! fnpa_txt('#'&trim$(z$),xmargin+4,lyne*5+ymargin)
! pr #20: 'Call Print.AddText("'&e$(1)&'",'&str$(xmargin+4)&','&str$(lyne*6+ymargin)&')'
! pr #20: 'Call Print.AddText("From: '&cnvrt$("PIC(ZZ/ZZ)",int(reading_date_prior/100))&'  To: '&cnvrt$("PIC(ZZ/ZZ)",int(reading_date_cur/100))&'",'&str$(xmargin+2)&','&str$(lyne*7+ymargin)&')'
! pr #20: 'Call Print.AddText("Is due now and payable.",'&str$(xmargin+2)&','&str$(lyne*8+ymargin)&')'
! pr #20: 'Call Print.AddText("Billing Date: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d1)&'",'&str$(xmargin+2)&','&str$(lyne*11+ymargin)&')'
! pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*12+1+ymargin)&',62,0)'
! fnpa_line(xmargin+1,lyne*12+1+ymargin,62,0)
! pr #20: 'Call Print.AddText("Reading",'&str$(xmargin+10)&','&str$(lyne*13+ymargin)&')'
! pr #20: 'Call Print.AddText("Usage",'&str$(xmargin+33)&','&str$(lyne*13+ymargin)&')'
! pr #20: 'Call Print.AddText("Charge",'&str$(xmargin+50)&','&str$(lyne*13+ymargin)&')'
 
PRINTGRID: !
		meter=3
		pr #20: 'Call Print.MyFontSize(8)'
		pr #20: 'Call Print.AddText("Reading Date",'&str$(xmargin+1)&','&str$(-1*lyne+ymargin)&')'
		pr #20: 'Call Print.AddText("'&trim$(cnvrt$("PIC(ZZ/ZZ/ZZ",d5))&'",'&str$(xmargin+1)&','&str$(ymargin)&')'
		pr #20: 'Call Print.AddText("'&cnvrt$("PIC(ZZ/ZZ/ZZ",d1)&'",'&str$(xmargin+48)&','&str$(ymargin)&')'
		if g(1)<>0 then
			pr #20: 'Call Print.AddText("WTR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
			pr #20: 'Call Print.AddText("'&fnformnumb$(d(1),0,9)&'",'&str$(xmargin+8)&','&str$(lyne*meter+ymargin)&')'
			pr #20: 'Call Print.AddText("'&fnformnumb$(d(3),0,9)&'",'&str$(xmargin+27)&','&str$(lyne*meter+ymargin)&')'
			pr #20: 'Call Print.AddText("'&fnformnumb$(g(1),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
		end if
		if g(2)<>0 then
			pr #20: 'Call Print.AddText("SWR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
			pr #20: 'Call Print.AddText("'&fnformnumb$(g(2),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
		end if
		if g(3)<>0 then
			pr #20: 'Call Print.AddText("Admin",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
			pr #20: 'Call Print.AddText("'&fnformnumb$(g(3),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
		end if
		if a4=1 then
			gcode$="RSGS"
		else if a4=2 then
			gcode$="CMGS"
		else if a4=3 then
			gcode$="INGS"
		else
			gcode$="GAS"
		end if
		if g(4)<>0 then
			pr #20: 'Call Print.AddText("'&gcode$&'",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
			pr #20: 'Call Print.AddText("'&fnformnumb$(d(9),0,9)&'",'&str$(xmargin+8)&','&str$(lyne*meter+ymargin)&')'
			pr #20: 'Call Print.AddText("'&fnformnumb$(d(11),0,9)&'",'&str$(xmargin+27)&','&str$(lyne*meter+ymargin)&')'
			pr #20: 'Call Print.AddText("'&fnformnumb$(g(4),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
		end if
		if g(5)<>0 or g(6)<>0 or g(7)<>0 then
! pr #20: 'Call Print.AddText("PEN",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
! pr #20: 'Call Print.AddText("'&fnformnumb$(g(5)+g(6)+g(7),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
		end if
! if g(6)<>0 then
!  pr #20: 'Call Print.AddText("FUR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
!  pr #20: 'Call Print.AddText("'&fnformnumb$(g(6),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
! . end if
		if g(10)<>0 then
! pr #20: 'Call Print.AddText("Penalty",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
! pr #20: 'Call Print.AddText("'&fnformnumb$(g(10),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
		end if
		if g(8)<>0 then
			pr #20: 'Call Print.AddText("MISC",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
			pr #20: 'Call Print.AddText("'&fnformnumb$(g(8),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
		end if
		if g(9)<>0 then
			pr #20: 'Call Print.AddText("TAX",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
			pr #20: 'Call Print.AddText("'&fnformnumb$(g(9),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
		end if
		if pb><0 then pr #20: 'Call Print.AddLine('&str$(xmargin+46)&','&str$(lyne*(meter+=1)+ymargin)&',15,0)'
		if pb><0 then
			pr #20: 'Call Print.AddText("   Subtotal",'&str$(xmargin+1)&','&str$(lyne*(meter+=.25)+ymargin)&')'
			pr #20: 'Call Print.AddText("'&fnformnumb$(g(1)+g(2)+g(3)+g(4)+g(5)+g(6)+g(7)+g(8)+g(9),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
		end if
		if pb<>0 then
			pr #20: 'Call Print.AddText("Previous Balance",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
			pr #20: 'Call Print.AddText("'&fnformnumb$(pb,2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
		end if
		if estimatedate=d1 then
			fnpa_txt("Bill estimated!",xmargin+1,11*lyne+ymargin)
			fnpa_txt(mg$(1),xmargin+1,12*lyne+ymargin)
			fnpa_txt(mg$(2),xmargin+1,13*lyne+ymargin)
		else
			fnpa_txt(mg$(1),xmargin+1,11*lyne+ymargin)
			fnpa_txt(mg$(2),xmargin+1,12*lyne+ymargin)
			fnpa_txt(mg$(3),xmargin+1,13*lyne+ymargin)
		end if
! If ESCROW>0 Then
! pr #20: 'Call Print.AddText("Escrow CR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
! pr #20: 'Call Print.AddText("'&fnformnumb$(escrow,2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'\
! end if
		pr #20: 'Call Print.MyFontSize(9)'
 
! if estimatedate=d1 then let fnpa_line("Bill estimated!",xmargin+1,lyne*29+ymargin)
! fnpa_line(xmargin+1,lyne*25+1+ymargin,63,0)
		if bal>0 then
			fnpa_txt(fnformnumb$(round(bal*1.1,2),2,8),xmargin-4,lyne*19.5+ymargin)
		else
			fnpa_txt(fnformnumb$(bal,2,8),xmargin-4,lyne*19.5+ymargin)
		end if
		fnpa_txt(cnvrt$("PIC(ZZ/ZZ/ZZ)",d4),xmargin+20,lyne*19.5+ymargin)
		fnpa_txt(fnformnumb$(bal,2,9),xmargin+39,lyne*19.5+ymargin)
		addy=23.6 ! 14
		fnpa_txt('#'&trim$(z$),xmargin+8,lyne*23.5+ymargin)
! if pe$(1)<>"" then let fnpa_txt(trim$(pe$(1)),xmargin+9,lyne*(addy+=1.1)+ymargin)
		if trim$(ba$(1))="" and trim$(ba$(2))="" and trim$(ba$(3))="" and trim$(ba$(4))="" then
			if trim$(pe$(2))<>"" then let fnpa_txt(trim$(pe$(2)),xmargin+8,lyne*(addy+=1.1)+ymargin)
			if trim$(pe$(3))<>"" then let fnpa_txt(trim$(pe$(3)),xmargin+8,lyne*(addy+=1.1)+ymargin)
			if trim$(pe$(4))<>"" then let fnpa_txt(trim$(pe$(4)),xmargin+8,lyne*(addy+=1.1)+ymargin)
		else
			if trim$(ba$(2))<>"" then let fnpa_txt(trim$(ba$(2)),xmargin+8,lyne*(addy+=1.1)+ymargin)
			if trim$(ba$(3))<>"" then let fnpa_txt(trim$(ba$(3)),xmargin+8,lyne*(addy+=1.1)+ymargin)
			if trim$(ba$(4))<>"" then let fnpa_txt(trim$(ba$(4)),xmargin+8,lyne*(addy+=1.1)+ymargin)
		end if
		fnpa_txt(trim$(e$(1)),xmargin+8,lyne*(addy+=1.5)+ymargin)
! fnpa_txt("  Office 217-628-3416",xmargin+1,lyne*28.5+ymargin)
 
		special=28
 
		pr #20: 'Call Print.MyFontSize(10)'
		if bal>0 then
! fnpa_txt('3Pay By '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':',csp-2,factor+line_height*11)
			fnpa_txt(fnformnumb$(round(bal*1.1,2),2,9),xmargin+68,lyne*7+ymargin)
		else
			fnpa_txt(fnformnumb$(bal,2,9),xmargin+68,lyne*7+ymargin)
		end if
		fnpa_txt(fnformnumb$(bal,2,9),xmargin+102,lyne*7+ymargin)
		pr #20: 'Call Print.MyFontSize(12)'
		addy=10
		fnpa_txt('#'&trim$(z$),xmargin+71,lyne*(addy+=1.3)+ymargin)
		if pe$(1)<>"" then let fnpa_txt(trim$(pe$(1)),xmargin+71,lyne*(addy+=1.5)+ymargin)
		if pe$(2)<>"" then let fnpa_txt(trim$(pe$(2)),xmargin+71,lyne*(addy+=1.3)+ymargin)
		if pe$(3)<>"" then let fnpa_txt(trim$(pe$(3)),xmargin+71,lyne*(addy+=1.3)+ymargin)
		if pe$(4)<>"" then let fnpa_txt(trim$(pe$(4)),xmargin+71,lyne*(addy+=1.3)+ymargin)
		pr #20: 'Call Print.MyFontSize(10)'
		if final>0 then let fnpa_txt("Final Bill",xmargin+75,lyne*(addy+5)+ymargin)
 
		if billcounter=0 then
			fnpa_newpage
		end if
fnend
 
