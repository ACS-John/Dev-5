! Replace S:\acsUB\ubprtbl1
! pr bills (new format)
! ______________________________________________________________________
	library 'S:\Core\Library': fnAcs,fnLbl,fnTxt,fnwait,fncmbrt2,fncombof,fnChk,fnerror,fnOpt,fnTos,fncmbact,fnLastBillingDate,fnxit,fnCmdSet,fntop,fnformnumb$,fnpause,fnpa_finis,fnpa_open,fnpa_txt,fnpa_newpage
	fntop("S:\acsUB\ubprtbl1",cap$="Print Bills")
	on error goto Ertn
! ______________________________________________________________________
	dim resp$(10)*80,txt$*40,mg$(3)*30,rw(22,13),cap$*128
	dim z$*10,e$(4)*30,f$*12,g(12),d(15),w$*31,y$*39,x$*70,b(11)
	dim gb(10),pe$(4)*30,ba$(4)*30,at$(3)*40
! ______________________________________________________________________
	fnLastBillingDate(d1)
	open #21: "Name=[Q]\UBmstr\Company.h[cno],Shr",internal,input 
	read #21,using "Form POS 41,2*C 40": at$(2),at$(3)
	close #21: 
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
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed  ! open in account order
	open #2: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx5.h[cno],Shr",internal,input,keyed  ! open in route-sequence
! ______________________________________________________________________
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
	resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d4)
	fnLbl(2,1,"Meter Read:",ll,1)
	fnTxt(2,pf,8,8,1,"1",0,tt$)
	resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d3)
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
	fncombof("ubm-act-nam",8,pf,40,"[Q]\UBmstr\Customer.h[cno]",1741,9,41,30,"[Q]\UBmstr\ubindx5.h[cno]",2)
	resp$(respc+=1)="[All]"
	fnLbl(9,1,"Route Number:",ll,1)
	fncmbrt2(9,pf)
	resp$(respc+=1)="[All]"
	fnChk(10,pf,"Select Accounts to Print",1)
	resp$(respc+=1)="False"
	fnCmdSet(3)
	fnAcs(sn$,0,mat resp$,ck)
	if ck=5 then goto ENDSCR
	d1=val(resp$(6))
	d3=val(resp$(2))
	d4=val(resp$(1))
	mg$(1)=resp$(3)
	mg$(2)=resp$(4)
	mg$(3)=resp$(5)
	if resp$(7)="[All]" then 
		a$=""
	else 
		a$=lpad$(trim$(resp$(7)(1:10)),10)
	end if 
	if resp$(8)="[All]" then 
		prtbkno=0
	else 
		prtbkno=val(resp$(8))
	end if 
	if resp$(9)="True" then sl1=1 else sl1=0
	if trim$(a$)<>"" then 
		read #1,using L480,key=a$: z$,route,sequence nokey SCREEN1
		st1=1
	end if 
L480: form pos 1,c 10,pos 1741,n 2,n 7
	if trim$(a$)="" and prtbkno=0 then restore #2,key>="         ": ! if no beginning account or starting route #, start at beginning of file
	if trim$(a$)<>"" then restore #2,key=cnvrt$("pic(zz)",route)& cnvrt$("pic(zzzzzzz)",sequence): nokey SCREEN1
	if trim$(a$)="" and prtbkno>0 then restore #2,key>=cnvrt$("pic(zz)",prtbkno)&"       ": ! selected a route and no beginning Account
	! ______________________________________________________________________
	open #3: "Name=[Q]\UBmstr\ubAdrBil.h[cno],KFName=[Q]\UBmstr\AdrIndex.h[cno],Shr",internal,input,keyed 
	fnPa_open("Landscape")
	! ______________________________________________________________________
	! IF SL1=0 THEN GOSUB SORT1
	L570: !
	if sl1=1 then goto SCREEN3
	if s5=0 then goto L640
	L590: read #7,using L600: r6 eof F5_CANCEL
	L600: form pos 1,pd 3
	read #6,using "Form POS 1,C 5,C 4,C 10",rec=r6: zip5$,cr$,z$ noRec L590
	read #1,using L650,key=z$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route nokey L590
	goto L650
	L640: read #2,using L650: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route eof F5_CANCEL
	L650: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2
	if prtbkno=0 then goto L680
	if prtbkno><route then goto F5_CANCEL
	L680: !
	if f><d1 then goto L570
	if st1=0 then goto HERE
	if st1$=z$ then st1=0 else goto L570
	HERE: ! 
! read alternate billing address
	read #3,using L740,key=z$: mat ba$ nokey L810
	L740: form pos 11,4*c 30
	e1=0 : mat pe$=("")
	for j=1 to 4
		if rtrm$(ba$(j))<>"" then 
			e1=e1+1
			pe$(e1)=ba$(j)
		end if 
	next j
goto L950
! ______________________________________________________________________
L810: e1=0 : mat pe$=("")
	for j=2 to 4
		if rtrm$(e$(j))<>"" then 
			e1=e1+1
			pe$(e1)=e$(j)
		end if 
	next j
goto L950
! ______________________________________________________________________
F5_CANCEL: ! 
	close #1: ioerr ignore
	close #3: ioerr ignore
	! 
	! close #20: ioerr ignore
	fnpa_finis
goto ENDSCR
! ______________________________________________________________________
L950: ! 
	pb=bal-g(11)
	! ______________print bill routine______________________________________
	fn_vbprint
	! _____________end of pr routine______________________________________
	bct(2)=bct(2)+1
	! accumulate totals
goto L570
! ______________________________________________________________________
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
	fnCmdSet(11): fnAcs(sn$,0,mat resp$,ck)
	if ck=5 then goto F5_CANCEL
	a$=lpad$(trim$(resp$(1)(1:10)),10)
	if trim$(a$)="" then goto F5_CANCEL
	read #1,using L650,key=a$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route nokey SCREEN3
	goto HERE
! ______________________________________________________________________
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
! ______________________________________________________________________
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
! ______________________________________________________________________
ENDSCR: ! pr totals screen
	if sum(bct)=0 then pct=0 else pct=bct(2)/sum(bct)*100
	fnTos(sn$="Bills-Total")
	mylen=23 : mypos=mylen+2
	respc=0
	fnLbl(1,1,"Total Bills Printed:",mylen,1)
	fnTxt(1,mypos,8,0,1,"",1)
	resp$(respc+=1)=cnvrt$("N 8",sum(bct))
	fnCmdSet(52)
	fnAcs(sn$,0,mat resp$,ck)
XIT: fnxit
! ______________________________________________________________________
ERTN: fnerror(program$,err,line,act$,"xit")
	if uprc$(act$)<>"PAUSE" then goto L1550
	execute "List -"&str$(line) : pause : goto L1550
	pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause 
L1550: execute act$
	goto ERTN
! ______________________________________________________________________
def fn_vbprint
	! -- Printer Program for New Laser Utility Bills
	checkcounter+=1
	if checkcounter=1 then xmargin=0 : ymargin=0
	if checkcounter=2 then xmargin=141.2 : ymargin=0
	if checkcounter=3 then xmargin=0 : ymargin=108
	if checkcounter=4 then xmargin=141.2 : ymargin=108 : checkcounter=0
	! ___________________________
	! - CONSTANTS
	lyne=3
	character=1.5
	pr #20: 'Call Print.MyOrientation("Landscape")'
	pr #20: 'Call Print.MyFontSize(12)'
	pr #20: 'Call Print.MyFontSize(10)'
	pr #20: 'Call Print.MyFontBold(False)'
	pr #20: 'Call Print.MyFontColor("Black")'
	fnpa_txt('#'&trim$(z$),xmargin,lyne*6+ymargin)
	fnpa_txt(e$(1),xmargin+26,lyne*6+ymargin)
	! pr #20: 'Call Print.AddText("Billing Date: ",'&str$(xmargin+2)&','&str$(lyne*11+ymargin)&')'
	! pr #20: 'Call Print.AddText("'&cnvrt$("PIC(ZZ/ZZ/ZZ)",d1)&'",'&str$(xmargin+30)&','&str$(lyne*11+ymargin)&')'
	! pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*12+1+ymargin)&','&str$(linelength)&',0)'
	! ___________________________
	PRINTGRID: meter=9
	pr #20: 'Call Print.MyFontSize(8)'
	! d(1)=123456789 : d(3)=123456789 : g(1)=123456.89 : g(2)=123456.89 : d(9)=123456789 : d(11)=123456789 : g(4)=123456.89 : g(5)=123456.89 : g(6)=123456.89 : g(8)=123456.89 : g(9)=123456.89 : pB=123456.89
	if g(1) then 
		pr #20: 'Call Print.AddText("WA",'&str$(xmargin)&','&str$(lyne*(meter+=1)+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(d(1),0,9)&'",'&str$(xmargin+1)&','&str$(lyne*meter+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(d(2),0,9)&'",'&str$(xmargin+18)&','&str$(lyne*meter+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(d(3),0,9)&'",'&str$(xmargin+35)&','&str$(lyne*meter+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(1),2,9)&'",'&str$(xmargin+52)&','&str$(lyne*meter+ymargin)&')'
	end if 
	if g(2) then 
		pr #20: 'Call Print.AddText("SW",'&str$(xmargin)&','&str$(lyne*(meter+=1)+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(2),2,9)&'",'&str$(xmargin+52)&','&str$(lyne*meter+ymargin)&')'
	end if 
	if g(4) then 
		pr #20: 'Call Print.AddText("PS",'&str$(xmargin)&','&str$(lyne*(meter+=1)+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(4),2,9)&'",'&str$(xmargin+52)&','&str$(lyne*meter+ymargin)&')'
	end if 
	if g(5) then 
		pr #20: 'Call Print.AddText("TR",'&str$(xmargin)&','&str$(lyne*(meter+=1)+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(5),2,9)&'",'&str$(xmargin+52)&','&str$(lyne*meter+ymargin)&')'
	end if 
	if g(6) then 
		pr #20: 'Call Print.AddText("PW",'&str$(xmargin)&','&str$(lyne*(meter+=1)+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(6),2,9)&'",'&str$(xmargin+52)&','&str$(lyne*meter+ymargin)&')'
	end if 
	if g(8) then 
		pr #20: 'Call Print.AddText("OC",'&str$(xmargin)&','&str$(lyne*(meter+=1)+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(8),2,9)&'",'&str$(xmargin+52)&','&str$(lyne*meter+ymargin)&')'
	end if 
	if g(9) then 
		pr #20: 'Call Print.AddText("TX",'&str$(xmargin)&','&str$(lyne*(meter+=1)+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(9),2,9)&'",'&str$(xmargin+52)&','&str$(lyne*meter+ymargin)&')'
	end if 
	if pb then 
		pr #20: 'Call Print.AddText("PB",'&str$(xmargin)&','&str$(lyne*(meter+=1)+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(pb,2,9)&'",'&str$(xmargin+52)&','&str$(lyne*meter+ymargin)&')'
	end if 
___________________________
	pr #20: 'Call Print.AddText("'&date$(days(d3,"mmddyy"),"m")&'",'&str$(xmargin)&','&str$(lyne*23+ymargin)&')'
	pr #20: 'Call Print.AddText("'&date$(days(d3,"mmddyy"),"D")&'",'&str$(xmargin+6)&','&str$(lyne*23+ymargin)&')'
	if bal>0 then 
		pr #20: 'Call Print.AddText("'&fnformnumb$(bal-g(9),2,9)&'",'&str$(xmargin+18)&','&str$(lyne*23+ymargin)&')'
		if g(10)>0 then pr #20: 'Call Print.AddText("'&fnformnumb$(g(10),2,9)&'",'&str$(xmargin+31)&','&str$(lyne*23+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(bal+g(10)-g(9),2,9)&'",'&str$(xmargin+52)&','&str$(lyne*23+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+18)&','&str$(lyne*29.2+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(bal+g(10),2,9)&'",'&str$(xmargin+52)&','&str$(lyne*29.2+ymargin)&')'
	else 
		pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+18)&','&str$(lyne*23+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+52)&','&str$(lyne*23+ymargin)&')'
	end if 
	if g(9)>0 and bal>0 then 
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(9),2,9)&'",'&str$(xmargin+18)&','&str$(lyne*25.4+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(9),2,9)&'",'&str$(xmargin+52)&','&str$(lyne*25.4+ymargin)&')'
	end if 
	if bal>0 then 
	end if 
	! pr #20: 'Call Print.AddText("Springfield",'&str$(xmargin+80)&','&str$(lyne*2-1+ymargin)&')'
	! pr #20: 'Call Print.AddText("     IL    ",'&str$(xmargin+80)&','&str$(lyne*3-1+ymargin)&')'
	! pr #20: 'Call Print.AddText("    62702  ",'&str$(xmargin+80)&','&str$(lyne*4-1+ymargin)&')'
	! pr #20: 'Call Print.AddLine('&str$(xmargin+97)&','&str$(ymargin+0)&',29,'&str$(lyne*4+2)&',True)'
	! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+0)&',7,0)'
	! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+2.8)&',7,0)'
	! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+5.6)&',7,0)'
	! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+8.4)&',7,0)'
	! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+11.2)&',7,0)'
	! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+14)&',7,0)'
	! pr #20: 'Call Print.AddText("First Class Mail",'&str$(xmargin+100)&','&str$(lyne*1-1+ymargin)&')'
	! pr #20: 'Call Print.AddText("  U.S. Postage  ",'&str$(xmargin+100)&','&str$(lyne*2-1+ymargin)&')'
	! pr #20: 'Call Print.AddText(" Paid One Ounce ",'&str$(xmargin+100)&','&str$(lyne*3-1+ymargin)&')'
	! pr #20: 'Call Print.AddText("  Permit No.916 ",'&str$(xmargin+100)&','&str$(lyne*4-1+ymargin)&')'
	pr #20: 'Call Print.MyFontSize(8)'
	pr #20: 'Call Print.AddText("Please return this side with",'&str$(xmargin+75)&','&str$(lyne*6+ymargin)&')'
	pr #20: 'Call Print.AddText("payment to:  '&env$('cnam')&'",'&str$(xmargin+75)&','&str$(lyne*7+ymargin)&')'
	addy=9
	fnpa_txt(e$(2),xmargin+75,lyne*(addy+=1)+ymargin)
	fnpa_txt(e$(3),xmargin+75,lyne*(addy+=1)+ymargin)
	fnpa_txt(e$(4),xmargin+75,lyne*(addy+=1)+ymargin)
	fnpa_txt(mg$(1),xmargin+75,lyne*(addy+=2)+ymargin)
	fnpa_txt(mg$(2),xmargin+75,lyne*(addy+=1)+ymargin)
	fnpa_txt(mg$(3),xmargin+75,lyne*(addy+=1)+ymargin)
	pr #20: 'Call Print.MyFontSize(9)'
	fnpa_txt(z$,xmargin+80,lyne*(addy+=5)+ymargin)
	fnpa_txt(cnvrt$("PIC(ZZ/ZZ/ZZ)",d4),xmargin+107,lyne*addy+ymargin)
	fnpa_txt(fnformnumb$(bal,2,9),xmargin+75,lyne*(addy+=8.5)+ymargin)
	if bal>0 then 
		fnpa_txt(fnformnumb$(bal+g(10),2,9),xmargin+106,lyne*addy+ymargin)
	else 
		fnpa_txt(fnformnumb$(bal,2,9),xmargin+106,lyne*addy+ymargin)
	end if 
	if checkcounter=0 then 
		fnpa_newpage
	end if 
fnend 
