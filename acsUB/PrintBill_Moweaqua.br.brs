! Replace S:\acsUB\ubprtbl1_mow
! pr bills for Village of Moweaqua
 
	autoLibrary
	on error goto Ertn
 
	dim resp$(10)*40,txt$*45,mg$(3)*30,cap$*128
	dim z$*10,e$(4)*30,f$*12,g(12),d(15),b(11),extra1$*30
	dim gb(10),pe$(4)*30,ba$(4)*30,at$(3)*40
 
	fnLastBillingDate(d1)
	open #21: "Name=[Q]\UBmstr\Company.h[cno],Shr",i,i
	read #21,using "form pos 41,2*C 40": at$(2),at$(3)
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
! linelength=62
!
!
	fn_bulksort
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",i,i,k  ! open in Account order
	open #2: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx5.h[cno],Shr",i,i,k  ! open in route-sequence #
 
SCREEN1: !
	a$="" : prtbkno=0
	fnTos(sn$="UBPrtBl1-1")
	pf=26 : ll=24
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
	fnLbl(8,1,"Starting Account:",ll,1)
	fnComboF('ubm-act-nam',8,pf,40,"[Q]\UBmstr\Customer.h[cno]",1741,9,41,30,"[Q]\UBmstr\ubindx5.h[cno]",2)
	resp$(respc+=1)="[All]"
	fnLbl(9,1,"Route Number:",ll,1)
	fncmbrt2(9,pf)
	resp$(respc+=1)="[All]"
	fnChk(10,pf,"Select Accounts to Print",1)
	resp$(respc+=1)='False'
	fnCmdSet(3)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	d1=val(resp$(5))
	d4=val(resp$(1))
	mg$(1)=resp$(2)
	mg$(2)=resp$(3)
	mg$(3)=resp$(4)
	if resp$(6)="[All]" then a$="" else a$=lpad$(trim$(resp$(6)(1:9)),9)
	if resp$(7)="[All]" then prtbkno=0 else prtbkno=val(resp$(7))
	if resp$(8)='True' then sl1=1: z$="" else sl1=0
	if trim$(a$)<>"" then
		read #2,using L460,key=a$: z$,route,sequence nokey SCREEN1
		st1=1
		st1$=z$
	end if
L460: form pos 1,c 10,pos 1741,n 2,n 7
	if trim$(a$)="" and prtbkno=0 then restore #2,key>="         ": ! if no beginning account or starting route #, start at beginning of file
	if trim$(a$)<>"" then restore #2,key=cnvrt$("pic(zz)",route)& cnvrt$("pic(zzzzzzz)",sequence): nokey SCREEN1
	if trim$(a$)="" and prtbkno>0 then restore #2,key>=cnvrt$("pic(zz)",prtbkno)&"       ": ! selected a route and no beginning Account
 
	open #3: "Name=[Q]\UBmstr\UBAdrBil.h[cno],KFName=[Q]\UBmstr\adrIndex.h[cno],Shr",i,i,k
	fnpa_open("Landscape")
	lyne=3
NEXT_CUSTOMER: !
	if sl1=1 then goto SCR_ASK_CUSTOMER
READ_SORT_FILE: !
	read #6,using 'form pos 22,c 10': z$ eof RELEASE_PRINT
	read #1,using F_CUSTOMER,key=z$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,sequence nokey READ_SORT_FILE
F_CUSTOMER: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1750,2*n 6,pos 1942,c 12,pos 1864,c 30,pos 1743,n 7
	if prtbkno=0 then goto L620
	if prtbkno><route then goto RELEASE_PRINT
L620: if f><d1 then goto NEXT_CUSTOMER
	if st1=0 then goto AFTER_CUSTOMER_READ
	if st1$=z$ then st1=0 else goto NEXT_CUSTOMER
AFTER_CUSTOMER_READ: !
! r: read alternate billing address
	read #3,using 'form pos 11,4*c 30',key=z$: mat ba$ nokey L750
	e1=0 : mat pe$=("")
	for j=1 to 4
		if rtrm$(ba$(j))<>"" then e1=e1+1 : pe$(e1)=ba$(j)
	next j
	goto PRINT_IT
 
L750: !
	e1=0 : mat pe$=("")
	for j=2 to 4
		if rtrm$(e$(j))<>"" then e1=e1+1 : pe$(e1)=e$(j)
	next j
	if trim$(extra1$)<>"" then pe$(4)=pe$(3): pe$(3)=extra1$ ! set third address line to extra1$ (2nd address)
	goto PRINT_IT
! /r
RELEASE_PRINT: !
	close #1: ioerr ignore
	close #3: ioerr ignore
	fnpa_finis
	goto ENDSCR
 
PRINT_IT: ! r:
	if bal<>0 then
		pb=bal-g(11)
		if bal<=0 then g(10)=0 ! don't show penalty if balance 0 or less
! print bill routine
		fn_vbprint
! end of pr routine
		bct(2)=bct(2)+1 ! accumulate totals
	end if
	goto NEXT_CUSTOMER ! /r
 
SCR_ASK_CUSTOMER: ! r:
	sn$="UBPrtBl1-2"
	fnTos(sn$)
	txt$="Account (blank to stop)"
	fnLbl(1,1,txt$,31,1)
	! If TRIM$(A$)="" Then Goto 1030 Else Goto 1040 ! kj 7/12/05
	if trim$(z$)<>"" then
		fnLbl(3,1,"Last Account entered was "&z$,44,1)
	end if
	fncmbact(1,17) !
	resp$(1)=a$
	fnCmdSet(3)
	ckey=fnAcs(mat resp$)
	a$=lpad$(trim$(resp$(1)(1:10)),10)
	if trim$(a$)="" then goto RELEASE_PRINT
	if ckey=5 then goto RELEASE_PRINT
	read #1,using F_CUSTOMER,key=a$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,sequence nokey SCR_ASK_CUSTOMER
goto AFTER_CUSTOMER_READ ! /r
ENDSCR: ! r: pr totals screen
	if sum(bct)=0 then pct=0 else pct=bct(2)/sum(bct)*100
	fnTos(sn$="Bills-Total")
	mylen=23 : mypos=mylen+2
	respc=0
	fnLbl(1,1,"Total Bills Printed:",mylen,1)
	fnTxt(1,mypos,8,0,1,"",1)
	resp$(respc+=1)=cnvrt$("N 8",sum(bct))
	fnCmdSet(52)
	ckey=fnAcs(mat resp$)
goto Xit ! /r
Xit: fnXit
def fn_vbprint
	! -- Standard 4 Per Page Even Perferated Card Stock Bills
	checkcounter+=1
	if checkcounter=1 then xmargin=0 : ymargin=0
	if checkcounter=2 then xmargin=139 : ymargin=0
	if checkcounter=3 then xmargin=0 : ymargin=108
	if checkcounter=4 then xmargin=139 : ymargin=108 : checkcounter=0
 
	fnpa_line(xmargin+5,ymargin+2,55,lyne*3+3,1)
	fnpa_fontbold(1)
	fnpa_fontsize(12)
	fnpa_font
	fnpa_txt("Village of Moweaqua",xmargin+8,lyne*1-1+ymargin)
	fnpa_font("Lucida Console")
	fnpa_fontsize
	fnpa_fontbold
	fnpa_txt("    122 North Main  ",xmargin+6,lyne*2+1+ymargin-.2)
	fnpa_txt("  Moweaqua, IL 62550    ",xmargin+6,lyne*3+1+ymargin)
	fnpa_txt("#"&trim$(z$)&'  '&bulk$,xmargin+4,lyne*5+ymargin)
	fnpa_txt(e$(1),xmargin+4,lyne*6+ymargin)
	fnpa_txt('From: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d2)&'  To: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d3),xmargin+2,lyne*7+ymargin)
	fnpa_txt("Is due now and payable.",xmargin+2,lyne*8+ymargin)
	fnpa_txt('Billing Date: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d1),xmargin+2,lyne*11+ymargin)
	fnpa_line(xmargin+1,lyne*12+1+ymargin,62,0)
	fnpa_txt("Reading",xmargin+10,lyne*13+ymargin)
	fnpa_txt("Usage",xmargin+33,lyne*13+ymargin)
	fnpa_txt("Charge",xmargin+50,lyne*13+ymargin)
 
	meter=14
	fnpa_fontsize(8)
	if g(1) then
		fnpa_txt("WTR",xmargin+1,lyne*(meter+=1)+ymargin)
		fnpa_txt(fnformnumb$(d(1),0,9),xmargin+6,lyne*meter+ymargin)
		fnpa_txt(fnformnumb$(d(3),0,9),xmargin+25,lyne*meter+ymargin)
		fnpa_txt(fnformnumb$(g(1),2,9),xmargin+45,lyne*meter+ymargin)
	end if
	if g(2) then
		fnpa_txt("SWR",xmargin+1,lyne*(meter+=1)+ymargin)
		fnpa_txt(fnformnumb$(g(2),2,9),xmargin+45,lyne*meter+ymargin)
	end if
	if g(3) then
		fnpa_txt("Water Plant Maintenance",xmargin+1,lyne*(meter+=1)+ymargin)
		fnpa_txt(fnformnumb$(g(3),2,9),xmargin+45,lyne*meter+ymargin)
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
	if g(4) then
		fnpa_txt(gcode$,xmargin+1,lyne*(meter+=1)+ymargin)
		fnpa_txt(fnformnumb$(d(9),0,9),xmargin+6,lyne*meter+ymargin)
		fnpa_txt(fnformnumb$(d(11),0,9),xmargin+25,lyne*meter+ymargin)
		fnpa_txt(fnformnumb$(g(4),2,9),xmargin+45,lyne*meter+ymargin)
	end if
	if g(5) then
		fnpa_txt("SAN",xmargin+1,lyne*(meter+=1)+ymargin)
		fnpa_txt(fnformnumb$(g(5),2,9),xmargin+45,lyne*meter+ymargin)
	end if
	if g(6) then
		fnpa_txt("FP",xmargin+1,lyne*(meter+=1)+ymargin)
		fnpa_txt(fnformnumb$(g(6),2,9),xmargin+45,lyne*meter+ymargin)
	end if
	if g(7) then
		fnpa_txt("FEUL ADJ",xmargin+1,lyne*(meter+=1)+ymargin)
		fnpa_txt(fnformnumb$(g(7),2,9),xmargin+45,lyne*meter+ymargin)
	end if
	if g(8) then
		fnpa_txt("MISC",xmargin+1,lyne*(meter+=1)+ymargin)
		fnpa_txt(fnformnumb$(g(8),2,9),xmargin+45,lyne*meter+ymargin)
	end if
	if g(9) then
		fnpa_txt("TAX",xmargin+1,lyne*(meter+=1)+ymargin)
		fnpa_txt(fnformnumb$(g(9),2,9),xmargin+45,lyne*meter+ymargin)
	end if
	if pb then
		fnpa_txt("Previous Balance",xmargin+1,lyne*(meter+=1)+ymargin)
		fnpa_txt(fnformnumb$(pb,2,9),xmargin+45,lyne*meter+ymargin)
	end if
	fnpa_fontsize
 
	fnpa_line(xmargin+1,lyne*23+1+ymargin,63,0)
	fnpa_txt('Pay By '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':',xmargin+1,lyne*24+ymargin)
	fnpa_txt(fnformnumb$(bal,2,9),xmargin+42,lyne*24+ymargin)
	fnpa_txt('Pay After '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':',xmargin+1,lyne*25+ymargin)
	fnpa_txt(fnformnumb$(bal+g(10),2,9),xmargin+42,lyne*25+ymargin)
	fnpa_line(xmargin+1,lyne*26+1+ymargin,63,0)
	fnpa_txt("Phone: 217-768-3036 Ext 2",xmargin+1,lyne*27+ymargin)
 
	fnpa_fontsize(7)
	fnpa_line(xmargin+97,ymargin+0,29,lyne*5+2,1)
	fnpa_line(xmargin+90,ymargin+0,7,0)
	fnpa_line(xmargin+90,ymargin+2.8,7,0)
	fnpa_line(xmargin+90,ymargin+5.6,7,0)
	fnpa_line(xmargin+90,ymargin+8.4,7,0)
	fnpa_line(xmargin+90,ymargin+11.2,7,0)
	fnpa_line(xmargin+90,ymargin+14,7,0)
	fnpa_line(xmargin+90,ymargin+17,7,0)
	fnpa_txt("   Pre-Sorted",xmargin+100,lyne*1-1+ymargin)
	fnpa_txt("First Class Mail",xmargin+100,lyne*2-1+ymargin)
	fnpa_txt("  U.S. Postage  ",xmargin+100,lyne*3-1+ymargin)
	fnpa_txt("      Paid",xmargin+100,lyne*4-1+ymargin)
	fnpa_txt("  Permit No 38",xmargin+100,lyne*5-1+ymargin)
	fnpa_fontsize(9)
	! fnpa_txt("Address Service Requested",XMARGIN+68LYNE*7+YMARGIN-6)
	fnpa_txt("Please return this",xmargin+68,lyne*7+ymargin)
	fnpa_txt("side with payment to:",xmargin+68,lyne*8+ymargin)
	fnpa_txt("Village of Moweaqua",xmargin+68,lyne*9+ymargin)
	fnpa_fontsize
	fnpa_txt('Pay By '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':',xmargin+68,lyne*11+ymargin)
	fnpa_txt(fnformnumb$(bal,2,9),xmargin+106,lyne*11+ymargin)
	fnpa_txt('After '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':',xmargin+68,lyne*12+ymargin)
	fnpa_txt(fnformnumb$(bal+g(10),2,9),xmargin+106,lyne*12+ymargin)
	fnpa_fontsize(9)
	addy=14
	fnpa_txt(mg$(1),xmargin+68,(addy+=1)*lyne+ymargin)
	fnpa_txt(mg$(2),xmargin+68,(addy+=1)*lyne+ymargin)
	fnpa_txt(mg$(3),xmargin+68,(addy+=1)*lyne+ymargin)
	addy+=1
	fnpa_fontsize
	if df$="Y" then
		fnpa_txt("Drafted",xmargin+1,lyne*(addy+=1)+ymargin)
	end if
	if c4>0 then
		fnpa_txt("Final Bill",xmargin+1,lyne*(addy+=1)+ymargin)
	end if
	if d(10)=1 then
		fnpa_txt("Bill Estimated",xmargin+1,lyne*(addy+=1)+ymargin)
	end if
	fnpa_txt("#"&trim$(z$)&' '&bulk$,xmargin+68,lyne*(addy+=1)+ymargin)
	fnpa_txt(pe$(1),xmargin+68,lyne*(addy+=1)+ymargin)
	fnpa_txt(pe$(2),xmargin+68,lyne*(addy+=1)+ymargin)
	fnpa_txt(pe$(3),xmargin+68,lyne*(addy+=1)+ymargin)
	fnpa_txt(pe$(4),xmargin+68,lyne*(addy+=1)+ymargin)
	fnpa_txt("Address Service Requested",xmargin+68,lyne*(addy+=2)+ymargin)
	if checkcounter=1 then checkx=1.375 : checky=3.6875
	if checkcounter=2 then checkx=6.75 : checky=3.6875
	if checkcounter=3 then checkx=1.375 : checky=7.9375
	if checkcounter=0 then checkx=6.75 : checky=7.9375
	if checkcounter=0 then fnpa_newpage
fnend
 
def fn_bulksort ! bulk sort order
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",i,i,k  ! open in Account order
	open #6: "Name=[Temp]\Temp.[Session],Replace,RecL=31",internal,output
	L2730: read #1,using "form pos 1,C 10,pos 1741,n 2,pos 1743,n 7,pos 1942,c 12": z$,route,seq,bulk$ eof L2760
	write #6,using "form pos 1,C 12,n 2,n 7,c 10": bulk$,route,seq,z$
	goto L2730
	L2760: close #1: ioerr ignore
	close #6: ioerr ignore
	execute "Index [Temp]\Temp.[Session] [Temp]\Tempidx.[Session] 1,19,Replace,DupKeys -n"
	open #6: "Name=[Temp]\Temp.[Session],KFName=[Temp]\Tempidx."&session$,i,i,k
 
fnend
include: ertn
