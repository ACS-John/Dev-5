! Company Information File
 
	autoLibrary
	on error goto Ertn
 
	if fnclient_has('P2') then let fnstyp(11) else let fnstyp(14) !  styp=11 for jobcost; styp=14 for regular payroll
	fnTop(program$)
 
	dim a$(3)*40,b$(2)*12,d$(10)*8,label1$(10)*20,m(10),r(10)
	dim fa$(10)*26,e$(10)*12,prh$(2)*40
	dim rpnames$(86)*20,rpnames2$(10)*6,x$(10)*20,io2$(50)*24,na$(125)*8
	dim gln(45),glnt(45),gln$(15)*12,dedcode(10),calcode(10),dedfed(10)
	dim prgl(15,3),label5$(15)*13,iolabel5$(17),io5$(45)*20,win6$*40
	dim x1(3),x2(3),x3(3),x4(3),x5(3),x6(3),x7(3),x8(3),x9(3),x10(3),sck(4)
	dim wcm(4),io6$(11)*16,jn$(2)*6,iolabel1$*10
	dim resp$(200)*40,ml$(3)*100
	dim fullname$(20)*20
	dim newcalcode(20),newdedfed(20),dedfica(20),dedst(20),deduc(20)
	dim abrevname$(20)*8
	dim newdedcode(20)
	dim gl$(20)*12,fid$*12
 
	dim opt_ded_or_add$(3)*7
	opt_ded_or_add$(1)="Deduct"
	opt_ded_or_add$(2)="Add"
	opt_ded_or_add$(3)="Benefit"
 
	dim opt_std_or_percent$(2)*8
	opt_std_or_percent$(1)="Standard"
	opt_std_or_percent$(2)="Percent"
 
 
	open #1: "Name=[Q]\PRmstr\Company.h[cno]"&',recl=759,version=0,use',internal,outIn,relative
	if lrec(1)=0 then write #1,using 'Form POS 1,3*C 40,C 12,PD 6.3,PD 6.2,PD 5.2,10*C 8,N 2,PD 4.2,PD 3.3,12*PD 4.2,10*PD 3.3,25*C 12,31*N 1,10*C 6,3*PD 4.3,3*PD 3.2,4*PD 4.2,N 1,2*C 6,N 2': mat a$,fid$,mcr,mcm,feducrat,mat d$,loccode,feducmax,ficarate,ficamaxw,ficawh,mat m,mat r,mat e$,mat gln$,gli,mat dedcode,mat calcode,mat dedfed,mat rpnames2$,mat sck,vacm,mhw,mat wcm,tc,mat jn$,dc
	read #1,using 'Form POS 1,3*C 40,C 12,PD 6.3,PD 6.2,PD 5.2,10*C 8,N 2,PD 4.2,PD 3.3,12*PD 4.2,10*PD 3.3,25*C 12,31*N 1,10*C 6,3*PD 4.3,3*PD 3.2,4*PD 4.2,N 1,2*C 6,N 2',rec=1: mat a$,fid$,mcr,mcm,feducrat,mat d$,loccode,feducmax,ficarate,ficamaxw,ficawh,mat m,mat r,mat e$,mat gln$,gli,mat dedcode,mat calcode,mat dedfed,mat rpnames2$,mat sck,vacm,mhw,mat wcm,tc,mat jn$,dc ioerr L290
	ficamaxw=ficamaxw*10
	L290: close #1:
	! READNAMES: !
	fnDedNames(mat fullname$,mat abrevname$,mat newdedcode,mat newcalcode,mat newdedfed,mat dedfica,mat dedst,mat deduc,mat gl$)
goto SCREEN_1
SCREEN_1: ! r:
	resp=0
	fnTos
	mylen=30: mypos=mylen+3 : right=1
	fram1=1: fnFra(1,1,10,80,"Company Number "&env$('cno'))
	fnLbl(1,1,"Company Name:",mylen,right,0,fram1)
	fnTxt(1,mypos,40,0,left,"",0,"",fram1)
	resp$(1)=a$(1)
	fnLbl(2,1,"Company Address:",mylen,right,0,fram1)
	fnTxt(2,mypos,40,0,left,"",0,"",fram1)
	resp$(2)=a$(2)
	fnLbl(3,1,"City, State, Zip:",mylen,right,0,fram1)
	fnTxt(3,mypos,40,0,left,"",0,"",fram1)
	resp$(3)=a$(3)
	fnLbl(4,1,"Federal ID #:",mylen,right,0,fram1)
	fnTxt(4,mypos,12,0,left,"",0,"",fram1)
	resp$(4)=fid$
	fnLbl(5,1,"Federal U/C Rate:",mylen,right,0,fram1)
	fnTxt(5,mypos,10,0,left,"33",0,"In 2007 the rate was .8% and should be entered as .8 and not .008",fram1)
	resp$(5)=str$(feducrat)
	fnLbl(6,1,"Federal U/C Maximum Wage:",mylen,right,0,fram1)
	fnTxt(6,mypos,12,0,left,"10",0,"",fram1)
	resp$(6)=str$(feducmax)
	fnLbl(7,1,"Social Security Rate:",mylen,right,0,fram1)
	fnTxt(7,mypos,10,0,left,"33",0,"Sample format 6.2",fram1 )
	resp$(7)=str$(ficarate)
	fnLbl(8,1,"SS Maximum Wage:",mylen,right,0,fram1)
	fnTxt(8,mypos,12,0,left,"10",0,"The maximum was 97500 for the year 2007.  See a 941 form.",fram1)
	resp$(8)=str$(ficamaxw)
	fnLbl(9,1,"Medicare Rate:",mylen,right,0,fram1)
	fnTxt(9,mypos,10,0,left,"33",0,"Format would be 1.450",fram1)
	resp$(9)=str$(mcr)
	fnLbl(10,1,"Medicare Maximum Wage:",mylen,right,0,fram1)
	fnTxt(10,mypos,12,0,left,"10",0,"Use 999999.99 since there no maximum wage at this time.",fram1)
	resp$(10)=str$(mcm)
	fram2=2: fnFra(13,1,8,90,"General Ledger Information")
	fnChk(1,30,"General Ledger Installed:",1,fram2)
	if gli=1 then resp$(11)="True" else resp$(11)="False"
	fnLbl(2,1,"Cash In Bank:",mylen,right,0,fram2)
	fnqgl(2,32,fram2,2,pas)
	resp$(12)=fnrgl$(gln$(15))
	fnLbl(3,1,"Federal W/H:",mylen,right,0,fram2)
	fnqgl(3,32,fram2,2,pas)
	resp$(13)=fnrgl$(gln$(1))
	fnLbl(4,1,"SS & Medicare W/H:",mylen,right,0,fram2)
	fnqgl(4,32,fram2,2,pas)
	resp$(14)=fnrgl$(gln$(2))
	fnLbl(5,1,"State W/H:",mylen,right,0,fram2)
	fnqgl(5,32,fram2,2,pas)
	resp$(15)=fnrgl$(gln$(3))
	fnLbl(6,1,"EIC:",mylen,right,0,fram2)
	fnqgl(6,32,fram2,2,pas)
	resp$(16)=fnrgl$(gln$(14))
	fnCmdKey("&Next",1,1,0,"Moves to 2nd screen of company information.")
	fnCmdKey("&Save and Exit",4,0,0,"Saves any changes and returns to menu without reviewing remainter of screens.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu without saving any changes on any screen.")
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto DoCancel
	a$(1)=resp$(1)
	a$(2)=resp$(2)
	a$(3)=resp$(3)
	fid$=resp$(4)
	feducrat=val(resp$(5))
	feducmax=val(resp$(6))
	ficarate=val(resp$(7))
	ficamaxw=val(resp$(8)) ! pr ficamaxw : pause
	mcr=val(resp$(9))
	mcm=val(resp$(10))
	if resp$(11)="True" then gli=1 else gli=0
	gln$(15)=fnagl$(resp$(12)) ! bank
	gln$(1)=fnagl$(resp$(13)) ! fed
	gln$(2)=fnagl$(resp$(14)) ! fica
	gln$(3)=fnagl$(resp$(15)) ! state
	gln$(14)=fnagl$(resp$(16)) ! eic
	if mcr<=0 then mcr=1.45
	if mcm<=0 then mcm=999999
	if feducrat>5 then goto L1000 else goto L1010
L1000: mat ml$(2)
	ml$(1)="The Federal Unemployment Rate appears to be wrong!"
	ml$(2)="Do you wish to continue anyway?"
	fnmsgbox(mat ml$,resp$,'',52)
	if resp$<>"Yes" then goto SCREEN_1
L1010: goto L1030 ! If FEDUCMAX<=0 Then Goto 1020 Else Goto 1120
	mat ml$(2)
	ml$(1)="The Federal U/C Maximum wage appears to be wrong!"
	ml$(2)="Do you wish to continue anyway?"
	fnmsgbox(mat ml$,resp$,'',52)
	if resp$="Yes" then goto SCREEN_2 else goto SCREEN_1
L1030: if ckey=4 then goto DONE
goto SCREEN_2 ! /r
SCREEN_2: ! r:
	fnTos
	mylen=32: mypos=mylen+3 : right=1
	pos_col(1)=1 ! deduction numbers
	pos_col(2)=5 ! deduction name
	pos_col(3)=23 ! name
	pos_col(4)=33 ! ded/add
	pos_col(5)=43 ! std/pct
	pos_col(6)=57 ! ded fed
	pos_col(7)=62 ! ded fica
	pos_col(8)=67 ! ded state
	pos_col(9)=72 ! ded u/c
	pos_col(10)=75
	fnLbl(1,1,"Enter your deductions names. Mark whether a deduction or addition.",90,left)
	fnLbl(2,1,"A check mark will indicate 'yes' to deduct before calculating Federal w/h, etc.",90,left)
	fnLbl(5,pos_col(2),"Deduction Name")
	fnLbl(5,pos_col(3),"Abbr")
	fnLbl(5,pos_col(3),"Name")
	fnLbl(5,pos_col(4),"Ded/Add")
	fnLbl(5,pos_col(5),"Std/Pct")
	fnLbl(4,pos_col(6)-2,"Ded")
	fnLbl(5,pos_col(6)-2,"Fed")
	fnLbl(4,pos_col(7)-2,"Ded")
	fnLbl(5,pos_col(7)-2,"FICA")
	fnLbl(4,pos_col(8)-2,"Ded")
	fnLbl(5,pos_col(8)-2,"State")
	fnLbl(4,pos_col(9)-2,"Ded")
	fnLbl(5,pos_col(9)-2,"U/C")
	fnLbl(5,pos_col(10),"GL Number")
	resp=0
	for j=1 to 20
		fnLbl(j+5,pos_col(1),str$(j)&'.',3,1)
		fnTxt(j+5,pos_col(2),15,20,left,"",0,"Enter your deduction name.",0 )
		resp$(resp+=1)=fullname$(j)
		fnTxt(j+5,pos_col(3),8,0,left,"",0,"Enter an abbreviated name that will be used in report headings.",0 )
		resp$(resp+=1)=abrevname$(j)
		fncomboa("MiscDeduct",j+5,pos_col(4),mat opt_ded_or_add$,"Indicate whether the amont should be deducted from the check or added to the check.",6)
		if newdedcode(j)=0 then newdedcode(j)=1
		resp$(resp+=1)=opt_ded_or_add$(newdedcode(j))
		fncomboa("std_or_percent",j+5,pos_col(5),mat opt_std_or_percent$,"Standard would a fixed amount each pay period.  Percent would indicate the deduction is a percent of gross pay.",8)
		if newcalcode(j)=0 then newcalcode(j)=1 ! stop subscript error
		resp$(resp+=1)=opt_std_or_percent$(newcalcode(j))
		fnChk(j+5,pos_col(6),"",1)
		if newdedfed(j)>0 then resp$(resp+=1)="True" else resp$(resp+=1)="False"
		fnChk(j+5,pos_col(7),"",1)
		if dedfica(j)>0 then resp$(resp+=1)="True" else resp$(resp+=1)="False"
		fnChk(j+5,pos_col(8),"",1)
		if dedst(j)>0 then resp$(resp+=1)="True" else resp$(resp+=1)="False"
		fnChk(j+5,pos_col(9),"",1)
		if deduc(j)>0 then resp$(resp+=1)="True" else resp$(resp+=1)="False"
		linecount=j+5
		fnqgl(linecount,pos_col(10),0,2,pas)
		resp$(resp+=1)=fnrgl$(gl$(j))
	next j
	fnCmdKey("&Next",1,1,0,"Moves to next screen of company information.")
	fnCmdKey("&Save and Exit",4,0,0,"Saves any changes and returns to menu.")
	fnCmdKey("&Back",2,0,0,"Returns to previous screen.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu without saving any changes on any screen.")
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto DoCancel
	resp=0
	for j=1 to 20
		fullname$(j)=resp$(resp+=1)
		abrevname$(j)=resp$(resp+=1)
		if resp$(resp+=1)=opt_ded_or_add$(1) then newdedcode(j)=1 ! deduction
		if resp$(resp)=opt_ded_or_add$(2) then newdedcode(j)=2 ! addition
		if resp$(resp)=opt_ded_or_add$(3) then newdedcode(j)=3 ! benefit
		if resp$(resp+=1)=opt_std_or_percent$(1) then newcalcode(j)=1
		if resp$(resp)=opt_std_or_percent$(2) then newcalcode(j)=2 ! percent method of calucalating
		if resp$(resp+=1)="True" then newdedfed(j)=1 else newdedfed(j)=0
		if resp$(resp+=1)="True" then dedfica(j)=1 else dedfica(j)=0
		if resp$(resp+=1)="True" then dedst(j)=1 else dedst(j)=0
		if resp$(resp+=1)="True" then deduc(j)=1 else deduc(j)=0
		gl$(j)=fnagl$(resp$(resp+=1))
	next j
	if ckey=2 then goto SCREEN_1
	if ckey=4 then goto DONE
	if ckey=5 then goto DoCancel
	! if ckey=1 then goto SCREEN_3
goto SCREEN_3 ! /r
 
SCREEN_3: ! r:
	fnTos
	mylen=32: mypos=mylen+3 : right=1
	fnLbl(1,10,"STATE CODES AND UNEMPLOYMENT INFORMATION",0,0)
	fnLbl(3,1,"Code State Name     State ID    U/C Maximum      U/C Rate",0,0)
	resp=0
 
	for j=1 to 10
		fnLbl(j+3,3,str$(j),mylen,0,0)
		fnTxt(j+3,6,8,0,left,"",0,"Enter your state name.",0 )
		resp$(resp+=1)=d$(j)
		fnTxt(j+3,19,12,0,left,"",0,"Enter the state id #.",0 )
		resp$(resp+=1)=e$(j)
		fnTxt(j+3,32,12,0,left,"10",0,"Enter the maximum wage subject to state unemployment (See your state u/c report.",0 )
		resp$(resp+=1)=str$(m(j))
		fnTxt(j+3,49,8,0,left,"33",0,"Enter the state unemployment rate (See your state u/c report. Enter in percent format. Example: 5% as 5.00",0 )
		resp$(resp+=1)=str$(r(j))
	next j
	fnCmdKey("&Next",1,1,0,"Moves to next screen of company information.")
	fnCmdKey("&Save and Exit",4,0,0,"Saves any changes and returns to menu.")
	fnCmdKey("&Back",2,0,0,"Returns to previous screen.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu without saving any changes on any screen.")
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto DoCancel
	resp=0
	for j=1 to 10
		d$(j)=resp$(resp+=1)
		e$(j)=resp$(resp+=1)
		m(j)=val(resp$(resp+=1))
		r(j)=val(resp$(resp+=1))
	next j
	if ckey=2 then goto SCREEN_2
	if ckey=4 then goto DONE
	if ckey=5 then goto DoCancel
goto SCREEN_4 ! /r
 
SCREEN_4: ! r:
	fnTos
	mylen=45: mypos=mylen+3 : right=1: resp=0
	fram3=1: fnFra(1,1,6,60,"Vacation and Sick Pay Information")
	fnLbl(1,1,"Days employed before accruing sick hours",mylen,right,0,fram3)
	fnTxt(1,mypos,8,0,0,"30",0,"",fram3)
	resp$(1)=str$(sck(1))
	fnLbl(2,1,"Sick hours accrued after eligibility period:",mylen,right,0,fram3)
	fnTxt(2,mypos,8,0,0,"33",0,"",fram3)
	resp$(2)=str$(sck(2))
	fnLbl(3,1,"Sick hours to accrue once eligible:",mylen,right,0,fram3)
	fnTxt(3,mypos,8,0,0,"33",0,"",fram3)
	resp$(3)=str$(sck(3))
	fnLbl(4,1,"Maximum Sick Hours:",mylen,right,0,fram3)
	fnTxt(4,mypos,8,0,0,"32",0,"",fram3)
	resp$(4)=str$(sck(4))
	fnLbl(5,1,"Maximum Vacation Hours:",mylen,right,0,fram3)
	fnTxt(5,mypos,8,0,0,"32",0,"",fram3)
	resp$(5)=str$(vacm)
	fram4=2: fnFra(9,1,3,60,"Miscellaneous Payroll Information")
	fnLbl(1,1,"Minimum Hourly Wage:",mylen,right,0,fram4)
	fnTxt(1,mypos,10,0,0,"10",0,"",fram4)
	resp$(6)=str$(mhw)
	fnLbl(2,1,"Local Withholding Code:",mylen,right,0,fram4)
	fnTxt(2,mypos,2,0,0,"30",0,"If one of the twenty miscellaneous dedutions is used for local withholdings, then enter the number of the deduction.",fram4)
	resp$(7)=str$(loccode)
	fram5=3: fnFra(14,1,5,60,"Workman's Compensation Limits")
	fnLbl(1,1,"Monthly:",mylen,right,0,fram5)
	fnTxt(1,mypos,10,0,0,"10",0,"If you pay workman's comp and there are limits on the maximum wage subject to workman's comp, then enter that maximum wage under the proper pay period.",fram5)
	resp$(8)=str$(wcm(1))
	fnLbl(2,1,"Semi-Monthly:",mylen,right,0,fram5)
	fnTxt(2,mypos,10,0,0,"10",0,"If you pay workman's comp and there are limits on the maximum wage subject to workman's comp, then enter that maximum wage under the proper pay period.",fram5)
	resp$(9)=str$(wcm(2))
	fnLbl(3,1,"Bi-Weekly:",mylen,right,0,fram5)
	fnTxt(3,mypos,10,0,0,"10",0,"If you pay workman's comp and there are limits on the maximum wage subject to workman's comp, then enter that maximum wage under the proper pay period.",fram5)
	resp$(10)=str$(wcm(3))
	fnLbl(4,1,"Weekly:",mylen,right,0,fram5)
	fnTxt(4,mypos,10,0,0,"10",0,"If you pay workman's comp and there are limits on the maximum wage subject to workman's comp, then enter that maximum wage under the proper pay period.",fram5)
	resp$(11)=str$(wcm(4))
	fnCmdKey("&Next",1,1,0,"Moves to next screen of company information.")
	fnCmdKey("&Save and Exit",4,0,0,"Saves any changes and returns to menu.")
	fnCmdKey("&Back",2,0,0,"Returns to previous screen.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu without saving any changes on any screen.")
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto DoCancel
	sck(1)=val(resp$(1))
	sck(2)=val(resp$(2))
	sck(3)=val(resp$(3))
	sck(4)=val(resp$(4))
	vacm=val(resp$(5))
	mhw=val(resp$(6))
	loccode=val(resp$(7))
	wcm(1)=val(resp$(8))
	wcm(2)=val(resp$(9))
	wcm(3)=val(resp$(10))
	wcm(4)=val(resp$(11))
	if ckey=2 then goto SCREEN_3
	if ckey=4 then goto DONE
	! if ckey=1 then goto SCREEN_5
goto SCREEN_5 ! /r
 
SCREEN_5: ! r:
	goto DONE ! will need the job cost screen someday
	if ckey=2 then goto SCREEN_4
	if ckey=5 then goto DONE
 
	close #106: ioerr ignore
	if fnstyp=14 then win6$="SRow=14,SCol=12,ECol=67"
	if fnstyp<>14 then win6$="SRow=09,SCol=11,ECol=69"
	open #106: win6$&",ERow=22,Border=SR,Caption=<"&env$('program_caption'),display,outIn
	pr #106: newpage
	pr #106,fields "2,2,Cr 44,N": "ACS General Ledger Installed (Y/N):"
	pr #106,fields "3,2,Cr 44,N": "Days employed before accruing Sick Hours:"
	pr #106,fields "4,2,Cr 44,N": "Sick Hours accrued after eligibility period:"
	pr #106,fields "5,2,Cr 44,N": "Sick Hours to accrue each time accumulated:"
	pr #106,fields "6,2,Cr 44,N": "Maximum Sick Hours:"
	pr #106,fields "7,2,Cr 44,N": "Maximum Vacation Hours:"
	pr #106,fields "8,2,Cr 44,N": "Minimum Hourly Wage:"
	io6$(1)="2,47,Cu 1,UT,N"
	io6$(2)="3,47,N 8.3,UT,N"
	io6$(3)="4,47,N 8.3,UT,N"
	io6$(4)="5,47,N 8.3,UT,N"
	io6$(5)="6,47,N 6.2,UT,N"
	io6$(6)="7,47,N 6.2,UT,N"
	io6$(7)="8,47,N 6.2,UT,N"
	if fnstyp=11 then
		! (Job Cost Questions)
		pr #106,fields "10,2,Cr 50,N": "Retain Transactions until Jobs are complete (Y/N):"
		pr #106,fields "11,2,Cr 50,N": "Starting of Range for Non-Productive Jobs:"
		pr #106,fields "12,2,Cr 50,N": "Ending of Range for Non-Productive Jobs:"
		pr #106,fields "13,2,Cr 50,N": "Number of Deduction used for Union Dues (if any):"
		io6$(8)="10,53,Cu 1,UT,N" : io6$(9)="11,53,C 6,UT,N"
		io6$(10)="12,53,C 6,UT,N" : io6$(11)="13,53,Nz 2,UT,N"
		if tc=1 then tc$="Y" else tc$="N"
	end if
	pr f "23,25,C 09,B,1": "Next (F1)"
	pr f "23,35,C 09,B,2": "Back (F2)"
	pr f "23,45,C 09,B,5": "Done (F5)"
	if gli=1 then gli$="Y" else gli$="N"
L2740: !
	if fnstyp=11 then
		rinput #106,fields mat io6$: gli$,mat sck,vacm,mhw,tc$,mat jn$,dc conv CONV7
	else
		rinput #106,fields mat io6$: gli$,mat sck,vacm,mhw conv CONV7
	end if
	if ce>0 then io6$(ce)(ce1:ce2)="U": ce=0
	if ckey>0 then goto L2840 else ce=curfld+1
	if ce>udim(io6$) then ce=1
L2790: io6$(ce)=rtrm$(io6$(ce)) : ce1=pos(io6$(ce),"U",1)
	ce2=ce1+1 : io6$(ce)(ce1:ce1)="UC" : goto L2740
CONV7: if ce>0 then io6$(ce)(ce1:ce2)="U"
	ce=cnt+1
ERR7: pr f "24,78,C 1": bell : goto L2790
L2840: if gli$<>"Y" and gli$<>"N" then ce=1 : goto ERR7
	if gli$="Y" then gli=1 else gli=0
	if fnstyp=14 then goto L2900
	if tc$="Y" then tc=1 else tc=0
	if fnstyp=11 and tc$<>"Y" and tc$<>"N" then ce=8 : goto ERR7
	if dc<0 or dc>10 then ce=11: goto ERR7
	L2900: !
	if ckey=5 or ckey=1 then goto DoCancel
	if ckey=2 then goto SCREEN_5
goto L2740 ! /r
DoCancel: ! r:
! 	mat ml$(2)
! 	ml$(1)="You have chosen to exit without saving any changes."
! 	ml$(2)="Save changes now?"
! 	fnmsgbox(mat ml$,resp$,'',52)
! if resp$="Yes" then goto DONE else goto Xit
goto Xit
! /r
 
DONE: ! r:
	open #1: "Name=[Q]\PRmstr\Company.h[cno]",internal,outIn,relative
	ficamaxw=ficamaxw*.1
	rewrite #1,using 'Form POS 1,3*C 40,C 12,PD 6.3,PD 6.2,PD 5.2,10*C 8,N 2,PD 4.2,PD 3.3,12*PD 4.2,10*PD 3.3,25*C 12,31*N 1,10*C 6,3*PD 4.3,3*PD 3.2,4*PD 4.2,N 1,2*C 6,N 2',rec=1: mat a$,fid$,mcr,mcm,feducrat,mat d$,loccode,feducmax,ficarate,ficamaxw,ficawh,mat m,mat r,mat e$,mat gln$,gli,mat dedcode,mat calcode,mat dedfed,mat rpnames2$,mat sck,vacm,mhw,mat wcm,tc,mat jn$,dc
	close #1:
	fnDedNames(mat fullname$,mat abrevname$,mat newdedcode,mat newcalcode,mat newdedfed,mat dedfica,mat dedst,mat deduc,mat gl$,1)
goto Xit ! /r
 
Xit: fnXit
include: ertn
