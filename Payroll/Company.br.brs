! Company Information File

autoLibrary
fnTop(program$)
on error goto Ertn
! r: initialize stuff, set constants, etc
if fnClientHas('P2') then fnstyp(11) else fnstyp(14) !  styp=11 for jobcost; styp=14 for regular payroll

dim resp$(200)*40

open #1: 'Name=[Q]\PRmstr\Company.h[cno],recl=759,version=0,use',i,outi,r
dim fPrCompany$*256
fPrCompany$='form pos 1,3*C 40,C 12,PD 6.3,PD 6.2,PD 5.2,10*C 8,N 2,PD 4.2,PD 3.3,12*PD 4.2,10*PD 3.3,25*C 12,31*N 1,10*C 6,3*PD 4.3,3*PD 3.2,4*PD 4.2,N 1,2*C 6,N 2'
dim a$(3)*40
dim fid$*12
dim d$(10)*8
dim xm(10)
dim xr(10)
dim e$(10)*12
dim gln$(15)*12
dim dedcode(10)
dim calcode(10)
dim dedfed(10)
dim rpnames2$(10)*6
dim sck(4)
dim wcm(4)
dim jn$(2)*6
if lrec(1)=0 then
	write #1,using fPrCompany$       	: mat a$,fid$,mcr,mcm,feducrat,mat d$,loccode,feducmax,ficarate,ssmax,ficawh,mat xm,mat xr,mat e$,mat gln$,gli,mat dedcode,mat calcode,mat dedfed,mat rpnames2$,mat sck,vacm,mhw,mat wcm,tc,mat jn$,dc
end if
read #1,using fPrCompany$,rec=1 	: mat a$,fid$,mcr,mcm,feducrat,mat d$,loccode,feducmax,ficarate,ssmax,ficawh,mat xm,mat xr,mat e$,mat gln$,gli,mat dedcode,mat calcode,mat dedfed,mat rpnames2$,mat sck,vacm,mhw,mat wcm,tc,mat jn$,dc ioerr L290
ssmax=ssmax*10
L290: !
close #1:
! READNAMES: !

dim fullname$(20)*20
dim abrevname$(20)*8
dim newdedcode(20)
dim newcalcode(20)
dim newdedfed(20)
dim dedFica(20)
dim dedSt(20)
dim dedUc(20)
dim dedGl$(20)*12
fnDedNames(mat fullname$,mat abrevname$,mat newdedcode,mat newcalcode,mat newdedfed,mat dedFica,mat dedSt,mat dedUc,mat dedGl$)
goto Screen1 ! /r

Screen1: ! r:
	resp=0
	fnTos
	mylen=30: mypos=mylen+3 : right=1
	fram1=1: fnFra(1,1,10,80,'Company Number '&env$('cno'))
	fnLbl(1,1,'Company Name:',mylen,right,0,fram1)              	: fnTxt(1,mypos,40,0,left,'',0,'',fram1)                                                                          	: resp$(1)=a$(1)
	fnLbl(2,1,'Company Address:',mylen,right,0,fram1)           	: fnTxt(2,mypos,40,0,left,'',0,'',fram1)                                                                          	: resp$(2)=a$(2)
	fnLbl(3,1,'City, State, Zip:',mylen,right,0,fram1)          	: fnTxt(3,mypos,40,0,left,'',0,'',fram1)                                                                          	: resp$(3)=a$(3)
	fnLbl(4,1,'Federal ID #:',mylen,right,0,fram1)              	: fnTxt(4,mypos,12,0,left,'',0,'',fram1)                                                                          	: resp$(4)=fid$
	fnLbl(5,1,'Federal U/C Rate:',mylen,right,0,fram1)          	: fnTxt(5,mypos,10,0,left,'33',0,'In 2007 the rate was .8% and should be entered as .8 and not .008',fram1)	: resp$(5)=str$(feducrat)
	fnLbl(6,1,'Federal U/C Maximum Wage:',mylen,right,0,fram1) 	: fnTxt(6,mypos,12,0,left,'10',0,'',fram1)                                                                        	: resp$(6)=str$(feducmax)
	fnLbl(7,1,'Social Security Rate:',mylen,right,0,fram1)     	: fnTxt(7,mypos,10,0,left,'33',0,'Sample format 6.2',fram1 )                                                    	: resp$(7)=str$(ficarate)
	fnLbl(8,1,'SS Maximum Wage:',mylen,right,0,fram1)           	: fnTxt(8,mypos,12,0,left,'10',0,'The maximum was 97500 for the year 2007.  See a 941 form.',fram1)        	: resp$(8)=str$(ssmax)
	fnLbl(9,1,'Medicare Rate:',mylen,right,0,fram1)             	: fnTxt(9,mypos,10,0,left,'33',0,'Format would be 1.450',fram1)                                                	: resp$(9)=str$(mcr)
	fnLbl(10,1,'Medicare Maximum Wage:',mylen,right,0,fram1)   	: fnTxt(10,mypos,12,0,left,'10',0,'Use 999999.99 since there no maximum wage at this time.',fram1)         	: resp$(10)=str$(mcm)
	fram2=2: fnFra(13,1,8,90,'General Ledger Information')
	fnChk(1,30,'General Ledger Installed:',1,fram2)                                        : resp$(11)='False' : 	if gli=1 then resp$(11)='True'
	fnLbl(2,1,'Cash In Bank:',mylen,right,0,fram2)              	: fnQgl(2,32,fram2,2,1) : resp$(12)=fnRgl$(gln$(15),0,1)
	fnLbl(3,1,'Federal W/H:',mylen,right,0,fram2)                	: fnQgl(3,32,fram2,2,1) : resp$(13)=fnRgl$(gln$( 1),0,1)
	fnLbl(4,1,'SS & Medicare W/H:',mylen,right,0,fram2)         	: fnQgl(4,32,fram2,2,1) : resp$(14)=fnRgl$(gln$( 2),0,1)
	fnLbl(5,1,'State W/H:',mylen,right,0,fram2)                  	: fnQgl(5,32,fram2,2,1) : resp$(15)=fnRgl$(gln$( 3),0,1)
	fnLbl(6,1,'EIC:',mylen,right,0,fram2)                         	: fnQgl(6,32,fram2,2,1) : resp$(16)=fnRgl$(gln$(14),0,1)
	fnCmdKey('&Next',1,1,0,'Moves to 2nd screen of company information.')
	fnCmdKey('&Save and Exit',4,0,0,'Saves any changes and returns to menu without reviewing remainder of screens.')
	fnCmdKey('&Cancel',5,0,1,'Returns to menu without saving any changes on any screen.')
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto DoCancel
	a$(1)=resp$(1)
	a$(2)=resp$(2)
	a$(3)=resp$(3)
	fid$=resp$(4)
	feducrat=val(resp$(5))
	feducmax=val(resp$(6))
	ficarate=val(resp$(7))
	ssmax=val(resp$(8)) ! pr ssmax : pause
	mcr=val(resp$(9))
	mcm=val(resp$(10))
	if resp$(11)='True' then gli=1 else gli=0
	gln$(15)=fnAgl$(resp$(12)) ! bank
	gln$(1)=fnAgl$(resp$(13)) ! fed
	gln$(2)=fnAgl$(resp$(14)) ! fica
	gln$(3)=fnAgl$(resp$(15)) ! state
	gln$(14)=fnAgl$(resp$(16)) ! eic
	if mcr<=0 then mcr=1.45
	if mcm<=0 then mcm=999999
	if feducrat>5 then
		dim ml$(0)*100
		mat ml$(2)
		ml$(1)='The Federal Unemployment Rate appears to be wrong!'
		ml$(2)='Do you wish to continue anyway?'
		fnMsgBox(mat ml$,resp$,'',52)
		if resp$<>'Yes' then goto Screen1
	end if
	if ckey=4 then goto Finis
goto Screen2 ! /r
goto Screen2 ! /r

Screen2: ! r:
	! r: screen2 setup
	if ~setup_screen2 then ! r:
		setup_screen2=1
		dim opt_ded_or_add$(3)*7
		opt_ded_or_add$(1)='Deduct'
		opt_ded_or_add$(2)='Add'
		opt_ded_or_add$(3)='Benefit'

		dim opt_std_or_percent$(2)*8
		opt_std_or_percent$(1)='Standard'
		opt_std_or_percent$(2)='Percent'
	end if ! /r
	! /r
	fnTos
	mylen=32: mypos=mylen+3 : right=1
	pos_col( 1)= 1 ! deduction numbers
	pos_col( 2)= 5 ! deduction name
	pos_col( 3)=23 ! name
	pos_col( 4)=33 ! ded/add
	pos_col( 5)=43 ! std/pct
	pos_col( 6)=57 ! ded fed
	pos_col( 7)=62 ! ded fica
	pos_col( 8)=67 ! ded state
	pos_col( 9)=72 ! ded u/c
	pos_col(10)=75
	fnLbl(1,1,'Enter your deductions names. Mark whether a deduction or addition.',90,left)
	fnLbl(2,1,'A check mark will indicate "yes" to deduct before calculating Federal w/h, etc.',90,left)
	fnLbl(5,pos_col(2),'Deduction Name')
	fnLbl(5,pos_col(3),'Abbr')
	fnLbl(5,pos_col(3),'Name')
	fnLbl(5,pos_col(4),'Ded/Add')
	fnLbl(5,pos_col(5),'Std/Pct')
	fnLbl(4,pos_col(6)-2,'Ded')
	fnLbl(5,pos_col(6)-2,'Fed')
	fnLbl(4,pos_col(7)-2,'Ded')
	fnLbl(5,pos_col(7)-2,'FICA')
	fnLbl(4,pos_col(8)-2,'Ded')
	fnLbl(5,pos_col(8)-2,'State')
	fnLbl(4,pos_col(9)-2,'Ded')
	fnLbl(5,pos_col(9)-2,'U/C')
	fnLbl(5,pos_col(10),'GL Number')
	resp=0
	for j=1 to 20
		fnLbl(j+5,pos_col(1),str$(j)&'.',3,1)
		fnTxt(j+5,pos_col(2),15,20,left,'',0,'Enter your deduction name.',0 )
		resp$(resp+=1)=fullname$(j)
		fnTxt(j+5,pos_col(3),8,0,left,'',0,'Enter an abbreviated name that will be used in report headings.',0 )
		resp$(resp+=1)=abrevname$(j)
		fnComboA('MiscDeduct',j+5,pos_col(4),mat opt_ded_or_add$,'Indicate whether the amont should be deducted from the check or added to the check.',6)
		if newdedcode(j)=0 then newdedcode(j)=1
		resp$(resp+=1)=opt_ded_or_add$(newdedcode(j))
		fnComboA('std_or_percent',j+5,pos_col(5),mat opt_std_or_percent$,'Standard would a fixed amount each pay period.  Percent would indicate the deduction is a percent of gross pay.',8)
		if newcalcode(j)=0 then newcalcode(j)=1 ! stop subscript error
		resp$(resp+=1)=opt_std_or_percent$(newcalcode(j))
		fnChk(j+5,pos_col(6),'',1) : if newdedfed(j)>0 then resp$(resp+=1)='True' else resp$(resp+=1)='False'
		fnChk(j+5,pos_col(7),'',1) : if dedFica(j)>0 then resp$(resp+=1)='True' else resp$(resp+=1)='False'
		fnChk(j+5,pos_col(8),'',1) : if dedSt(j)>0 then resp$(resp+=1)='True' else resp$(resp+=1)='False'
		fnChk(j+5,pos_col(9),'',1) : if dedUc(j)>0 then resp$(resp+=1)='True' else resp$(resp+=1)='False'
		linecount=j+5
		fnQgl(linecount,pos_col(10),0,2,1)
		resp$(resp+=1)=fnRgl$(dedGl$(j))
	next j
	fnCmdKey('&Next',1,1,0,'Moves to next screen of company information.')
	fnCmdKey('&Save and Exit',4,0,0,'Saves any changes and returns to menu.')
	fnCmdKey('&Back',2,0,0,'Returns to previous screen.')
	fnCmdKey('&Cancel',5,0,1,'Returns to menu without saving any changes on any screen.')
	ckey=fnAcs(mat resp$)
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
		if resp$(resp+=1)='True' then newdedfed(j)=1 else newdedfed(j)=0
		if resp$(resp+=1)='True' then dedFica(j)=1 else dedFica(j)=0
		if resp$(resp+=1)='True' then dedSt(j)=1 else dedSt(j)=0
		if resp$(resp+=1)='True' then dedUc(j)=1 else dedUc(j)=0
		dedGl$(j)=fnAgl$(resp$(resp+=1))
	next j
	if ckey=2 then goto Screen1
	if ckey=4 then goto Finis
	if ckey=5 then goto DoCancel
	! if ckey=1 then goto Screen3
goto Screen3 ! /r

Screen3: ! r:
	fnTos
	mylen=32: mypos=mylen+3 : right=1
	fnLbl(1,10,'STATE CODES AND UNEMPLOYMENT INFORMATION',0,0)
	fnLbl(3,1,'Code State Name     State ID    U/C Maximum      U/C Rate',0,0)
	resp=0

	for j=1 to 10
		fnLbl(j+3,3,str$(j),mylen,0,0)
		fnTxt(j+3,6,8,0,left,'',0,'Enter your state name.',0 )
		resp$(resp+=1)=d$(j)
		fnTxt(j+3,19,12,0,left,'',0,'Enter the state id #.',0 )
		resp$(resp+=1)=e$(j)
		fnTxt(j+3,32,12,0,left,'10',0,'Enter the maximum wage subject to state unemployment (See your state u/c report.',0 )
		resp$(resp+=1)=str$(xm(j))
		fnTxt(j+3,49,8,0,left,'33',0,'Enter the state unemployment rate (See your state u/c report. Enter in percent format. Example: 5% as 5.00',0 )
		resp$(resp+=1)=str$(xr(j))
	next j
	fnCmdKey('&Next',1,1,0,'Moves to next screen of company information.')
	fnCmdKey('&Save and Exit',4,0,0,'Saves any changes and returns to menu.')
	fnCmdKey('&Back',2,0,0,'Returns to previous screen.')
	fnCmdKey('&Cancel',5,0,1,'Returns to menu without saving any changes on any screen.')
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto DoCancel
	resp=0
	for j=1 to 10
		d$(j)=resp$(resp+=1)
		e$(j)=resp$(resp+=1)
		xm(j)=val(resp$(resp+=1))
		xr(j)=val(resp$(resp+=1))
	next j
	if ckey=2 then goto Screen2
	if ckey=4 then goto Finis
	if ckey=5 then goto DoCancel
goto Screen4 ! /r

Screen4: ! r:
	fnTos
	mylen=45: mypos=mylen+3 : right=1: resp=0
	fram3=1: fnFra(1,1,6,60,'Vacation and Sick Pay Information')
	fnLbl(1,1,'Days employed before accruing sick hours',mylen,right,0,fram3)
	fnTxt(1,mypos,8,0,0,'30',0,'',fram3)
	resp$(1)=str$(sck(1))
	fnLbl(2,1,'Sick hours accrued after eligibility period:',mylen,right,0,fram3)
	fnTxt(2,mypos,8,0,0,'33',0,'',fram3)
	resp$(2)=str$(sck(2))
	fnLbl(3,1,'Sick hours to accrue once eligible:',mylen,right,0,fram3)
	fnTxt(3,mypos,8,0,0,'33',0,'',fram3)
	resp$(3)=str$(sck(3))
	fnLbl(4,1,'Maximum Sick Hours:',mylen,right,0,fram3)
	fnTxt(4,mypos,8,0,0,'32',0,'',fram3)
	resp$(4)=str$(sck(4))
	fnLbl(5,1,'Maximum Vacation Hours:',mylen,right,0,fram3)
	fnTxt(5,mypos,8,0,0,'32',0,'',fram3)
	resp$(5)=str$(vacm)
	fram4=2: fnFra(9,1,3,60,'Miscellaneous Payroll Information')
	fnLbl(1,1,'Minimum Hourly Wage:',mylen,right,0,fram4)
	fnTxt(1,mypos,10,0,0,'10',0,'',fram4)
	resp$(6)=str$(mhw)
	fnLbl(2,1,'Local Withholding Code:',mylen,right,0,fram4)
	fnTxt(2,mypos,2,0,0,'30',0,'If one of the twenty miscellaneous dedutions is used for local withholdings, then enter the number of the deduction.',fram4)
	resp$(7)=str$(loccode)
	fram5=3: fnFra(14,1,5,60,'Workman''s Compensation Limits')
	fnLbl(1,1,'Monthly:',mylen,right,0,fram5)
	fnTxt(1,mypos,10,0,0,'10',0,'If you pay workman''s comp and there are limits on the maximum wage subject to Workman''s comp, then enter that maximum wage under the proper pay period.',fram5)
	resp$(8)=str$(wcm(1))
	fnLbl(2,1,'Semi-Monthly:',mylen,right,0,fram5)
	fnTxt(2,mypos,10,0,0,'10',0,'If you pay workman''s comp and there are limits on the maximum wage subject to Workman''s comp, then enter that maximum wage under the proper pay period.',fram5)
	resp$(9)=str$(wcm(2))
	fnLbl(3,1,'Bi-Weekly:',mylen,right,0,fram5)
	fnTxt(3,mypos,10,0,0,'10',0,'If you pay workman''s comp and there are limits on the maximum wage subject to Workman''s comp, then enter that maximum wage under the proper pay period.',fram5)
	resp$(10)=str$(wcm(3))
	fnLbl(4,1,'Weekly:',mylen,right,0,fram5)
	fnTxt(4,mypos,10,0,0,'10',0,'If you pay Workman''s comp and there are limits on the maximum wage subject to Workman''s comp, then enter that maximum wage under the proper pay period.',fram5)
	resp$(11)=str$(wcm(4))
	fnCmdKey('&Next',1,1,0,'Moves to next screen of company information.')
	fnCmdKey('&Save and Exit',4,0,0,'Saves any changes and returns to menu.')
	fnCmdKey('&Back',2,0,0,'Returns to previous screen.')
	fnCmdKey('&Cancel',5,0,1,'Returns to menu without saving any changes on any screen.')
	ckey=fnAcs(mat resp$)
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
	if ckey=2 then goto Screen3
	if ckey=4 then goto Finis
	! if ckey=1 then goto Screen5
goto Screen5 ! /r

Screen5: ! r:
	goto Finis ! we'll need the job cost screen someday
	if ckey=2 then goto Screen4
	if ckey=5 then goto Finis

	close #106: ioerr ignore
	dim win6$*40
	if fnstyp=14 then win6$='SRow=14,SCol=12,ECol=67'
	if fnstyp<>14 then win6$='SRow=09,SCol=11,ECol=69'
	open #106: win6$&',ERow=22,Border=SR,Caption=<'&env$('program_caption'),display,outIn
	pr #106: newpage
	pr #106,fields '2,2,Cr 44,N': 'ACS General Ledger Installed (Y/N):'
	pr #106,fields '3,2,Cr 44,N': 'Days employed before accruing Sick Hours:'
	pr #106,fields '4,2,Cr 44,N': 'Sick Hours accrued after eligibility period:'
	pr #106,fields '5,2,Cr 44,N': 'Sick Hours to accrue each time accumulated:'
	pr #106,fields '6,2,Cr 44,N': 'Maximum Sick Hours:'
	pr #106,fields '7,2,Cr 44,N': 'Maximum Vacation Hours:'
	pr #106,fields '8,2,Cr 44,N': 'Minimum Hourly Wage:'
	dim io6$(11)*16
	io6$(1)='2,47,Cu 1,UT,N'
	io6$(2)='3,47,N 8.3,UT,N'
	io6$(3)='4,47,N 8.3,UT,N'
	io6$(4)='5,47,N 8.3,UT,N'
	io6$(5)='6,47,N 6.2,UT,N'
	io6$(6)='7,47,N 6.2,UT,N'
	io6$(7)='8,47,N 6.2,UT,N'
	if fnstyp=11 then
		! (Job Cost Questions)
		pr #106,fields '10,2,Cr 50,N': 'Retain Transactions until Jobs are complete (Y/N):'
		pr #106,fields '11,2,Cr 50,N': 'Starting of Range for Non-Productive Jobs:'
		pr #106,fields '12,2,Cr 50,N': 'Ending of Range for Non-Productive Jobs:'
		pr #106,fields '13,2,Cr 50,N': 'Number of Deduction used for Union Dues (if any):'
		io6$(8)='10,53,Cu 1,UT,N' : io6$(9)='11,53,C 6,UT,N'
		io6$(10)='12,53,C 6,UT,N' : io6$(11)='13,53,Nz 2,UT,N'
		if tc=1 then tc$='Y' else tc$='N'
	end if
	pr f '23,25,C 09,B,1': 'Next (F1)'
	pr f '23,35,C 09,B,2': 'Back (F2)'
	pr f '23,45,C 09,B,5': 'Finis (F5)'
	if gli=1 then gli$='Y' else gli$='N'
L2740: !
	if fnstyp=11 then
		rinput #106,fields mat io6$: gli$,mat sck,vacm,mhw,tc$,mat jn$,dc conv CONV7
	else
		rinput #106,fields mat io6$: gli$,mat sck,vacm,mhw conv CONV7
	end if
	if ce>0 then io6$(ce)(ce1:ce2)='U': ce=0
	if ckey>0 then goto L2840 else ce=curfld+1
	if ce>udim(io6$) then ce=1
L2790: io6$(ce)=rtrm$(io6$(ce)) : ce1=pos(io6$(ce),'U',1)
	ce2=ce1+1 : io6$(ce)(ce1:ce1)='UC' : goto L2740
CONV7: if ce>0 then io6$(ce)(ce1:ce2)='U'
	ce=cnt+1
ERR7: pr f '24,78,C 1': bell : goto L2790
L2840: if gli$<>'Y' and gli$<>'N' then ce=1 : goto ERR7
	if gli$='Y' then gli=1 else gli=0
	if fnstyp=14 then goto L2900
	if tc$='Y' then tc=1 else tc=0
	if fnstyp=11 and tc$<>'Y' and tc$<>'N' then ce=8 : goto ERR7
	if dc<0 or dc>10 then ce=11: goto ERR7
	L2900: !
	if ckey=5 or ckey=1 then goto DoCancel
	if ckey=2 then goto Screen5
goto L2740 ! /r
DoCancel: ! r:
! 	mat ml$(2)
! 	ml$(1)='You have chosen to exit without saving any changes.'
! 	ml$(2)='Save changes now?'
! 	fnMsgBox(mat ml$,resp$,'',52)
! if resp$='Yes' then goto Finis else goto Xit
goto Xit
! /r

Finis: ! r:
	open #hCompany=fnH: 'Name=[Q]\PRmstr\Company.h[cno]',i,outi,r
	ssmax=ssmax*.1
	rewrite #hCompany,using fPrCompany$,rec=1: mat a$,fid$,mcr,mcm,feducrat,mat d$,loccode,feducmax,ficarate,ssmax,ficawh,mat xm,mat xr,mat e$,mat gln$,gli,mat dedcode,mat calcode,mat dedfed,mat rpnames2$,mat sck,vacm,mhw,mat wcm,tc,mat jn$,dc
	close #hCompany:
	fnDedNames(mat fullname$,mat abrevname$,mat newdedcode,mat newcalcode,mat newdedfed,mat dedFica,mat dedSt,mat dedUc,mat dedGl$,1)
goto Xit ! /r

Xit: fnXit
include: ertn
