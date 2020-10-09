! REPLACE S:\acsPR\newprElecW2correction
! never could get it work with an RO record if all zeroes; without
! RO record i had to put an RE record in front of every RW record and
! had to follow with an RT record - Right now this program will not
! create an RO record
!
	autoLibrary
	on error goto Ertn
!
	dim em$(3)*30,ss$*11,d(14),tcp(32),s(9),t(9),z$*8,message$*40
	dim tdc(10),newdedcode(20),newcalcode(20),newdedfed(20),dedfica(20)
	dim dedst(20),deduc(20),abrevname$(20)*8,fullname$(20)*20
	dim a$(3)*40
	dim b$*12
	dim d$(10)*8
	dim e$(10)*12
	dim s2(2)
	dim fa$(1),fb$(1),fc$(1),fd$(1),l$(10),dedfed(10),w3(2),i2(2),t2(2)
	dim emppin$*17
	dim tlcn$*6
	dim contact$*27,contactph$*15,phoneext$*5,email$*40
	dim ml$(2)*80,orgw32$*11,w32$*11
	dim w2(9),i1(9),t1(9),ct$*20,st$*2,ibm$*8,namcde$*1,typemp$*1,io1$(18)
	dim orgt1(9),orgt2(2)
	dim orgw2(9),orgw3(2)
	dim terminat$*1,first$*15,mid$*15,last$*20,resp$(20)*40,path$*30
!
	fnTop(program$)
	on fkey 5 goto Xit
!
	open #1: "Name=[Q]\PRmstr\Company.h[cno],Shr",internal,input
	read #1,using L280: mat a$,b$,mat d$,loccode,mat e$,mat dedfed,oldmax
L280: form pos 1,3*c 40,c 12,pos 150,10*c 8,n 2,pos 317,10*c 12,pos 638,10*n 1,pos 239,pd 4.2
	close #1:
!
DATE_SCREEN: !
L320: fnTos(sn$="W2-1")
	rc=cf=0: mylen=34 : mypos=mylen+3
	fnFra(1,1,3,60,"Date Range for Corrected W2's","Normally this would the first and last day of the calendar year",0)
	cf+=1 : fratype=cf
	fnLbl(1,1,"Starting Date:",mylen,1,0,1)
	fnTxt(1,mypos,10,0,1,"3",0,"First day of calendar year",1)
	resp$(rc+=1)=str$(beg_date)
	fnLbl(2,1,"Ending Date:",mylen,1,0,1)
	fnTxt(2,mypos,10,0,1,"3",0,"Last day of calendar year",1)
	resp$(rc+=1)=str$(end_date)
	fnLbl(3,1,"Output File Designation and Name:",mylen,1,0,1)
	fnTxt(3,mypos,30,0,0,"",0,"Destination and file name you wish to use.",1)
	resp$(rc+=1)="c:\w2report"
	fnFra(7,1,3,60,"Date Range used on Original W2's","This could be any date rqnge entered by mistake",0)
	cf+=1 : fratype=cf
	fnLbl(1,1,"Original Starting Date:",mylen,1,0,2)
	fnTxt(1,mypos,10,0,1,"3",0,"First day of calendar year used on original submission of W2s",2)
	resp$(rc+=1)=str$(orgbeg_date)
	fnLbl(2,1,"Original Ending Date:",mylen,1,0,2)
	fnTxt(2,mypos,10,0,1,"3",0,"Last day of calendar year used on original submission of W2s",2)
	resp$(rc+=1)=str$(orgend_date)
	fnCmdKey("Next",1,1,0,"Prints the report")
	fnCmdKey("Cancel",5,0,1,"Returns to menu")
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	beg_date=val(resp$(1))
	end_date=val(resp$(2))
	path$=resp$(3)
	orgbeg_date=val(resp$(4))
	orgend_date=val(resp$(5))
	if beg_date=0 or end_date=0 then goto L320
	if orgbeg_date=0 or orgend_date=0 then goto L320
	fnconsole(1)
L560: p1=pos(b$,"-",1)
	if p1=0 then goto L600
	b$(p1:p1)=""
	goto L560
L600: !
	b1=val(b$)
	p1=pos(a$(3),",",1): comma=1
	if p1=0 then p1=pos(a$(3)," ",1): comma=0
	ct$=a$(3)(1:p1-1)
	if comma=1 then st$=a$(3)(p1+2:p1+3) else st$=a$(3)(p1+1:p1+2)
	p2=len(rtrm$(a$(3)))
	p1=p2-4
	zip$=a$(3)(p1:p2)
	if val(date$(1:2))-1 <70 then yr=2000+val(date$(1:2))-1 else yr=1900+val(date$(1:2))-1
	if date$(4:5)="12" then yr=yr+1 ! if you run in december, add 1 year back
	io1$(1)="5,25,C 40,UT,N"
	io1$(2)="6,25,C 40,UT,N"
	io1$(3)="7,25,C 20,UT,N"
	io1$(4)="8,25,C 2,UT,N"
	io1$(5)="9,25,C 5,UT,N"
	io1$(6)="10,25,N 9,UT,N"
	io1$(7)="11,25,N 4,UT,N"
	io1$(8)="12,25,N 10.2,UT,N"
	io1$(9)="13,25,N 10.4,UT,N"
	io1$(10)="14,25,N 10.2,UT,N"
	io1$(11)="15,25,N 10.4,UT,N"
	io1$(12)="16,75,N 2,UT,N"
	io1$(13)="17,47,N 2,UT,N"
	io1$(14)="18,61,N 2,UT,N"
	io1$(15)="19,65,N 2,UT,N"
	io1$(16)="20,35,C 8,UT,N"
	io1$(17)="21,52,C 1,UT,N"
	io1$(18)="22,38,C 1,UT,N"
	ibm$="IBM"
	namcde$="F"
	typemp$="R"
!
SCR1: !
	goto L1220
	fnTos(sn$="ElecW2-2")
	rc=cf=0: mylen=30 : mypos=mylen+3
	fnLbl(1,1,"Position Diskette in Drive A",mylen+25,1,0,0)
	fnLbl(3,1,"Company Name:",mylen,1,0,0)
	fnTxt(3,mypos,40,0,0,"",0,"Enter the name of the company submitting the files",0)
	resp$(rc+=1)=a$(1)
	fnLbl(4,1,"Street Address:",mylen,1,0,0)
	fnTxt(4,mypos,40,0,0,"",0,"Enter the address of the company submitting the files",0)
	resp$(rc+=1)=a$(2)
	fnLbl(5,1,"City:",mylen,1,0,0)
	fnTxt(5,mypos,22,0,0,"",0,"Enter the city of the company submitting the files",0)
	resp$(rc+=1)=ct$
	fnLbl(6,1,"State:",mylen,1,0,0)
	fnTxt(6,mypos,2,0,0,"",0,"Enter the state forthe company being submitted.",0)
	resp$(rc+=1)=st$
	fnLbl(7,1,"Zip Code:",mylen,1,0,0)
	fnTxt(7,mypos,5,0,0,"",0,"Enter the zip code for the company being submitted.",0)
	resp$(rc+=1)=zip$
	fnLbl(8,1,"Federal ID #:",mylen,1,0,0)
	fnTxt(8,mypos,9,0,0,"30",0,"Enter the Federal Id number without slashes or dashes.",0)
	resp$(rc+=1)=str$(b1)
	fnLbl(9,1,"Payment Year:",mylen,1,0,0)
	fnTxt(9,mypos,4,0,0,"30",0,"Enter the year for which the wages were paid in ccyy format.",0)
	resp$(rc+=1)=str$(yr)
	fnLbl(10,1,"Social Security Maximum Wage:",mylen,1,0,0)
	fnTxt(10,mypos,10,0,0,"10",0,"Enter the social security maximum wage for the year just completed.",0)
	resp$(rc+=1)=str$(ssmax)
	fnLbl(11,1,"Social Security Rate:",mylen,1,0,0)
	fnTxt(11,mypos,6,0,0,"34",0,"Enter the social security rate for the year just completed.",0)
	resp$(rc+=1)=str$(ssrate)
	fnLbl(12,1,"Medicare Maximum Wage:",mylen,1,0,0)
	fnTxt(12,mypos,10,0,0,"10",0,"Enter the medicare maximum wage for the year just completed.",0)
	resp$(rc+=1)=str$(mcmax)
	fnLbl(13,1,"Medicare Rate:",mylen,1,0,0)
	fnTxt(13,mypos,6,0,0,"34",0,"Enter the medicare rate for the year just completed.",0)
	resp$(rc+=1)=str$(mcrate)
	fnCmdKey("Next",1,1,0,"Proceed with submission")
	fnCmdKey("Cancel",5,0,1,"Returns to menu")
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
L1220: pr newpage
	close #101: ioerr L1240
L1240: open #101: "SROW=2,SCOL=3,EROW=23,ECOL=77,BORDER=DR,CAPTION=<Create Electronic W2 Diskette for I.R.S.",display,outIn
	pr f "3,15,C 51,R,N": "  INSERT DISKETTE FOR ELECTRONIC W2'S IN DRIVE A:"
	pr f "5,5,C 60": "Company Name:"
	pr f "6,5,C 60": "Street Address:"
	pr f "7,5,C 60": "City:"
	pr f "8,5,C 60": "State:"
	pr f "9,5,C 60": "Zip Code:"
	pr f "10,5,C 60": "Federal ID #:"
	pr f "11,5,C 60": "Payment Year:"
	pr f "12,5,C 60": "Soc-Sec Maximum:"
	pr f "13,5,C 60": "Soc-Sec Rate:"
	pr f "14,5,C 60": "Medicare Maximum:"
	pr f "15,5,C 60": "Medicare Rate:"
	pr f "16,5,C 70": "Miscellaneous Deduction Containing Employer Cost Group-Term Life Ins:"
	pr f "17,5,C 70": "Miscellaneous Deduction Used For Pension:"
	pr f "18,5,C 70": "Miscellaneous Deduction Used For Deferred Compensation:"
	pr f "19,5,C 70": "Miscellaneous Deduction Used For Dependent Care Assistance:"
	pr f "20,5,C 60": "Computer Manufacturer's Name:"
	pr f "21,5,C 60,N": "F=First Name First or S=Surname First on File:"
	pr f "22,5,C 60": "Type of Business Code R=Regular:"
	pr f "24,28,C 9,B,1": "Next (F1)"
	pr f "24,39,C 11,B,5": "Cancel (F5)"
	pr f mat io1$: a$(1),a$(2),ct$,st$,zip$,b1,yr,87900,.062,999999,.0145,ins,pen,dfc,dcan,ibm$,namcde$,typemp$
L1470: input fields mat io1$,attr "R": a$(1),a$(2),ct$,st$,zip$,b1,yr,ssmax,ssrate,mcmax,mcrate,ins,pen,dfc,dcan,ibm$,namcde$,typemp$ conv CONV1
	if ce>0 then io1$(ce)(ce1:ce2)="U": ce=0
	if cmdkey>0 then goto L1560 else ce=curfld+1
	if ce>udim(io1$) then ce=1
L1510: io1$(ce)=rtrm$(uprc$(io1$(ce))) : ce1=pos(io1$(ce),"U",1)
	ce2=ce1+1 : io1$(ce)(ce1:ce1)="UC" : goto L1470
CONV1: if ce>0 then io1$(ce)(ce1:ce2)="U"
	ce=cnt+1
ERR1: pr f "24,78,C 1": bell : goto L1510
L1560: !
	if cmdkey=5 then goto Xit
	if rtrm$(a$(1))="" then ce=1: goto ERR1
	if rtrm$(a$(2))="" then ce=2: goto ERR1
	if rtrm$(ct$)="" then ce=3: goto ERR1
	if rtrm$(st$)="" then ce=4: goto ERR1
	if rtrm$(zip$)="" then ce=5: goto ERR1
	if b1=0 then ce=6: goto ERR1
	if yr<2001 then ce=7: goto ERR1
	ficarate=ssrate+mcrate
	if ssmax<53400 then ce=8: goto ERR1
	if ins<0 or ins>10 then ce=9: goto ERR1
	if pen<0 or pen>10 then ce=10: goto ERR1
	if dfc<0 or dfc>10 then ce=11: goto ERR1
!
	mat io1$(2)
	io1$(1)="12,71,N 2,UT,N"
	io1$(2)="14,71,N 2,UT,N"
	close #101: ioerr L1750
L1750: pr newpage
	open #101: "SROW=7,SCOL=2,EROW=16,ECOL=79,BORDER=DR,CAPTION=<Electronic W-2   State Reporting Information",display,outIn
	pr f "8,4,C 72": "Some states require filing W2's on diskette.  Answer the following"
	pr f "9,4,C 72": "questions if you wish to create 'RS' records during this run."
	pr f "12,8,Cr 62": "State code used in your record to identify the selected state:"
	pr f "14,8,Cr 62": "Appropriate FIPS postal numeric code:"
	pr f "15,8,C 70": "(See an appendix in your electronic booklet for the postal code!)"
	pr f "17,28,C 9,B,1": "Next (F1)"
	pr f "17,39,C 11,B,5": "Cancel (F5)"
L1840: input fields mat io1$: sr1,sr2 conv L1840
	if ce>0 then io1$(ce)(ce1:ce2)="U": ce=0
	if cmdkey>0 then goto L1930 else ce=curfld+1
	if ce>udim(io1$) then ce=1
L1880: io1$(ce)=rtrm$(uprc$(io1$(ce))) : ce1=pos(io1$(ce),"U",1)
	ce2=ce1+1 : io1$(ce)(ce1:ce1)="UC" : goto L1840
CONV2: if ce>0 then io1$(ce)(ce1:ce2)="U"
	ce=cnt+1
ERR2: pr f "24,78,C 1": bell : goto L1880
L1930: if cmdkey=5 then goto Xit
	if sr1<0 or sr1>udim(e$) then ce=1: goto ERR2
	if sr1>0 and sr2=0 then ce=2: goto ERR2
!
	gosub SCR2
	pr newpage
	win=101
	message$=""
!
	fnDedNames(mat fullname$,mat abrevname$,mat newdedcode,mat newcalcode,mat newdedfed,mat dedfica,mat dedst,mat deduc)
	open #1: "Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-no.h[cno],Shr",internal,input,keyed
! Open #2: "Name=[Q]\PRmstr\RPTRAIL.h[cno],Shr",Internal,Input,Relative
	open #4: "Name=[Q]\PRmstr\payrollchecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno]",internal,outIn,keyed
L2070: open #22: "Name=W2REPORT,RecL=1024,eol=crlf,replace",display,output
	goto L2140
	pr newpage
	msgline$(1)="Insert Diskette"
	mtype=1
	if err=4221 then gosub L4930 ! fnOldMsgBox(MAT RESPONSE$,CAP$,MAT MSGLINE$,MTYPE)
	goto L2070
L2140: gosub RECRCA
! GOSUB RECRE
L2160: ! pr f "12,32,N 3,UT,N": R1/LREC(1)*100
L2170: read #1,using L2190: eno,mat em$,ss$,em6 eof END1
	gosub L4380
L2190: form pos 1,n 8,3*c 30,c 11,pos 122,n 2
	r1=r1+1
	p1=pos(em$(3),",",1) : comma=1
	if p1=0 then p1=pos(em$(3)," ",1): comma=0
	emct$=em$(3)(1:p1-1)
	gosub L5770: emst$=holdst$ ! If COMMA=1 Then eMST$=EM$(3)(P1+2:P1+3) Else eMST$=EM$(3)(P1+1:P1+2)
	p2=len(rtrm$(em$(3)))
	p1=p2-4
	emzip$=em$(3)(p1:p2)
L2280: p1=pos(ss$,"-",1)
	if p1>0 then ss$(p1:p1)="": goto L2280 else ssn=val(ss$)
	checkkey$=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",0)&cnvrt$("pd 6",0) ! index employee#,department# and payroll date
	restore #4,key>=checkkey$: nokey L2170
L2320: read #4,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat tcp eof L2680
	if heno<>eno then goto L2680
	if prd<beg_date or prd>end_date then goto L2320 ! not this year
	form pos 1,n 8,pos 48,n 2,pos 168,21*pd 5.2,pos 468,pd 3
	if tcd<1 or tcd>10 then tcd=1
! finish with this employee
	dedret=0
	cafded=0
	for j=1 to 20
		if newdedfed(j)=1 then goto L2420 else goto L2430
L2420: dedret=dedret+tcp(j+4)
L2430: if dedfica(j)=1 then goto L2440 else goto L2450
L2440: cafded=cafded+tcp(j+4)
L2450: next j
	w2(1)=min(w2(1)+tcp(31)-tcp(30)-cafded,ssmax-tcp(30)) ! TOTAL SOC-SEC WAGES
	w3=w3+tcp(2) ! TOTAL FICA WITHHELD
	w3(1)=w3(1)+tcp(31)-cafded ! TOTAL MEDICARE WAGES & TIPS
! if env$('client')="Washington Parrish" then w3(1)=w3(1)+tcp(5) ! add deferred comp match to medicare wages
	w3(1)=min(mcmax,w3(1)) ! MC WAGES CANNOT EXCEED MAXIMUM
	if uprc$(med$)="Y" then w2=w2+tcp(2) else w2=round(min(w3/(ssrate+mcrate)*ssrate,ssmax*ssrate),2) ! SS WH
	if uprc$(med$)="Y" then w3(2)=w3(2)+tcp(3) else w3(2)=w3-w2 ! MEDICARE WITHHELD
	w2(2)=w2(2)+tcp(30) ! FICA TIPS YTD
	w2(3)=w2(3)+tcp(31)-dedret ! TOTAL FEDERAL WAGES
	w2(4)=w2 ! W2(4)+tcp(2) ! FICA W/H YTD       (COULD BE +W2 INSTEAD OF +tcp(2)IF ONLY FICA PORTION GOES INTO W2 RECORD)
! w2(4)=W2 ! PUT SS W/H ONLY IN 2-W RECORD (EXCLUDE MEDICARE W/H)
	w2(5)=w2(5)+tcp(1) ! FED W/H YTD
	if ins>0 then w2(6)=w2(6)+tcp(4+ins) ! EMPLOYER COST GROUP LIFE INS.
	w2(7)=w2(7)+0 ! UNCOLLECTED EMPLOYEE FICA TAX ON TIPS
	w2(8)=w2(8)+tcp(24) ! EIC TOTAL
	w2(9)=w2(9)+0 ! ALLOCATED TIPS
	if dfc>0 then dc1=dc1+tcp(4+dfc)*100 ! DEFERRED COMPENSATION
	if dcan>0 then dca=dca+tcp(4+dcan)*100 ! DEPENDENT CARE ASSISTANCE
	if sr1><tcd then goto L2670
	s2(1)=s2(1)+(tcp(31)*100)
	s2(2)=s2(2)+(tcp(4)*100)
L2670: goto L2320
L2680: gosub EXTRACT_ORIGINAL
	if em6=9 then w2(1)=w2(4)=w3(1)=w3(2)=0: orgw2(1)=orgw2(4)=orgw3(1)=orgw3(2)=0 ! NO SS OR MC
	if em6=1 then w3(1)=w3(2)=0: orgw3(1)=orgw3(2)=0 ! NO MEDICARE
	if em6=2 then w2(1)=w2(4)=0: orgw2(1)=orgw2(4)=0 ! NO SOC-SEC
	if w2(3)=0 and w2(1)=0 then goto L2160
	if sum(w2)=sum(orgw2) and sum(w3)=sum(orgw3) then goto L2160 ! no changes to any dollar figusres
	gosub RECRCE
	gosub RECRCW
	gosub RECRS
	tw1=tw1+1
	tw2=tw2+1
	gosub RECRCT
	tw2=0
goto L2160
 
RECRCA: ! r:
	pr #22,using L2830: "RCA",b1,emppin$(1:8),"","98",a$(1),"",a$(2)(1:22),ct$,st$,zip$,"","","","","",contact$,contactph$,phoneext$,"",email$,"","","2","L","1",tlcn$,""
	L2830: form pos 1,c 3,pic(#########),c 8,c 9,c 2,c 57,c 22,c 22,c 22,c 2,c 5,c 4,c 6,c 23,c 15,c 2,c 27,c 15,c 5,c 3,c 40,c 3,c 10,c 1,c 1,c 1,c 6,c 701
return ! /r
RECRCE: ! r:
	pr #22,using L2870: "RCE",yr,"",b1,"","","","",a$(1)(1:37),"",a$(2)(1:22),ct$,st$,zip$(1:5),"","","","","","","R","","",""
	L2870: form pos 1,c 3,pic(####),c 9,pic(#########),c 1,c 9,c 4,c 4,c 57,c 22,c 22,c 22,c 2,c 5,c 4,c 4,c 23,c 15,c 2,c 1,c 1,c 1,c 1,c 799
return ! /r
RECRCW: ! r:
	for j=1 to 9
		w2(j)=w2(j)*100
		orgw2(j)=orgw2(j)*100
	next j
	for j=1 to 2
		w3(j)=w3(j)*100
		orgw3(j)=orgw3(j)*100
	next j
	if pen=0 then pen$=" " else pen$="1"
	if dfc=0 then dfc$="" else dfc$="D"
	! medicare withholdings  exceptions
	orgw32$="           ": w32$="           "
	if orgw3(2)=w3(2) then orgw32$="           ": w32$="           " : goto L3100 ! if amounts are the same,then report both as blanks
	if orgw3(2)>0 or w3(2)>0 then orgw32$=cnvrt$("pic(###########)",orgw3(2)): w32$=cnvrt$("pic(###########)",w3(2)) ! try to stop accuwage errors when either the original medicare withholdings or the new one is >0
	L3100: ! federal withholdings exceptions
	w25$="           ": orgw25$="           "
	if w2(5)=orgw2(5) then goto L3140 ! try to stop accuwage errors when both both federal wh are same
	if w2(5)>0 or orgw2(5)>0 then w25$=cnvrt$("pic(###########)",w2(5)): orgw25$=cnvrt$("pic(###########)",orgw2(5)) ! try to stop accuwage errors when both both federal wh are zeros on indiviuals
	L3140: !  total wages
	w23$="           ": orgw23$="           "
	if w2(3)=orgw2(3) then goto L3180 ! try to stop accuwage errors when both both total wages
	if w2(3)>0 or orgw2(3)>0 then w23$=cnvrt$("pic(###########)",w2(3)): orgw23$=cnvrt$("pic(###########)",orgw2(3)) ! try to stop accuwage errors when both both total wages are zeros on indiviuals
	L3180: ! ss wages
	w21$="           ": orgw21$="           "
	if w2(1)=orgw2(1) then goto L3220 ! try to stop accuwage errors when both both ss wages agree
	if w2(1)>0 or orgw2(1)>0 then w21$=cnvrt$("pic(###########)",w2(1)): orgw21$=cnvrt$("pic(###########)",orgw2(1)) ! try to stop accuwage errors when both both ss wages are zeros on indiviuals
	L3220: ! medicare wages
	w31$="           ": orgw31$="           "
	if w3(1)=orgw3(1) then goto L3260 ! try to stop accuwage errors when both both medicare wages agree
	if w3(1)>0 or orgw3(1)>0 then w31$=cnvrt$("pic(###########)",w3(1)): orgw31$=cnvrt$("pic(###########)",orgw3(1)) ! try to stop accuwage errors when both both medicare wages are zeros on indiviuals
	L3260: ! ss wh
	w24$="           ": orgw24$="           "
	if w2(4)=orgw2(4) then orgw2(4)=0: goto L3300 ! try to stop accuwage errors when both both medicare wages agree !! had to set the orgw2(4) to zero because I could never change the pr line to allow me to make it a alpha field  ??? still don't know why i cannot change it
	if w2(4)>0 or orgw2(4)>0 then w24$=cnvrt$("pic(###########)",w2(4)): orgw24$=cnvrt$("pic(###########)",orgw2(4)) ! try to stop accuwage errors when both both ss wh are zeros
	L3300: !
	pr #22,using L3310: "RCW",0,ssn,first$,mid$,last$,first$,mid$,last$,"",em$(2)(1:22),emct$,emst$,emzip$,"","","","","",orgw23$,w23$,orgw25$,w25$,orgw21$,w21$,orgw2(4),w24$,orgw31$,w31$,orgw32$,w32$,"","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",pen$,pen$,"","",""
	L3310: form pos 1,c 3,2*pic(#########),c 15,c 15,c 20,c 15,c 15,c 20,c 22,c 22,c 22,c 2,c 5,c 4,c 5,c 23,c 15,c 2,6*c 11,pic(###########),5*c 11,28*c 11,c 22,2*c 11,8*c 11,c 187,c 1,c 1,c 1,c 1,c 1,c 1,c 16
return ! /r
RECRS: ! r: STATE RECORD
	if sr1=0 then goto L3370 ! NO STATE SELECTED
	if s2(1)=0 and s2(2)=0 then goto L3370 ! NO STATE WAGES
	form pos 1,c 2,g 2,c 5,pic(#########),c 15,c 15,c 20,c 4,c 22,c 22,c 22,c 2,c 5,c 4,c 5,c 23,c 15,c 2,c 2,c 6,2*pic(###########),pic(##),2*pic(########),c 5,c 20,c 6,g 2,2*pic(###########),c 10,c 1,2*pic(###########),c 7,c 75,c 75,c 25
	L3370: !
	t1=t1+1: mat t1=t1+w2 : orgt1=orgt1+1: mat orgt1=orgt1+orgw2
	mat i1=i1+w2
	mat i2=i2+w3
	mat t2=t2+w3 : mat orgt2=orgt2+orgw3
	dc2=dc2+dc1
	dc3=dc3+dc1
	dca2=dca2+dca
	dca3=dca3+dca
	w2=w3=dca=dc1=orgw2=wrfw3=orgdca=orgdc1=0
	mat w2=(0)
	mat w3=(0)
	mat s2=(0)
	mat orgw2=(0)
	mat orgw3=(0)
return ! /r
RECRCT: ! r:
	! total wages
	t13$="               " : orgt13$="               "
	if orgt1(3)=t1(3) then goto L3590 ! both equal
	if orgt1(3)>0 or t1(3)>0 then orgt13$=cnvrt$("pic(###############)",orgt1(3)): t13$=cnvrt$("pic(###############)",t1(3)) ! try to stop accuwage errors when either total wages are >0
	L3590: ! ss wages
	t11$="               " : orgt11$="               "
	if orgt1(1)=t1(1) then goto L3630 ! both equal
	if orgt1(1)>0 or t1(1)>0 then orgt11$=cnvrt$("pic(###############)",orgt1(1)): t11$=cnvrt$("pic(###############)",t1(1)) ! try to stop accuwage errors when either total ss wages are >0
	L3630: ! ss wh
	t14$="               " : orgt14$="               "
	if orgt1(4)=t1(4) then goto L3670 ! both equal
	if orgt1(4)>0 or t1(4)>0 then orgt14$=cnvrt$("pic(###############)",orgt1(4)): t14$=cnvrt$("pic(###############)",t1(4)) ! try to stop accuwage errors when either total ss wh are >0
	L3670: ! total medicare withholding
	t22$="               " : orgt22$="               "
	if orgt2(2)=t2(2) then goto L3710 ! both equal
	if orgt2(2)>0 or t2(2)>0 then orgt22$=cnvrt$("pic(###############)",orgt2(2)): t22$=cnvrt$("pic(###############)",t2(2)) ! try to stop accuwage errors when either medicare withholdings are >0
	L3710: ! medicare wages
	orgt21$="               " : t21$="               "
	if orgt2(1)=t2(1) then goto L3750
	if orgt2(1)>0 or t2(1)>0 then orgt21$=cnvrt$("pic(###############)",orgt2(1)): t21$=cnvrt$("pic(###############)",t2(1)) ! try to stop accuwage errors when either the old or new is greater than zero
	L3750: ! federal wh
	orgt15$="             " : t15$="             "
	if t1(5)=orgt1(5) then goto L3800 ! both are same
	if t1(5)>0 or orgt1(5)>0 then t15$=cnvrt$("pic(###############)",t1(5)): orgt15$=cnvrt$("pic(###############)",orgt1(5)) ! try to stop accuwage errors when both were either fed wh >0
	L3800: !
	pr #22,using L3820: "RCT",tw2,orgt13$,t13$,orgt15$,t15$,orgt11$,t11$,orgt14$,t14$,orgt21$,t21$,orgt22$,t22$,orgt12$,t12$,"","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""
	dc3=0 ! kj 120805
	L3820: form pos 1,c 3,pic(#######),14*c 15,26*c 15,c 15,2*c 15,8*c 15,c 234
	form pos 1,c 2,pic(#######),7*pic(###############),c 240,9*pic(###############),c 23
	t1=0: orgt1=0: mat t1=(0) : mat orgt1=(0)
	mat t2=(0): mat orgt2=(0)
return ! /r
RECRCF: ! r:
	pr #22,using L3890: "RCF",tw1,""
	L3890: form pos 1,c 3,pic(#########),c 1012
return ! /r
END1: ! r:
! Gosub RECRCT
	gosub RECRCF
	gosub L3980
goto Xit ! /r
Xit: fnXit
L3980: ! r:
	dim a$*1024
	close #24: ioerr ignore
	close #22: ioerr ignore
	open #24: "Name=X,RecL=1025,EOL=NONE,REPLACE",external,output
	open #22: "Name=w2report,RecL=1024",display,input
	do
		linput #22: a$ eof L4080
		if a$(1024:1024)="X" then a$(1024:1024)=""
		write #24,using L4060: rpad$(a$,1024),chr$(10)
		L4060: form pos 1,c 1024,c 1
	loop
	L4080: !
	close #24:
	close #22:
	execute "COPY x "&path$&" -n"
return ! /r
SCR2: !
	dim contact$*27,email$*40
	win=101
	win_height=12: win_width=75: display_cnam=1: button_option=2: gosub L4560 ! fnWin3b(WIN,CAP$,12,62,1,2)
	pr #win,fields "04,2,Cr 31,N": "Personal ID Number:"
	pr #win,fields "05,2,Cr 31,N": "Resub Indicator:"
	pr #win,fields "06,2,Cr 31,N": "Resub TLCN:"
	pr #win,fields "07,2,Cr 31,N": "Contact Name:"
	pr #win,fields "08,2,Cr 31,N": "Contact Phone Number:"
	pr #win,fields "09,2,Cr 31,N": "Contact Phone Extension:"
	pr #win,fields "10,2,Cr 31,N": "Contact E-Mail:"
	pr #win,fields "11,2,Cr 31,N": "Terminating Business Indicator:"
	scr2_io$(1)="04,34,C 17,UT,N"
	scr2_io$(2)="05,34,C 01,UT,N"
	scr2_io$(3)="06,34,C 06,UT,N"
	scr2_io$(4)="07,34,C 27,UT,N"
	scr2_io$(5)="08,34,C 15,UT,N"
	scr2_io$(6)="09,34,C 05,UT,N"
	scr2_io$(7)="10,34,C 40,UT,N"
	scr2_io$(8)="11,34,C 01,UT,N"
	if resub$="" then resub$="0"
! If TLCN$="" Then tLCN$="0"
	if terminat$="" then terminat$="0"
L4210: rinput #win,fields mat scr2_io$: emppin$,resub$,tlcn$,contact$,contactph$,phoneext$,email$,terminat$ conv CONV_SCR2
	med$="Y"
	if ce>0 then scr2_io$(ce)(ce1:ce2)="U": ce=0
	if cmdkey>0 then goto L4310 else ce=curfld
L4250: ce=ce+1: if ce>udim(scr2_io$) then ce=1
L4260: scr2_io$(ce)=rtrm$(scr2_io$(ce)) : ce1=pos(scr2_io$(ce),"U",9) : if ce1=0 then goto L4250
	ce2=ce1+1 : scr2_io$(ce)(ce1:ce1)="UC" : goto L4210
CONV_SCR2: if ce>0 then scr2_io$(ce)(ce1:ce2)="U"
	ce=cnt+1
ERR_SCR2: pr f "24,78,C 1": bell : goto L4260
L4310: !
	if resub$<>"0" and resub$<>"1" then ce=2 : goto ERR_SCR2
	if resub$="1" and rtrm$(tlcn$)="" then ce=3 : goto ERR_SCR2
	if terminat$<>"0" and terminat$<>"1" then ce=8 : goto ERR_SCR2
	if uprc$(med$)="Y" or uprc$(med$)="N" then goto L4350 else ce=9: goto ERR_SCR2
L4350: close #win:
	if cmdkey=5 then goto SCR1
	return
L4380: dim first$*15,mid$*15,last$*20,em$(3)*30
	em$(1)=uprc$(rtrm$(em$(1))): ! nAMCDE$="s"
	x1=pos(em$(1)," ",1)
	x2=pos(em$(1)," ",x1+1)
	x3=pos(em$(1)," ",x2+1)
	if uprc$(namcde$)="S" then goto L4480
	first$=em$(1)(1:min(15,max(x1-1,1)))
	if x2>0 then mid$=em$(1)(x1+1:x2-1): last$=em$(1)(x2+1:len(em$(1)))
	if x2=0 then last$=em$(1)(x1+1:len(em$(1))): mid$=""
	goto L4540
L4480: ! last name first
	if x1=0 then x1=pos(em$(1),",",1)
	if x1>0 and em$(1)(x1-1:x1-1)="," then last$=em$(1)(1:x1-2) else last$=em$(1)(1:max(x1-1,1))
	if x2>0 then first$=em$(1)(x1+1:x2-1): mid$=em$(1)(x2+1:len(em$(1)))
	if x2=0 then first$=em$(1)(x1+1:len(em$(1)))(1:15): mid$=""
	x=pos(first$,",",1): if x>0 then first$(x:x)=""
L4540: ! pr FIRST$,MID$,LAST$
	return
L4560: ! def library fnWin3b(WIN,&CAP$,WIN_HEIGHT,WIN_WIDTH,DISPLAY_CNAM,BUTTON_OPTION)
! Win= Window number to be opened
! Cap$= Caption to be used
! Win_Height and Win_Width= seems pretty obvious don't it
! Display_CNam= 1. Display Cnam and Cno in top 2 lines of window
	!               2. Display only Cno in top 1 line
! button_option= 0. Does nothing.
	!                1. Cancel(F5)
	!                2. Next(F1),Cancel(F5)
	!                3. Print(F1),Cancel(F5)
	!                4. Save (F1),Cancel(F5)
	!                5. Next (F1),Cancel(F5),Search(F6)
	!                6. Next (F1),Back(F2),Cancel(F5)
	!                7. Save (F1),Delete(F4),Cancel(F5)
	!                8. (undefinded)
	! (button_option>0 is set to have Cancel(F5))
	if exists("C:\ACS\Local\Settings\No_Print_Newpage.txt") then goto L4640 else pr newpage
L4640: screen_width=80
	screen_height=24
	if display_cnam=0 then goto L4680
L4680: sc=max(int(((screen_width-win_width)/2)+1),2)
	ec=min(sc+win_width-1,79)
	sr=max(int(((screen_height-win_height)/2)+1),2)
	er=min(sr+win_height-1,23)
!     pr "win_height="&STR$(WIN_HEIGHT),"win_width="&STR$(WIN_WIDTH)
!     pr "sr="&STR$(SR),"sc="&STR$(SC)
!     pr "er="&STR$(ER),"ec="&STR$(EC) : Pause
	close #win: ioerr L4760
L4760: open #win: "SRow="&str$(sr)&",SCol="&str$(sc)&",ERow="&str$(er)&",ECol="&str$(ec)&",Border=Sr,Caption=<"&env$('program_caption'),display,outIn
	pr #win: newpage
	if display_cnam=0 then goto L4810
	if display_cnam=1 then
		pr #win,fields "1,1,Cc "&str$(win_width)&",R,N": env$('cnam')(1:min(40,win_width))
		pr #win,fields "2,1,Cc "&str$(win_width)&",R,N": "Company Number [cno]"(1:min(40,win_width))
	end if
	if display_cnam=2 then
		pr #win,fields "1,1,Cc "&str$(win_width)&",R,N": "Company Number [cno]"(1:min(40,win_width))
	end if
L4810: if button_option=0 then goto L4920
	mat fkey$=("") : em$="" : es=0
	fkey$(5)="Cancel" ! included by default
	if button_option=2 then
		fkey$(1)="Next"
	else if button_option=3 then
		fkey$(1)="Print"
	else if button_option=4 then
		fkey$(1)="Save"
	else if button_option=5 then
		fkey$(1)="Next"
		fkey$(6)="Search"
	else if button_option=6 then
		fkey$(1)="Next"
		fkey$(2)="Back"
	else if button_option=7 then
		fkey$(1)="Save"
		fkey$(4)="Delete"
	end if
	scrline=er+1: gosub L5560 !  fnFKEY(ER+1,MAT FKEY$,MAT DISFK,EM$,ES)
!
L4920: return  ! Fnend
L4930: ! mtype=0 means splash    - returns no response                                 ! mostly for "please wait..." and "printing..."                                 ! (anywhere no response is required - no buttons are displyed either)
! mtype=1 means OK only   - returns no response
! mtype=2 means Yes or No - returns "Y" or "N"
! mtype=3 means Yes, No, Cancel - returns "Y" or "N" or ""
! response$(1)= code you're looking for 2-5 are reserved for future use
	close #104: ioerr L4990
L4990: endrow=12
	for j=2 to udim(msgline$)
		if msgline$(j)<>"" then endrow=endrow+1
	next j
	open #104: "SRow=10,SCol=09,ERow="&str$(endrow)&",ECol=70,Border=SR,Caption=<"&env$('program_caption'),display,outIn
	pr #104: newpage
	mglinerow=2
	for j=1 to udim(msgline$)
		pr #104,fields str$(mglinerow+j-1)&",2,Cc 60,N": msgline$(j)
	next j
	if mtype=1 then pr f str$(endrow+1)&",38,Cc 4,B,1": "Ok"
	if mtype=1 then input fields str$(endrow)&",09,C 1,AE,N": pause$
	if mtype=2 then pr f str$(endrow+1)&",35,Cc 4,B,21": "Yes"
	if mtype=2 then pr f str$(endrow+1)&",40,Cc 4,B,22": "No"
L5130: if mtype=2 then input fields str$(endrow)&",09,Cu 1,AE,N": response$(1)
	if mtype=2 and cmdkey=22 then response$(1)="N"
	if mtype=2 and cmdkey=21 then response$(1)="Y"
	if mtype=2 and response$(1)<>"Y" and response$(1)<>"N" then pr f "24,1,C 7,N": bell$ : goto L5130
	if mtype=3 then pr f str$(endrow+1)&",29,Cc 4,B,21": "Yes"
	if mtype=3 then pr f str$(endrow+1)&",34,Cc 4,B,22": "No"
	if mtype=3 then pr f str$(endrow+1)&",39,C 12,B,22": "Cancel (Esc)"
	if mtype=3 then input fields str$(endrow)&",09,Cu 1,AE,N": response$(1)
	if mtype=3 and cmdkey=22 then response$(1)="N"
	if mtype=3 and cmdkey=21 then response$(1)="Y"
	if mtype=3 and cmdkey=99 then response$(1)=""
	if mtype=3 and response$(1)<>"Y" and response$(1)<>"N" and response$(1)<>"" then pr f "24,1,C 7,N": bell$ : goto L5130
	close #104: ioerr ignore
return  ! Fnend
! def library fnOPENWIN(WIN,SR,SC,ER,EC,&CAP$)
	if sr<1 then sr=10
	if sc<1 then sc=20
	if er<1 then er=14
	if ec<1 then ec=59
	win_width=ec-sc+1
	close #win: ioerr L5490
L5490: open #win: "SRow="&str$(sr)&",SCol="&str$(sc)&",ERow="&str$(er)&",ECol="&str$(ec)&",Border=Sr,Caption=<"&env$('program_caption'),display,outIn
	pr #win: newpage
	pr #win,fields "1,1,Cc "&str$(win_width)&",R,N": env$('cnam')(1:min(40,win_width))
	pr #win,fields "2,1,Cc "&str$(win_width)&",R,N": "Company Number [cno]"(1:min(40,win_width))
!
!
	return  ! Fnend
L5560: ! def library fnFKEY(SCRLINE,MAT FKEY$,MAT DISFK,&EM$,ES)
	totallen=0
	startpos=0
	for j=1 to udim(fkey$) ! add ' (Fx)' to each button
		if fkey$(j)="" then goto L5620
		fkey$(j)=fkey$(j)&" (F"&str$(j)&")"
		! add ' (Fx)' to each button
		totallen=totallen+len(fkey$(j))+1
L5620: next j
	totallen=totallen+len(rtrm$(em$))+min(len(rtrm$(em$)),1)+es
	totallen=totallen-1
	startpos=int((80-totallen)/2)+1
	pr f str$(scrline)&","&str$(startpos)&",C "&str$(totallen)&",N": rpt$("Ä",totallen)
	for j=1 to udim(fkey$)
		if fkey$(j)="" then goto L5730
		if disfk(j)=1 then pr f str$(scrline)&","&str$(startpos)&",C "&str$(len(fkey$(j)))&",R,"&str$(j): fkey$(j)
		if disfk(j)=1 then goto L5720
		pr f str$(scrline)&","&str$(startpos)&",C "&str$(len(fkey$(j)))&",B,"&str$(j): fkey$(j)
L5720: startpos=startpos+len(fkey$(j))+1
L5730: next j
	if rtrm$(em$)="" then goto L5760
	pr f str$(scrline)&","&str$(startpos)&",C "&str$(len(rtrm$(em$))+es)&",R,N": rtrm$(em$)
L5760: return  ! Fnend
L5770: ! extract state name
	holdst$="          "
	p3=oldp3=0
	p4=10
	for j=1 to 10
		p3=pos(rtrm$(em$(3))," ",p3+1)
		if oldp3>p3 then goto L5860 ! end of address reached
		if p3>0 then oldp3=p3 else goto L5850
L5850: next j
L5860: for j=1 to 10
		if rtrm$(em$(3)(oldp3-j:oldp3-j))="" or em$(3)(oldp3-j:oldp3-j)="," then goto L5880 else p4=p4-1: holdst$(p4:p4)=em$(3)(oldp3-j:oldp3-j): goto L5890
L5880: if rtrm$(holdst$)="" then goto L5890 else goto L5900
L5890: next j
L5900: holdst$=ltrm$(holdst$)(1:2)
	if holdst$="TE" then holdst$="TX"
return
EXTRACT_ORIGINAL: ! r:
	restore #4,key>=checkkey$: nokey L2170
	L6040: !
	read #4,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat tcp eof L6370
	if heno<>eno then goto L6370
	if prd<orgbeg_date or prd>orgend_date then goto L6040 ! not this year
	form pos 1,n 8,pos 48,n 2,pos 168,21*pd 5.2,pos 468,pd 3
	if tcd<1 or tcd>10 then tcd=1
	! finish with this employee
	dedret=0
	cafded=0
	for j=1 to 20
		if newdedfed(j)=1 then goto L6140 else goto L6150
L6140: dedret=dedret+tcp(j+4)
L6150: if dedfica(j)=1 then goto L6160 else goto L6170
L6160: cafded=cafded+tcp(j+4)
L6170: next j
	orgw2(1)=min(orgw2(1)+tcp(31)-tcp(30)-cafded,ssmax-tcp(30)) ! TOTAL SOC-SEC WAGES
	orgw3=orgw3+tcp(2) ! TOTAL FICA WITHHELD
	orgw3(1)=orgw3(1)+tcp(31)-cafded ! TOTAL MEDICARE WAGES & TIPS
! if env$('client')="Washington Parrish" then orgw3(1)=orgw3(1)+tcp(5) ! add deferred comp match to medicare wages
	orgw3(1)=min(mcmax,orgw3(1)) ! MC WAGES CANNOT EXCEED MAXIMUM
	if uprc$(med$)="Y" then orgw2=orgw2+tcp(2) else orgw2=round(min(orgw3/(ssrate+mcrate)*ssrate,ssmax*ssrate),2) ! SS WH
	if uprc$(med$)="Y" then orgw3(2)=orgw3(2)+tcp(3) else orgw3(2)=orgw3-orgw2 ! MEDICARE WITHHELD
	orgw2(2)=orgw2(2)+tcp(30) ! FICA TIPS YTD
	orgw2(3)=orgw2(3)+tcp(31)-dedret ! TOTAL FEDERAL WAGES
	orgw2(4)=orgw2 ! orgw2(4)+tcp(2) ! FICA W/H YTD       (COULD BE +W2 INSTEAD OF +tcp(2)IF ONLY FICA PORTION GOES INTO W2 RECORD)
! orgw2(4)=orgW2 ! PUT SS W/H ONLY IN 2-W RECORD (EXCLUDE MEDICARE W/H)
	orgw2(5)=orgw2(5)+tcp(1) ! FED W/H YTD
	if ins>0 then orgw2(6)=orgw2(6)+tcp(4+ins) ! EMPLOYER COST GROUP LIFE INS.
	orgw2(7)=orgw2(7)+0 ! UNCOLLECTED EMPLOYEE FICA TAX ON TIPS
	orgw2(8)=orgw2(8)+tcp(24) ! EIC TOTAL
	orgw2(9)=orgw2(9)+0 ! ALLOCATED TIPS
	if orgdfc>0 then orgdc1=orgdc1+tcp(4+orgdfc)*100 ! DEFERRED COMPENSATION
	if orgdcan>0 then orgdca=orgdca+tcp(4+orgdcan)*100 ! DEPENDENT CARE ASSISTANCE
	goto L6040
	L6370: !
	if em6=9 then orgw2(1)=orgw2(4)=orgw3(1)=orgw3(2)=0 ! NO SS OR MC
	if em6=1 then orgw3(1)=orgw3(2)=0 ! NO MEDICARE
	if em6=2 then orgw2(1)=orgw2(4)=0 ! NO SOC-SEC
return ! /r
include: ertn
