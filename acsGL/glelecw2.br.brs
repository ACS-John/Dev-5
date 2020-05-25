!  Replace S:\acsGL\glElecW2
! Create Electronic W-2s
 
	autoLibrary
	on error goto Ertn
 
	dim em$(3)*30,ss$*11,d(14),m(36),s(9),t(9),z$*8
	dim a$(3)*40,b$*12,g$*12,d$(10)*8,tty(10),e$(10)*12,s2(2)
	dim fa$(1),fb$(1),fc$(1),fd$(1),l$(10),dedfed(10),w3(2),i2(2),t2(2)
	dim cap$*128,message$*40
	dim emppin$*17,tlcn$*6,contact$*27,contactph$*15,phoneext$*5,email$*40
	dim terminat$*1,first$*15,mid$*15,last$*20
	dim w2(9),i1(9),t1(9),ct$*20,st$*2,ibm$*8,namcde$*1,typemp$*1,io1$(18)
 
	fnTop(program$,cap$="Create Electronic W-2s")
	pr newpage
	open #1: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,input  : _
	read #1,using 'Form POS 1,3*C 40,C 12,POS 618,50*N 1': mat a$,b$,mat dedcode,mat dedfed,mat dedfica,mat dedst,mat deduc : _
	close #1:
	on fkey 5 goto Xit
 
L210: p1=pos(b$,"-",1)
	if p1=0 then goto L260
	b$(p1:p1)=""
	goto L210
 
L260: b1=val(b$)
	p1=pos(a$(3),",",1): comma=1
	if p1=0 then p1=pos(a$(3)," ",1): comma=0
	ct$=a$(3)(1:p1-1)
	if comma=1 then st$=a$(3)(p1+2:p1+3) else st$=a$(3)(p1+1:p1+2)
	p2=len(rtrm$(a$(3)))
	p1=p2-4
	zip$=a$(3)(p1:p2)
	if val(date$(1:2))-1 <70 then yr=2000+val(date$(1:2))-1 else yr=1900+val(date$(1:2))-1
	if date$(4:5)="12" then yr=yr+1 ! add one to year if run in december
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
 
SCR1: !
	pr newpage
	close #101: ioerr L610
L610: open #101: "SROW=2,SCOL=3,EROW=23,ECOL=77,BORDER=DR,CAPTION=<Create Electronic W2 Diskette for I.R.S.",display,outIn
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
L840: input fields mat io1$,attr "R": a$(1),a$(2),ct$,st$,zip$,b1,yr,ssmax,ssrate,mcmax,mcrate,ins,pen,dfc,dcan,ibm$,namcde$,typemp$ conv CONV1
	if ce>0 then io1$(ce)(ce1:ce2)="U": ce=0
	if cmdkey>0 then goto L930 else ce=curfld+1
	if ce>udim(io1$) then ce=1
L880: io1$(ce)=rtrm$(uprc$(io1$(ce))) : ce1=pos(io1$(ce),"U",1)
	ce2=ce1+1 : io1$(ce)(ce1:ce1)="UC" : goto L840
CONV1: if ce>0 then io1$(ce)(ce1:ce2)="U"
	ce=cnt+1
ERR1: pr f "24,78,C 1": bell : goto L880
L930: !
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
 
	mat io1$(2)
	io1$(1)="12,71,N 2,UT,N"
	io1$(2)="14,71,N 2,UT,N"
	close #101: ioerr L1120
L1120: pr newpage
	open #101: "SROW=7,SCOL=2,EROW=15,ECOL=79,BORDER=DR,CAPTION=<Electronic W-2   State Reporting Information",display,outIn
	pr f "8,4,C 72": "Some states require filing W2's on diskette.  Answer the following"
	pr f "9,4,C 72": "questions if you wish to create 'RS' records during this run."
	pr f "12,8,Cr 62": "State code used in your record to identify the selected state:"
	pr f "14,8,Cr 62": "Appropriate FIPS postal numeric code:"
	pr f "16,28,C 9,B,1": "Next (F1)"
	pr f "16,39,C 11,B,5": "Cancel (F5)"
L1200: input fields mat io1$: sr1,sr2 conv L1200
	if ce>0 then io1$(ce)(ce1:ce2)="U": ce=0
	if cmdkey>0 then goto L1290 else ce=curfld+1
	if ce>udim(io1$) then ce=1
L1240: io1$(ce)=rtrm$(uprc$(io1$(ce))) : ce1=pos(io1$(ce),"U",1)
	ce2=ce1+1 : io1$(ce)(ce1:ce1)="UC" : goto L1200
CONV2: if ce>0 then io1$(ce)(ce1:ce2)="U"
	ce=cnt+1
ERR2: pr f "24,78,C 1": bell : goto L1240
L1290: if cmdkey=5 then goto Xit
	if sr1<0 or sr1>udim(e$) then ce=1: goto ERR2
	if sr1>0 and sr2=0 then ce=2: goto ERR2
 
	gosub SCR2
	pr newpage
	win=101
	message$=""
	stopable=1: gosub L3970 ! fnWAIT(MESSAGE$,1)
 
	open #1: "Name=[Q]\GLmstr\PRmstr.h[cno],KFName=[Q]\GLmstr\PRINDEX.h[cno],Shr",internal,input,keyed
L1400: open #22: "Name=W2REPORT,RecL=512,eol=crlf,replace",display,output
	goto L1480
 
	pr newpage
	msgline$(1)="Insert Diskette"
	mtype=1
	if err=4221 then gosub L3610 ! fnOldMsgBox(MAT RESPONSE$,CAP$,MAT MSGLINE$,MTYPE)
	goto L1400
L1480: gosub RECRA
! Gosub RECRE
L1500: ! pr f "12,32,N 3,UT,N": R1/LREC(1)*100
	pr f "12,32,N 3,N": r1/lrec(1)*100
	read #1,using L1610: eno,mat em$,ss$,mat m eof END1
	gosub L3030
	dedfed=dedfica=dedst=0
	for j=1 to 10
		if dedcode(j)><1 then goto L1600
		if dedfed(j)=1 then dedfed=dedfed+m(j*2+9)
		if dedfica(j)=1 then dedfica=dedfica+m(j*2+9)
		if dedst(j)=1 then dedst=dedst+m(j*2+9)
L1600: next j
L1610: form pos 1,n 4,3*c 25,c 11,36*pd 5.2
	p1=pos(em$(3),",",1) : comma=1
	if p1=0 then p1=pos(em$(3)," ",1): comma=0
	emct$=em$(3)(1:p1-1)
	if comma=1 then emst$=em$(3)(p1+2:p1+3) else emst$=em$(3)(p1+1:p1+2)
	emst$=em$(3)(p1+2:p1+3)
	p2=len(rtrm$(em$(3)))
	p1=p2-4
	emzip$=em$(3)(p1:p2)
L1700: p1=pos(ss$,"-",1)
	if p1>0 then ss$(p1:p1)="": goto L1700 else ssn=val(ss$)
	w2(1)=min(w2(1)+m(1)-m(31)-dedfica,ssmax-m(31)) ! TOTAL SOC-SEC WAGES
	w3=w3+m(5) ! TOTAL FICA WITHHELD
	w3(1)=w3(1)+m(1)-dedfica ! TOTAL MEDICARE WAGES & TIPS
	w3(1)=min(mcmax,w3(1)) ! MC wages cannot exceen maximum
	w2=round(min(w3/(ssrate+mcrate)*ssrate,ssmax*ssrate),2) ! SS WH
	w3(2)=w3-w2 ! Medicare withheld
	w2(2)=w2(2)+m(31) ! FICA tips YTD
	w2(3)=w2(3)+m(1)-dedfed ! TOTAL FEDERAL WAGES
	w2(4)=w2(4)+w2 ! FICA W/H YTD
! w2(4)=W2 ! SS WH only in W-2 record ( EXCLUDE MEDICARE W/H)
	w2(5)=w2(5)+m(3) ! FED W/H YTD
	if ins>0 then w2(6)=w2(6)+m(9+(ins*2)) ! EMPLOYER COST GROUP LIFE INS
	w2(7)=w2(7)+0 ! uncollected employee fica tax on tips
	w2(8)=w2(8)+m(35) ! EIC TOTAL
	w2(9)=w2(9)+0 ! ALLOCATED TIPS
	if dfc>0 then dc1=dc1+m(9+(dfc*2))*100 ! DEFERRED COMPENSATION
	if dcan>0 then dca=dca+m(9+(dcan*2))*100 ! Dependent care assistance
	if sr1=0 then goto L1920
	s2(1)=s2(1)+(m(1)*100)
	s2(2)=s2(2)+(m(7)*100)
L1920: if em6=9 then w2(1)=w2(4)=w3(1)=w3(2)=0
	gosub RECRE
	gosub RECRW
	gosub RECRS
	tw1=tw1+1
	tw2=tw2+1
	gosub RECRT
	tw2=0
	goto L1500
 
RECRA: pr #22,using L2030: "RA",rpad$(ltrm$(str$(b1)),9),emppin$,resub$,tlcn$,"98",a$(1),"",a$(2)(1:22),ct$,st$,zip$,"","","","","",a$(1),"",a$(2)(1:22),ct$,st$,zip$,"","","","","",contact$,contactph$,phoneext$,"",email$,"","","2","L",""
L2030: form pos 1,c 2,pic(#########),c 17,c 1,c 6,c 2,c 57,c 22,c 22,c 22,c 2,c 5,c 4,c 5,c 23,c 15,c 2,c 57,c 22,c 22,c 22,c 2,c 5,c 4,c 5,c 23,c 15,c 2,c 27,c 15,c 5,c 3,c 40,c 3,c 10,c 1,c 1,c 12
return
 
RECRE: pr #22,using L2070: "RE",yr,"",rpad$(ltrm$(str$(b1)),9),"",terminat$,"","",a$(1),"",a$(2)(1:22),ct$,st$,zip$,"","","","","","R","",0,""
L2070: form pos 1,c 2,pic(####),c 1,pic(#########),c 9,c 1,c 4,c 9,c 57,c 22,c 22,c 22,c 2,c 5,c 4,c 5,c 23,c 15,c 2,c 1,c 1,n 1,c 291
return
 
	form pos 1,c 2,pic(#########),c 15,c 15,c 20,c 4,c 22,c 22,c 22,c 2,c 5,c 4,c 5,c 23,c 15,c 2,18*pic(###########),c 22,2*pic(###########),c 56,n 1,c 1,c 1,n 1,c 23,
	pr #22,using L2120: "2E",ct$,st$,"",zip$,namcde$,typemp$,"","","",""
L2120: form pos 1,c 2,g 25,g 10,2*g 5,2*g 1,g 2,g 4,g 2,c 71
return
 
RECRW: for j=1 to 9: w2(j)=w2(j)*100: next j
	for j=1 to 2: w3(j)=w3(j)*100 : next j
	if pen=0 then pen$="0" else pen$="1"
	if dfc=0 then dfc$="" else dfc$="D"
	pr #22,using L2200: "RW",ssn,first$,mid$,last$,"","",em$(2)(1:22),emct$,emst$,emzip$,"","","","","",w2(3),w2(5),w2(1),w2(4),w3(1),w3(2),w2(2),w2(8),dca,dc1,0,0,0,0,0,0,0,0,0,"",w2(6),0,0,0,0,"",0,"",pen$,0,""
L2200: form pos 1,c 2,pic(#########),c 15,c 15,c 20,c 4,c 22,c 22,c 22,c 2,c 5,c 4,c 5,c 23,c 15,c 2,19*pic(###########),c 11,5*pic(###########),c 23,pic(#),c 1,c 1,pic(#),c 23
! pr #22,Using 2270: "RO","",W2(9),W2(7),0,0,0,0,0,"","","",0,0,0,0,0,0,0,"",0,0,""
	form pos 1,c 2,c 9,7*pic(###########),c 176,c 1,c 9,7*pic(###########),c 11,2*pic(###########),c 128
return
 
RECRS: ! STATE RECORD
	if sr1=0 then goto L2300 ! NO STATE SELECTED
	if s2(1)=0 and s2(2)=0 then goto L2300 ! NO STATE WAGES
	pr #22,using L2290: "RS",sr2,"",ssn,first$,mid$,last$,"","",em$(2)(1:22),emct$,emst$,emzip$,"","","","","","","",0,0,0,0,0,"","","",sr2,s2(1),s2(2),"","",0,0,"","","",""
L2290: form pos 1,c 2,g 2,c 5,pic(#########),c 15,c 15,c 20,c 4,c 22,c 22,c 22,c 2,c 5,c 4,c 5,c 23,c 15,c 2,c 2,c 6,2*pic(###########),pic(##),2*pic(########),c 5,c 20,c 6,g 2,2*pic(###########),c 10,c 1,2*pic(###########),c 7,c 75,c 75,c 25
L2300: t1=t1+1: mat t1=t1+w2
	mat i1=i1+w2
	mat i2=i2+w3
	mat t2=t2+w3
	dc2=dc2+dc1
	dc3=dc3+dc1
	dca2=dca2+dca
	dca3=dca3+dca
	w2=w3=dca=dc1=0
	mat w2=(0)
	mat w3=(0)
	mat s2=(0)
return
 
RECRT: !
	pr #22,using L2460: "RT",tw2,t1(3),t1(5),t1(1),t1(4),t2(1),t2(2),t1(2),t1(8),dca3,dc3,0,0,0,0,0,0,0,0,0,"",t1(6),0,0,0,0,0,""
L2460: form pos 1,c 2,pic(#######),19*pic(###############),c 15,6*pic(###############),c 113
! pr #22,Using 2520: "RU",TW2,T1(9),T1(7),0,0,0,0,0,"",0,0,0,0,0,0,0,0,0,""
	form pos 1,c 2,pic(#######),7*pic(###############),c 240,9*pic(###############),c 23
	t1=0: mat t1=(0)
	mat t2=(0)
return
 
RECRF: pr #22,using L2540: "RF"," ",tw1,""
L2540: form pos 1,c 2,c 5,pic(#########),c 496
return
 
END1: !
! Gosub RECRT
	gosub RECRF
	gosub L2630
Xit: fnXit
 
L2630: close #24: ioerr L2650
	dim a$*512
L2650: close #22: ioerr L2660
L2660: open #24: "Name=X,RecL=513,EOL=NONE,REPLACE",external,output
	open #22: "Name=w2report,RecL=512",display,input
L2680: linput #22: a$ eof L2730
	if a$(512:512)="X" then a$(512:512)=""
	write #24,using L2710: rpad$(a$,512),chr$(10)
L2710: form pos 1,c 512,c 1
	goto L2680
L2730: close #24:
	close #22:
	execute "COPY x f:w2report"
return
 
SCR2: !
	dim contact$*27,email$*40
	win=101
	win_height=12: win_width=75: display_cnam=1: button_option=2: gosub L3200
	pr #win,fields "04,2,Cr 31,N": "Personal ID Number:" : _
	pr #win,fields "05,2,Cr 31,N": "Resub Indicator:" : _
	pr #win,fields "06,2,Cr 31,N": "Resub TLCN:" : _
	pr #win,fields "07,2,Cr 31,N": "Contact Name:" : _
	pr #win,fields "08,2,Cr 31,N": "Contact Phone Number:" : _
	pr #win,fields "09,2,Cr 31,N": "Contact Phone Extension:" : _
	pr #win,fields "10,2,Cr 31,N": "Contact E-Mail:" : _
	pr #win,fields "11,2,Cr 31,N": "Terminating Business Indicator:"
	scr2_io$(1)="04,34,C 17,UT,N" : _
	scr2_io$(2)="05,34,C 01,UT,N" : _
	scr2_io$(3)="06,34,C 06,UT,N" : _
	scr2_io$(4)="07,34,C 27,UT,N" : _
	scr2_io$(5)="08,34,C 15,UT,N" : _
	scr2_io$(6)="09,34,C 05,UT,N" : _
	scr2_io$(7)="10,34,C 40,UT,N" : _
	scr2_io$(8)="11,34,C 01,UT,N"
	if resub$="" then resub$="0"
! If TLCN$="" Then tLCN$="0"
	if terminat$="" then terminat$="0"
L2870: rinput #win,fields mat scr2_io$: emppin$,resub$,tlcn$,contact$,contactph$,phoneext$,email$,terminat$ conv CONV_SCR2
	if ce>0 then scr2_io$(ce)(ce1:ce2)="U": ce=0
	if cmdkey>0 then goto L2960 else ce=curfld
L2900: ce=ce+1: if ce>udim(scr2_io$) then ce=1
L2910: scr2_io$(ce)=rtrm$(scr2_io$(ce)) : ce1=pos(scr2_io$(ce),"U",1) : if ce1=0 then goto L2900
	ce2=ce1+1 : scr2_io$(ce)(ce1:ce1)="UC" : goto L2870
CONV_SCR2: if ce>0 then scr2_io$(ce)(ce1:ce2)="U"
	ce=cnt+1
ERR_SCR2: pr f "24,78,C 1": bell : goto L2910
L2960: if resub$<>"0" and resub$<>"1" then ce=2 : _
		goto ERR_SCR2
	if resub$="1" and rtrm$(tlcn$)="" then ce=3 : _
		goto ERR_SCR2
	if terminat$<>"0" and terminat$<>"1" then ce=8 : _
		goto ERR_SCR2
	close #win:
	if cmdkey=5 then goto SCR1
return
 
L3030: dim first$*15,mid$*15,last$*20,em$(3)*30
	em$(1)=uprc$(rtrm$(em$(1))): ! nAMCDE$="s"
	x1=pos(em$(1)," ",1)
	x2=pos(em$(1)," ",x1+1)
	x3=pos(em$(1)," ",x2+1)
	if uprc$(namcde$)="S" then goto L3130
	first$=em$(1)(1:max(x1-1,1))
	if x2>0 then mid$=em$(1)(x1+1:x2-1): last$=em$(1)(x2+1:len(em$(1)))
	if x2=0 then last$=em$(1)(x1+1:len(em$(1))): mid$=""
	goto L3170
L3130: ! last name first
	if x1>0 and em$(1)(x1-1:x1-1)="," then last$=em$(1)(1:x1-2) else last$=em$(1)(1:max(x1-1,1))
	if x2>0 then first$=em$(1)(x1+1:x2-1): mid$=em$(1)(x2+1:len(em$(1)))
	if x2=0 then first$=em$(1)(x1+1:len(em$(1))): mid$=""
L3170: ! pr FIRST$,MID$,LAST$
return
 
L3200: !
	if exists("C:\ACS\Local\Settings\No_Print_Newpage.txt") then goto L3280 else pr newpage
L3280: screen_width=80
	screen_height=24
	if display_cnam=0 then goto L3350
L3350: sc=max(int(((screen_width-win_width)/2)+1),2)
	ec=min(sc+win_width-1,79)
	sr=max(int(((screen_height-win_height)/2)+1),2)
	er=min(sr+win_height-1,23)
!     pr "win_height="&STR$(WIN_HEIGHT),"win_width="&STR$(WIN_WIDTH)
!     pr "sr="&STR$(SR),"sc="&STR$(SC)
!     pr "er="&STR$(ER),"ec="&STR$(EC) : Pause
	close #win: ioerr L3430
L3430: open #win: "SRow="&str$(sr)&",SCol="&str$(sc)&",ERow="&str$(er)&",ECol="&str$(ec)&",Border=Sr,Caption=<"&cap$,display,outIn
	pr #win: newpage
	if display_cnam=0 then goto L3480
	if display_cnam=1 then : _
		pr #win,fields "1,1,Cc "&str$(win_width)&",R,N": env$('cnam')(1:min(40,win_width)) : _
		pr #win,fields "2,1,Cc "&str$(win_width)&",R,N": "Company Number [cno]"(1:min(40,win_width))
	if display_cnam=2 then : _
		pr #win,fields "1,1,Cc "&str$(win_width)&",R,N": "Company Number [cno]"(1:min(40,win_width))
L3480: if button_option=0 then goto L3590
	mat fkey$=("") : em$="" : es=0
	fkey$(5)="Cancel" ! included by default
	if button_option=2 then : _
		fkey$(1)="Next"
	if button_option=3 then : _
		fkey$(1)="Print"
	if button_option=4 then : _
		fkey$(1)="Save"
	if button_option=5 then : _
		fkey$(1)="Next" : _
		fkey$(6)="Search"
	if button_option=6 then : _
		fkey$(1)="Next" : _
		fkey$(2)="Back"
	if button_option=7 then : _
		fkey$(1)="Save" : _
		fkey$(4)="Delete"
	scrline=er+1: gosub L4320 !  fnFKEY(ER+1,MAT FKEY$,MAT DISFK,EM$,ES)
 
L3590: return  ! Fnend
 
L3610: ! def library fnOldMsgBox(mat RESPONSE$,&CAP$,mat MSGLINE$,MTYPE)
! mtype=0 means splash    - returns no response                                 ! mostly for "please wait..." and "printing..."                                 ! (anywhere no response is required - no buttons are displyed either)
! mtype=1 means OK only   - returns no response
! mtype=2 means Yes or No - returns "Y" or "N"
! mtype=3 means Yes, No, Cancel - returns "Y" or "N" or ""
! response$(1)= code you're looking for 2-5 are reserved for future use
	close #104: ioerr L3680
L3680: endrow=12
	for j=2 to udim(msgline$)
		if msgline$(j)<>"" then endrow=endrow+1
	next j
	open #104: "SRow=10,SCol=09,ERow="&str$(endrow)&",ECol=70,Border=SR,Caption=<"&cap$,display,outIn
	pr #104: newpage
	mglinerow=2
	for j=1 to udim(msgline$)
		pr #104,fields str$(mglinerow+j-1)&",2,Cc 60,N": msgline$(j)
	next j
	if mtype=1 then pr f str$(endrow+1)&",38,Cc 4,B,1": "Ok"
	if mtype=1 then input fields str$(endrow)&",09,C 1,AE,N": pause$
	if mtype=2 then pr f str$(endrow+1)&",35,Cc 4,B,21": "Yes"
	if mtype=2 then pr f str$(endrow+1)&",40,Cc 4,B,22": "No"
L3820: if mtype=2 then input fields str$(endrow)&",09,Cu 1,AE,N": response$(1)
	if mtype=2 and cmdkey=22 then response$(1)="N"
	if mtype=2 and cmdkey=21 then response$(1)="Y"
	if mtype=2 and response$(1)<>"Y" and response$(1)<>"N" then pr f "24,1,C 7,N": bell$ : goto L3820
	if mtype=3 then pr f str$(endrow+1)&",29,Cc 4,B,21": "Yes"
	if mtype=3 then pr f str$(endrow+1)&",34,Cc 4,B,22": "No"
	if mtype=3 then pr f str$(endrow+1)&",39,C 12,B,22": "Cancel (Esc)"
	if mtype=3 then input fields str$(endrow)&",09,Cu 1,AE,N": response$(1)
	if mtype=3 and cmdkey=22 then response$(1)="N"
	if mtype=3 and cmdkey=21 then response$(1)="Y"
	if mtype=3 and cmdkey=99 then response$(1)=""
	if mtype=3 and response$(1)<>"Y" and response$(1)<>"N" and response$(1)<>"" then pr f "24,1,C 7,N": bell$ : goto L3820
	close #104: ioerr L3950
L3950: return  ! Fnend
 
L3970: ! def library fnWAIT(&MESSAGE$,STOPABLE)
! if stopable=1 will display "Cancel (F5)" button
! win = window number
	close #win: ioerr ignore
	open #win: "Srow=10,SCol=20,ERow=14,ECol=59,Border=Sr,Caption=<"&cap$,display,outIn
	pr #win: newpage
	pr #win,fields "1,1,Cc 40,R,N": env$('cnam')
	pr #win,fields "2,1,Cc 40,R,N": "Company Number [cno]"
	pr #win,fields "4,1,Cc 40,N": message$
	if rtrm$(message$)="" then pr #win,fields "4,1,Cc 40,N": "Please wait..."
	if stopable=0 then pr f "15,34,C 11,R,N": "Do Not Stop"
	if stopable=1 then pr f "15,34,C 11,B,5": "Cancel (F5)"
return  ! Fnend
! def library fnOPENWIN(WIN,SR,SC,ER,EC,&CAP$)
L4190: if sr<1 then sr=10
	if sc<1 then sc=20
	if er<1 then er=14
	if ec<1 then ec=59
	win_width=ec-sc+1
	close #win: ioerr L4250
L4250: open #win: "SRow="&str$(sr)&",SCol="&str$(sc)&",ERow="&str$(er)&",ECol="&str$(ec)&",Border=Sr,Caption=<"&cap$,display,outIn
	pr #win: newpage
	pr #win,fields "1,1,Cc "&str$(win_width)&",R,N": env$('cnam')(1:min(40,win_width))
	pr #win,fields "2,1,Cc "&str$(win_width)&",R,N": "Company Number [cno]"(1:min(40,win_width))
 
 
return  ! Fnend
L4320: ! def library fnFKEY(SCRLINE,MAT FKEY$,MAT DISFK,&EM$,ES)
	totallen=0 : _
	startpos=0
	for j=1 to udim(fkey$) ! add ' (Fx)' to each button
		if fkey$(j)="" then goto L4380
		fkey$(j)=fkey$(j)&" (F"&str$(j)&")" : _
		! add ' (Fx)' to each button
		totallen=totallen+len(fkey$(j))+1
L4380: next j
	totallen=totallen+len(rtrm$(em$))+min(len(rtrm$(em$)),1)+es
	totallen=totallen-1
	startpos=int((80-totallen)/2)+1
	pr f str$(scrline)&","&str$(startpos)&",C "&str$(totallen)&",N": rpt$("Ä",totallen)
	for j=1 to udim(fkey$)
		if fkey$(j)="" then goto L4490
		if disfk(j)=1 then pr f str$(scrline)&","&str$(startpos)&",C "&str$(len(fkey$(j)))&",R,"&str$(j): fkey$(j)
		if disfk(j)=1 then goto L4480
		pr f str$(scrline)&","&str$(startpos)&",C "&str$(len(fkey$(j)))&",B,"&str$(j): fkey$(j)
L4480: startpos=startpos+len(fkey$(j))+1
L4490: next j
	if rtrm$(em$)="" then goto L4520
	pr f str$(scrline)&","&str$(startpos)&",C "&str$(len(rtrm$(em$))+es)&",R,N": rtrm$(em$)
L4520: return  ! Fnend
 
include: Ertn
 
