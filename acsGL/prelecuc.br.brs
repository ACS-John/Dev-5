! REPLACE S:\acsGL\prElecuc
! never could get it work with an RO record if all zeroes; without 
! RO record i had to put an RE record in front of every RW record and 
! had to follow with an RT record - Right now this program will not 
! create an RO rec

autoLibrary
fnTop(program$,cap$="PR ELEC UC")
on error goto Ertn

dim em$(3)*30,ss$*11,d(14),ty(21),tqm(17),s(9),t(9),z$*8,cap$*128,message$*40
dim a$(3)*40,b$*12,g$*12,d$(10)*8,e$(10)*12,s2(2)
dim k(1),k$(3)*25,l$(1)*11,d(14),m(36),n(2)
dim fa$(1),fb$(1),fc$(1),fd$(1),l$(10),dedfed(10),w3(2),i2(2),t2(2)
dim emppin$*17,tlcn$*6,contact$*27,contactph$*15,phoneext$*5,email$*40
dim w2(9),i1(9),t1(9),ct$*20,st$*2,ibm$*8,namcde$*1,typemp$*1,io1$(15)
dim terminat$*1,first$*15,mid$*15,last$*20,m(10),r(10),e$(10)*12

open #1: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,input
read #1,using FORM_COMPANY: mat a$,b$,mat d$,loccode,mat e$,mat dedfed,oldmax,mat m,mat r,mat e$,mat dedcode
FORM_COMPANY: form pos 1,3*c 40,c 12,pos 150,10*c 8,n 2,pos 317,10*c 12,pos 638,10*n 1,pos 239,pd 4.2,pos 247,10*pd 4.2,10*pd 3.3,10*c 12,pos 618,10*n 1
! read #1,using FORM_PRCOINFO: mat a$,b$,c$,oldmax,mat dedfed
! FORM_PRCOINFO: form pos 1,3*c 40,c 12,x 12,c 5,pos 188,pd 7.2,pos 658,10*n 1
close #1:
L340: !
	p1=pos(b$,"-",1)
	if p1=0 then goto L380
	b$(p1:p1)=""
goto L340
L380: !
	b1=val(b$)
	p1=pos(a$(3),",",1): comma=1
	if p1=0 then p1=pos(a$(3)," ",1): comma=0
	ct$=a$(3)(1:p1-1)
	if comma=1 then st$=a$(3)(p1+2:p1+3) else st$=a$(3)(p1+1:p1+2)
	if uprc$(a$(3)(p1+2:p1+4))="TEX" then st$="Tx"
	p2=len(rtrm$(a$(3)))
	p1=p2-4
	zip$=a$(3)(p1:p2)
	io1$(1)="5,25,C 40,UT,N"
	io1$(2)="6,25,C 40,UT,N"
	io1$(3)="7,25,C 20,UT,N"
	io1$(4)="8,25,c 2,UT,N"
	io1$(5)="9,25,C 5,UT,N"
	io1$(6)="10,25,N 9,UT,N"
	io1$(7)="11,25,N 6,UT,N"
	io1$(8)="12,52,C 1,UT,N"
	io1$(9)="13,38,C 1,UT,N"
	io1$(10)="14,68,n 2,UT,N"
	io1$(11)="15,43,n 2,UT,N"
	io1$(12)="17,25,c 3,UT,N"
	io1$(13)="18,25,c 6,UT,N"
	io1$(14)="19,57,c 1,UT,N"
	io1$(15)="20,49,c 1,UT,N"
	namcde$="F"
	typemp$="R"

SCR1: !
	pr newpage
	close #101: ioerr L680
L680: open #101: "SROW=2,SCOL=3,EROW=23,ECOL=77,BORDER=DR,CAPTION=<Create Electronic U/C Diskette for state.",display,outIn
	pr f "3,15,C 51,R,N": "  Insert diskette for elecronic U/C in drive A:"
	pr f "5,5,C 60": "Company Name:"
	pr f "6,5,C 60": "Street Address:"
	pr f "7,5,C 60": "City:"
	pr f "8,5,C 60": "State:"
	pr f "9,5,C 60": "Zip Code:"
	pr f "10,5,C 60": "Federal ID #:"
	pr f "11,5,C 60": "Quarter Ending Date:"
	pr f "12,5,C 60,N": "F=First Name First or S=Surname First on File:"
	pr f "13,5,C 60": "Type of Business Code R=Regular:"
	pr f "14,5,C 65": "State code used in your record to identify the selected state:"
	pr f "15,5,C 60": "Appropriate FIPS postal numeric code:"
	pr f "16,5,C 70": "(See an appendix in your electronic booklet for the postal code!)"
	pr f "17,5,C 60": "Country Code:"
	pr f "18,5,C 60": "NAICS Code:"
	pr f "19,5,C 60": "Deduct Cafiteria Plans for Calculating Wages (Y/N)?"
	pr f "20,5,C 60": "Deduct Pension for Calculating Wages (Y/N)?"
	pr f "22,28,C 9,B,1": "Next (F1)"
	pr f "22,39,C 11,B,5": "Cancel (F5)"
	if b1>999999999 then b1=0
	pr f mat io1$: a$(1),a$(2),ct$,st$,zip$,b1,yr,namcde$,typemp$,1,48
L900: input fields mat io1$,attr "R": a$(1),a$(2),ct$,st$,zip$,b1,endingdate,namcde$,typemp$,sr1,sr2,country$,naics$,cafiteria$,pension$ conv CONV1
	if ce>0 then io1$(ce)(ce1:ce2)="U": ce=0
	if cmdkey>0 then goto L990 else ce=curfld+1
	if ce>udim(io1$) then ce=1
L940: io1$(ce)=rtrm$(uprc$(io1$(ce))) : ce1=pos(io1$(ce),"U",1)
	ce2=ce1+1 : io1$(ce)(ce1:ce1)="UC" : goto L900
CONV1: if ce>0 then io1$(ce)(ce1:ce2)="U"
	ce=cnt+1
ERR1: pr f "24,78,C 1": bell : goto L940
L990: !
	if cmdkey=5 then goto Xit
	if rtrm$(a$(1))="" then ce=1: goto ERR1
	if rtrm$(a$(2))="" then ce=2: goto ERR1
	if rtrm$(ct$)="" then ce=3: goto ERR1
	if rtrm$(st$)="" then ce=4: goto ERR1
	if rtrm$(zip$)="" then ce=5: goto ERR1
	if b1=0 then ce=6: goto ERR1
	if endingdate<010100 or endingdate>123199 then ce=7: goto ERR1
	if sr1<0 or sr1>10 then ce=10: goto ERR1
	if sr2<0 or sr1>99 then ce=11: goto ERR1
	cafiteria$=uprc$(cafiteria$): pension$=uprc$(pension$)
	if cafiteria$="Y" or cafiteria$="N" then goto L1120 else ce=14: goto ERR1
L1120: if pension$="Y" or pension$="N" then goto L1130 else ce=15: goto ERR1
L1130: monthyr$=cnvrt$("pic(######)",endingdate)(1:2)&"20"&cnvrt$("pic(######)",endingdate)(5:6)
	yr=endingdate-(int(endingdate/100)*100)+2000

	gosub SCR2
	pr newpage
	win=101
	message$=""
	stopable=1: gosub L3390 ! fnWAIT(MESSAGE$,1)

	open #1: "Name=[Q]\GLmstr\PRmstr.h[cno],KFName=[Q]\GLmstr\PRIndex.h[cno],Shr",internal,input,keyed
L1230: open #22: "Name=[Q]\UCReport,RecL=512,eol=crlf,replace",display,output

	goto BEGINNING_OF_FILE
	pr newpage
	msgline$(1)="Insert Diskette"
	mtype=1
	if err=4221 then gosub L3040
	goto L1230

BEGINNING_OF_FILE: gosub RECRA : gosub RECRE
	open #255: "Name=PRN:/SELECT,PAGEOFLOW=58,RecL=220",display,output
	pr #255,using "form SKIP 2,pos 20,cc 40,skip 1,pos 20,cc 40": "Electronic Edit List",cnvrt$("pic(zz/zz/zzzz",endingdate)
! READ_EMPLOYEE: Read #1,Using 1370: ENO,MAT EM$,SS$,EM6,EM16,TA Eof END1
READ_EMPLOYEE: read #1,using L1360: eno,mat em$,ss$,mat m eof END1
L1360: form pos 1,n 4,3*c 25,c 11,36*pd 5.2,2*n 5
	m1=m2=m3=m4=0
	gosub NAME_BREAKDOWN
! Form POS 1,N 8,3*C 30,C 11,POS 122,N 2,POS 156,N 6,POS 173,PD 3
	r1=r1+1
	p1=pos(em$(3),",",1) : comma=1
	if p1=0 then p1=pos(em$(3)," ",1): comma=0
	emct$=em$(3)(1:p1-1)
	gosub STATE_BREAKDOWN: emst$=holdst$ ! If COMMA=1 Then eMST$=EM$(3)(P1+2:P1+3) Else eMST$=EM$(3)(P1+1:P1+2)
	p2=len(rtrm$(em$(3)))
	p1=p2-4
	emzip$=em$(3)(p1:p2)
L1480: p1=pos(ss$,"-",1)
	if p1>0 then ss$(p1:p1)="": goto L1480 else ssn=val(ss$)
! READ_DEPARTMENT: Read #2,Using 1500,Rec=TA: TENO,TCD,MAT TY,MAT TQM,TA
! If SS$="459499366" Then Pause
! Form POS 1,N 8,POS 48,N 2,POS 168,38*PD 5.2,POS 468,PD 3
! If TCD<1 OR TCD>10 Then tCD=1
	gosub CALCULATEUC ! determine wages for quarter
! If TA>0 Then Goto 1480
! Gosub RECRE
	gosub RECRS
	tw1=tw1+1 ! counter
	goto READ_EMPLOYEE

RECRA: pr #22,using L1620: "RA",b1,"","98",a$(1),"",a$(2)(1:22),ct$,st$,zip$,"","","","",country$(1:2),a$(1),"",a$(2)(1:22),ct$,st$,zip$,"","","","",country$(1:2),contact$,contactph$,phoneext$,"",email$,"","",""
L1620: form pos 1,c 2,pic(#########),c 24,c 2,c 57,c 22,c 22,c 22,c 2,c 5,c 4,c 5,c 23,c 15,c 2,c 57,c 22,c 22,c 22,c 2,c 5,c 4,c 5,c 23,c 15,c 2,c 27,c 15,c 5,c 3,c 40,c 3,c 10,c 14
return

RECRE: pr #22,using L1660: "RE",yr,"",b1,"","0","","",a$(1),"",a$(2)(1:22),ct$,st$,zip$,"","",e$(sr1)(1:9),monthyr$,"",r(sr1)*.01,"",naics$,""
L1660: form pos 1,c 2,pic(####),c 1,pic(#########),c 9,c 1,c 4,c 9,c 57,c 22,c 22,c 22,c 2,c 5,c 4,c 126,c 9,c 6,c 1,n 5.4,c 1,c 6,c 185
return

	form pos 1,c 2,pic(#########),c 15,c 15,c 20,c 4,c 22,c 22,c 22,c 2,c 5,c 4,c 5,c 23,c 15,c 2,18*pic(###########),c 22,2*pic(###########),c 56,n 1,c 1,c 1,n 1,c 23,
	pr #22,using L1710: "2E",ct$,st$,"",zip$,namcde$,typemp$,"","","",""
L1710: form pos 1,c 2,g 25,g 10,2*g 5,2*g 1,g 2,g 4,g 2,c 71
return

RECRS: ! STATE RECORD
	if sr1=0 then goto L1830 ! NO STATE SELECTED
	if m1=0 then goto L1830 ! NO quarterly wages
	bd=fndate_mmddyy_to_ccyymmdd(em16): y=int(bd/10000): x=bd-y*10000: z=x*10000+y
	pr #22,using L1790: "RS",sr2,"UTAX",ssn,first$,mid$,last$,"","",monthyr$,m1*100,h2*100,0,z,0,"",e$(sr1)(1:9),"","",country$(1:2),"",naics$,"","","","",""
L1790: form pos 1,c 2,g 2,c 5,pic(#########),c 15,c 15,c 20,c 4,c 124,c 6,2*pic(###########),n 2,n 8,n 8,c 5,c 9,c 81,c 3,c 3,c 1,c 6,c 1,c 10,c 1,c 5,c 145
	pr #255,using L1810: ssn,trim$(first$)&trim$(last$),m1,h2
L1810: form pos 1,n 12,x 2,c 25,2*pic(zz,zzz,zzz.##)
	totwage+=m1: tottaxable+=h2: totemployees+=1
L1830: t1=t1+1: mat t1=t1+w2
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

RECRF: pr #22,using L1980: "RF"," ",tw1,""
L1980: form pos 1,c 2,c 5,pic(#########),c 496
return

END1: !
	pr #255,using "form skip 1,pos 1,c 14,pic(zz,zzz,zzz.##)": "Total wages:",totwage,"Total Taxable:",tottaxable
	pr #255,using "form pos 1,c 16,pic(zz,zzz,zzz)": "Total employees:",totemployees
	gosub L2070
goto Xit

L2070: !
	close #24: ioerr ignore
	dim a$*512
	close #22: ioerr ignore
	open #24: "Name=[Temp]\x,RecL=514,EOL=NONE,REPLACE",external,output
	open #22: "Name=[Q]\UCReport,RecL=512",display,input
	L2120: !
	linput #22: a$ eof L2170
	if a$(512:512)="X" then a$(512:512)=""
	write #24,using L2150: rpad$(a$,512),chr$(13),chr$(10)
	L2150: form pos 1,c 512,c 1,c 1
	goto L2120
	L2170: !
	close #24:
	close #22:
	fnCopy(env$('temp')&"\x","a:UCReport")
return
SCR2: !
	dim contact$*27,email$*40
	win=101
	win_height=12: win_width=75: display_cnam=1: button_option=2: gosub L2680
	pr #win,fields "04,2,Cr 31,N": "Personal ID Number:"
	pr #win,fields "05,2,Cr 31,N": "Resub Indicator:"
	pr #win,fields "06,2,Cr 31,N": "Resub TLCN:"
	pr #win,fields "07,2,Cr 31,N": "Contact Name:"
	pr #win,fields "08,2,Cr 31,N": "Contact Phone Number:"
	pr #win,fields "09,2,Cr 31,N": "Contact Phone Extension:"
	pr #win,fields "10,2,Cr 31,N": "Contact E-Mail:"
	pr #win,fields "11,2,Cr 31,N": "Terminating Business Indicator:"
	pr #win,fields "12,2,Cr 66,N": "Is Medicare W/H a separate field in the employee record (Y/N):"
	scr2_io$(1)="04,34,C 17,UT,N"
	scr2_io$(2)="05,34,C 01,UT,N"
	scr2_io$(3)="06,34,C 06,UT,N"
	scr2_io$(4)="07,34,C 27,UT,N"
	scr2_io$(5)="08,34,C 15,UT,N"
	scr2_io$(6)="09,34,C 05,UT,N"
	scr2_io$(7)="10,34,C 40,UT,N"
	scr2_io$(8)="11,34,C 01,UT,N"
	scr2_io$(9)="12,68,Cu 01,UT,N"
	if resub$="" then resub$="0"
! If TLCN$="" Then tLCN$="0"
	if terminat$="" then terminat$="0"
	med$="Y"
L2310: rinput #win,fields mat scr2_io$: emppin$,resub$,tlcn$,contact$,contactph$,phoneext$,email$,terminat$,med$ conv CONV_SCR2
	if ce>0 then scr2_io$(ce)(ce1:ce2)="U": ce=0
	if cmdkey>0 then goto L2400 else ce=curfld
L2340: ce=ce+1: if ce>udim(scr2_io$) then ce=1
L2350: scr2_io$(ce)=rtrm$(scr2_io$(ce)) : ce1=pos(scr2_io$(ce),"U",9) : if ce1=0 then goto L2340
	ce2=ce1+1 : scr2_io$(ce)(ce1:ce1)="UC" : goto L2310
CONV_SCR2: if ce>0 then scr2_io$(ce)(ce1:ce2)="U"
	ce=cnt+1
ERR_SCR2: pr f "24,78,C 1": bell : goto L2350
L2400: if resub$<>"0" and resub$<>"1" then ce=2 : goto ERR_SCR2
	if resub$="1" and rtrm$(tlcn$)="" then ce=3   : goto ERR_SCR2
	if terminat$<>"0" and terminat$<>"1" then ce=8 : goto ERR_SCR2
	if uprc$(med$)="Y" or uprc$(med$)="N" then goto L2440 else ce=9: goto ERR_SCR2
L2440: close #win:
	if cmdkey=5 then goto SCR1
return

NAME_BREAKDOWN: !
	dim first$*15,mid$*15,last$*20,em$(3)*30
	em$(1)=uprc$(rtrm$(em$(1))): ! nAMCDE$="s"
	x1=pos(em$(1)," ",1)
	x2=pos(em$(1)," ",x1+1)
	x3=pos(em$(1)," ",x2+1)
	if uprc$(namcde$)="S" or uprc$(namcde$)="L" then goto L2590
	first$=em$(1)(1:min(15,max(x1-1,1)))
	if x2>0 then mid$=em$(1)(x1+1:x2-1): last$=em$(1)(x2+1:len(em$(1)))
	if x2=0 then last$=em$(1)(x1+1:len(em$(1))): mid$=""
	goto L2640
L2590: ! last name first
	if x1=0 then x1=pos(em$(1),",",1)
	if x1>0 and em$(1)(x1-1:x1-1)="," then last$=em$(1)(1:x1-2) else last$=em$(1)(1:max(x1-1,1))
	if x2>0 then first$=em$(1)(x1+1:x2-1): mid$=em$(1)(x2+1:len(em$(1)))
	if x2=0 then first$=em$(1)(x1+1:len(em$(1)))(1:15): mid$=""
L2640: x=pos(first$,",",1): if x>0 then first$(x:x)=""
	x=pos(last$,",",1): if x>0 then last$(x:x)=""
! pr FIRST$,MID$,LAST$
return
L2680: !
	if exists("C:\ACS\Local\Settings\No_Print_Newpage.txt") then goto L2760 else pr newpage
L2760: screen_width=80
	screen_height=24
	if display_cnam=0 then goto L2790
L2790: sc=max(int(((screen_width-win_width)/2)+1),2)
	ec=min(sc+win_width-1,79)
	sr=max(int(((screen_height-win_height)/2)+1),2)
	er=min(sr+win_height-1,23)
!     pr "win_height="&STR$(WIN_HEIGHT),"win_width="&STR$(WIN_WIDTH)
!     pr "sr="&STR$(SR),"sc="&STR$(SC)
!     pr "er="&STR$(ER),"ec="&STR$(EC) : Pause
	close #win: ioerr L2870
L2870: open #win: "SRow="&str$(sr)&",SCol="&str$(sc)&",ERow="&str$(er)&",ECol="&str$(ec)&",Border=Sr,Caption=<"&cap$,display,outIn
	pr #win: newpage
	if display_cnam=0 then goto L2920
	if display_cnam=1 then
		pr #win,fields "1,1,Cc "&str$(win_width)&",R,N": env$('cnam')(1:min(40,win_width))
		pr #win,fields "2,1,Cc "&str$(win_width)&",R,N": "Company Number [cno]"(1:min(40,win_width))
	else if display_cnam=2 then
		pr #win,fields "1,1,Cc "&str$(win_width)&",R,N": "Company Number [cno]"(1:min(40,win_width))
	end if
L2920: if button_option=0 then goto L3030
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
	scrline=er+1: gosub L3660 !  fnFKEY(ER+1,MAT FKEY$,MAT DISFK,EM$,ES)

L3030: return  ! Fnend
L3040: ! def library fnOldMsgBox(mat RESPONSE$,&CAP$,mat MSGLINE$,MTYPE)
! mtype=0 means splash    - returns no response                                 ! mostly for "please wait..." and "printing..."                                 ! (anywhere no response is required - no buttons are displyed either)
! mtype=1 means OK only   - returns no response
! mtype=2 means Yes or No - returns "Y" or "N"
! mtype=3 means Yes, No, Cancel - returns "Y" or "N" or ""
! response$(1)= code you're looking for 2-5 are reserved for future use
	close #104: ioerr L3110
L3110: endrow=12
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
L3250: if mtype=2 then input fields str$(endrow)&",09,Cu 1,AE,N": response$(1)
	if mtype=2 and cmdkey=22 then response$(1)="N"
	if mtype=2 and cmdkey=21 then response$(1)="Y"
	if mtype=2 and response$(1)<>"Y" and response$(1)<>"N" then pr f "24,1,C 7,N": bell$ : goto L3250
	if mtype=3 then pr f str$(endrow+1)&",29,Cc 4,B,21": "Yes"
	if mtype=3 then pr f str$(endrow+1)&",34,Cc 4,B,22": "No"
	if mtype=3 then pr f str$(endrow+1)&",39,C 12,B,22": "Cancel (Esc)"
	if mtype=3 then input fields str$(endrow)&",09,Cu 1,AE,N": response$(1)
	if mtype=3 and cmdkey=22 then response$(1)="N"
	if mtype=3 and cmdkey=21 then response$(1)="Y"
	if mtype=3 and cmdkey=99 then response$(1)=""
	if mtype=3 and response$(1)<>"Y" and response$(1)<>"N" and response$(1)<>"" then pr f "24,1,C 7,N": bell$ : goto L3250
	close #104: ioerr L3380
L3380: return  ! Fnend
L3390: ! def library fnWAIT(&MESSAGE$,STOPABLE)
! if stopable=1 will display "Cancel (F5)" button
! win = window number
	close #win: ioerr L3430
L3430: open #win: "Srow=10,SCol=20,ERow=14,ECol=59,Border=Sr,Caption=<"&cap$,display,outIn
	pr #win: newpage
	pr #win,fields "1,1,Cc 40,R,N": env$('cnam')
	pr #win,fields "2,1,Cc 40,R,N": "Company Number [cno]"
	pr #win,fields "4,1,Cc 40,N": message$
	if rtrm$(message$)="" then pr #win,fields "4,1,Cc 40,N": "Please wait..."
	if stopable=0 then pr f "15,34,C 11,R,N": "Do Not Stop"
	if stopable=1 then pr f "15,34,C 11,B,5": "Cancel (F5)"
return  ! Fnend
! def library fnOPENWIN(WIN,SR,SC,ER,EC,&CAP$)
	if sr<1 then sr=10
	if sc<1 then sc=20
	if er<1 then er=14
	if ec<1 then ec=59
	win_width=ec-sc+1
	close #win: ioerr L3590
L3590: open #win: "SRow="&str$(sr)&",SCol="&str$(sc)&",ERow="&str$(er)&",ECol="&str$(ec)&",Border=Sr,Caption=<"&cap$,display,outIn
	pr #win: newpage
	pr #win,fields "1,1,Cc "&str$(win_width)&",R,N": env$('cnam')(1:min(40,win_width))
	pr #win,fields "2,1,Cc "&str$(win_width)&",R,N": "Company Number [cno]"(1:min(40,win_width))


return  ! Fnend
L3660: ! def library fnFKEY(SCRLINE,MAT FKEY$,MAT DISFK,&EM$,ES)
	totallen=0
	startpos=0
	for j=1 to udim(fkey$) ! add ' (Fx)' to each button
		if fkey$(j)="" then goto L3720
		fkey$(j)=fkey$(j)&" (F"&str$(j)&")"		! add ' (Fx)' to each button
		totallen=totallen+len(fkey$(j))+1
L3720: next j
	totallen=totallen+len(rtrm$(em$))+min(len(rtrm$(em$)),1)+es
	totallen=totallen-1
	startpos=int((80-totallen)/2)+1
	pr f str$(scrline)&","&str$(startpos)&",C "&str$(totallen)&",N": rpt$("Ä",totallen)
	for j=1 to udim(fkey$)
		if fkey$(j)="" then goto L3830
		if disfk(j)=1 then pr f str$(scrline)&","&str$(startpos)&",C "&str$(len(fkey$(j)))&",R,"&str$(j): fkey$(j)
		if disfk(j)=1 then goto L3820
		pr f str$(scrline)&","&str$(startpos)&",C "&str$(len(fkey$(j)))&",B,"&str$(j): fkey$(j)
L3820: startpos=startpos+len(fkey$(j))+1
L3830: next j
	if rtrm$(em$)="" then goto L3860
	pr f str$(scrline)&","&str$(startpos)&",C "&str$(len(rtrm$(em$))+es)&",R,N": rtrm$(em$)
L3860: return  ! Fnend
STATE_BREAKDOWN: ! extract state name
	holdst$="          "
	p3=oldp3=0
	p4=10
	for j=1 to 10
		p3=pos(rtrm$(em$(3))," ",p3+1)
		if oldp3>p3 then goto L3960 ! end of address reached
		if p3>0 then oldp3=p3 else goto L3950
L3950: next j
L3960: for j=1 to 10
		if rtrm$(em$(3)(oldp3-j:oldp3-j))="" or em$(3)(oldp3-j:oldp3-j)="," then goto L3980 else p4=p4-1: holdst$(p4:p4)=em$(3)(oldp3-j:oldp3-j): goto L3990
L3980: if rtrm$(holdst$)="" then goto L3990 else goto L4000
L3990: next j
L4000: holdst$=ltrm$(holdst$)(1:2)
	if holdst$="TE" then holdst$="TX"
return

CALCULATEUC: ! determine quarterly wages
	dcy=dcq=0
	for j=1 to 10
		if dedfed(j)=1 then dcy=dcy+m(j*2+9)
		if dedfed(j)=1 then dcq=dcq+m(j*2+10)
	next j
	m2=m2+m(1)-dcy
	m1=m1+m(2)-dcq
	if m2=0 then goto L4280 ! skip if total wage =0
	if m1=0 then goto L4280 ! SKIP IF QUARTERLY WAGE=0
	p3=p3+1
	if m2<m(sr1) then goto L4230
	if m2-m1>m(sr1) then goto L4210
	h2=m(sr1)-(m2-m1)
	goto L4240
L4210: h2=0
	goto L4240
L4230: h2=m1
L4240: h3=m1-h2
	t1=t1+m1
	t2=t2+h3
	t3=t3+h2
L4280: return
Xit: fnXit
include: Ertn