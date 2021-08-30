!  Replace S:\acsGL\Elec1099
! Create Electronic 1099s

autoLibrary
fnTop(program$)
on error goto Ertn

dim vn$*8,nam$*30,ad1$*30,ad2$*30,csz$*30,ss$*11
dim a$(3)*40,b$*12,knp(51),wrd2$(13),io2$(13)
dim io1$(51),pnc$*4,cfs$*1,ti$*1,ai$*9,spn$*40,dsc$*2,ln4$*4,tin$*1
dim amt(12),c$*29,s$*2,z$*9,ps$*2,camt(12),kamt(51,12),stu(51)
dim ti2(12),cn$*40,orc$*1,de$*30,resp$(60)*40
dim tr$(13)*12

! r: get mat st$
	dim st$(51)*22
	st$(1 )='AL-01-Alabama'
	st$(2 )='AK-  -Alaska'
	st$(3 )='AZ-04-Arizona'
	st$(4 )='AR-05-Arkansas'
	st$(5 )='CA-06-California'
	st$(6 )='CO-  -Colorado'
	st$(7 )='CT-  -Connecticut'
	st$(8 )='DE-10-Delaware'
	st$(9 )='FL-  -Florida'
	st$(10)='GA-13-Georgia'
	st$(11)='HI-15-Hawaii'
	st$(12)='ID-16-Idaho'
	st$(13)='IL-  -Illinois'
	st$(14)='IN-18-Indiana'
	st$(15)='IA-19-Iowa'
	st$(16)='KS-20-Kansas'
	st$(17)='KY-  -Kentucky'
	st$(18)='LA-  -Louisiana'
	st$(19)='ME-23-Maine'
	st$(20)='MD-  -Maryland'
	st$(21)='MA-25-Massachusetts'
	st$(22)='MI-  -Michigan'
	st$(23)='MN-27-Minnesota'
	st$(24)='MS-28-Mississippi'
	st$(25)='MO-29-Missouri'
	st$(26)='MT-30-Montana'
	st$(27)='NE-  -Nebraska'
	st$(28)='NV-  -Nevada'
	st$(29)='NH-  -New Hampshire'
	st$(30)='NJ-34-New Jersey'
	st$(31)='NM-35-New Mexico'
	st$(32)='NY-36-New York'
	st$(33)='NC-37-North Carolina'
	st$(34)='ND-38-North Dakota'
	st$(35)='OH-  -Ohio'
	st$(36)='OK-  -Oklahoma'
	st$(37)='OR-41-Oregon'
	st$(38)='PA-  -Pennsylvania'
	st$(39)='RI-  -Rhode Island'
	st$(40)='SC-45-South Carolina'
	st$(41)='SD-  -South Dakota'
	st$(42)='TN-47-Tennessee'
	st$(43)='TX-  -Texas'
	st$(44)='UT-  -Utah'
	st$(45)='VT-  -Vermont'
	st$(46)='VA-  -Virginia'
	st$(47)='WA-  -Washington'
	st$(48)='DC-11-Washington DC'
	st$(49)='WV-  -West Virginia'
	st$(50)='WI-55-Wisconsin'
	st$(51)='WY-  -Wyoming'
! /r
! r: get mat amt$(13,9)
	dim amt$(13,9)*70
	! FORM 1098
	amt$(1,1)='1-0600-Mortgage interest received from payer/borrower'
	amt$(1,2)='2'
	amt$(1,3)='3'
	amt$(1,4)='4'
	amt$(1,5)='5'
	amt$(1,6)='6'
	amt$(1,7)='7'
	amt$(1,8)='8'
	amt$(1,9)='9'
	! FORM 1099-A
	amt$(2,1)='1'
	amt$(2,2)='2-0010-Amount of debt outstanding'
	amt$(2,3)='3-0010-Amount of debt satisfied'
	amt$(2,4)='4-0010-Fair market value of property at acquisition or abandonment'
	amt$(2,5)='5'
	amt$(2,6)='6'
	amt$(2,7)='7'
	amt$(2,8)='8'
	amt$(2,9)='9'
	! FORM 1099-B
	amt$(3,1)='1'
	amt$(3,2)='2-0001-Stocks bonds etc'
	amt$(3,3)='3-0001-Bartering'
	amt$(3,4)='4-0001-Federal income tax withheld'
	amt$(3,5)='5'
	amt$(3,6)='6-0001-Profit (or loss) realized current year'
	amt$(3,7)='7-0001-Unrealized profit (or loss) on open contracts 12/31/87'
	amt$(3,8)='8-0001-Unrealized profit (or loss) on open contracts 12/31/88'
	amt$(3,9)='9-0001-Aggregate profit (or loss)'
	! FORM 1099-DIV
	amt$(4,1)='1-0010-Ordinary dividends'
	amt$(4,2)='2-0010-Total capital gains distributions'
	amt$(4,3)='3-0010-28% rate gain'
	amt$(4,4)='4-0010-Qualified 5-year gain'
	amt$(4,5)='5-0010-Unrecaptured section 1250 gain'
	amt$(4,6)='6-0010-Section 1202 gain'
	amt$(4,7)='7-0010-Nontaxable distributions'
	amt$(4,8)='8-0010-Federal income tax withheld'
	amt$(4,9)='9-0010-Investment expenses'
	! FORM 1099-G
	amt$(5,1)='1-0010-Unemployment compensation'
	amt$(5,2)='2-0010-State or local income tax refunds'
	amt$(5,3)='3'
	amt$(5,4)='4-0010-Federal income tax withheld'
	amt$(5,5)='5-0600-Discharge of indebtedness'
	amt$(5,6)='6-0600-Taxable grants'
	amt$(5,7)='7-0600-Agriculture payments'
	amt$(5,8)='8'
	amt$(5,9)='9'
	! FORM 1099-INT
	amt$(6,1)='1-0010-Earnings from savings'
	amt$(6,2)='2-0010-Early withdrawal penalty'
	amt$(6,3)='3-0010-U S Savings bonds etc'
	amt$(6,4)='4-0010-Federal income tax withheld'
	amt$(6,5)='5-0010-Foreigh tax paid'
	amt$(6,6)='6'
	amt$(6,7)='7'
	amt$(6,8)='8'
	amt$(6,9)='9'
	! FORM 1099-MISC
	amt$(7,1)='1-0600-Rents'
	amt$(7,2)='2-0600-Royalties'
	amt$(7,3)='3-0600-Prizes and awards'
	amt$(7,4)='4-0600-Federal income tax withheld'
	amt$(7,5)='5-0600-Fishing boat proceeds'
	amt$(7,6)='6-0600-Medical and health care payments'
	amt$(7,7)='7-0600-Nonemployee compensation'
	amt$(7,8)='8-0600-Substitute payments in lieu of dividends or interest'
	amt$(7,9)='9-5000-Direct sales indicator'
	! FORM 1099-OID
	amt$(8,1)='1-0010-Total original issue discount for year'
	amt$(8,2)='2-0010-Other periodic interest'
	amt$(8,3)='3-0010-Early withdrawal penalty'
	amt$(8,4)='4-0010-Federal income tax withheld'
	amt$(8,5)='5'
	amt$(8,6)='6'
	amt$(8,7)='7'
	amt$(8,8)='8'
	amt$(8,9)='9'
	! FORM 1099-PATR
	amt$(9,1)='1-0010-Patronage dividends'
	amt$(9,2)='2-0010-Nonpatronage distributions'
	amt$(9,3)='3-0010-Per-unit retain allocations'
	amt$(9,4)='4-0010-Federal income tax withheld'
	amt$(9,5)='5-0010-Redemption of nonqualified notices and allocations'
	amt$(9,6)='6-0010-Investment credit'
	amt$(9,7)='7-0010-Energy investment credit'
	amt$(9,8)='8-0010-Jobs credit'
	amt$(9,9)='9-0010-Low-income housing credit'
	! FORM 1099-R
	amt$(10,1)='1-0001-Gross distribution'
	amt$(10,2)='2-0001-Taxable amount'
	amt$(10,3)='3-0001-Amount eligible for capital gain election'
	amt$(10,4)='4-0001-Federal income tax withheld'
	amt$(10,5)='5-0001-Employee contributions or insurance premiums'
	amt$(10,6)='6-0001-Net unrealized appreciation in employers securities'
	amt$(10,7)='7'
	amt$(10,8)='8-0001-Other'
	amt$(10,9)='9-0001-State income tax withheld'
	! FORM 1099-S
	amt$(11,1)='1'
	amt$(11,2)='2-0001-Gross proceeds'
	amt$(11,3)='3'
	amt$(11,4)='4'
	amt$(11,5)='5'
	amt$(11,6)='6'
	amt$(11,7)='7'
	amt$(11,8)='8'
	amt$(11,9)='9'
	! FORM 5498
	amt$(12,1)='1-0001-Regular IRA contributions'
	amt$(12,2)='2-0001-Rollover IRA contributions'
	amt$(12,3)='3-0001-Life insurance cost included'
	amt$(12,4)='4-0001-Fair market value of the IRA or SEP at year end'
	amt$(12,5)='5'
	amt$(12,6)='6'
	amt$(12,7)='7'
	amt$(12,8)='8'
	amt$(12,9)='9'
	! FORM W-2G
	amt$(13,1)='1-0600-Gross winnings'
	amt$(13,2)='2-0001-Federal income tax withheld'
	amt$(13,3)='3-0001-State income tax withheld'
	amt$(13,4)='4'
	amt$(13,5)='5'
	amt$(13,6)='6'
	amt$(13,7)='7-0600-Winnings from identical wagers'
	amt$(13,8)='8'
	amt$(13,9)='9'
! /r

	tr$(1)="3 1098"
	tr$(2)="4 1099-A"
	tr$(3)="B 1099-B"
	tr$(4)="1 1099-DIV"
	tr$(5)="F 1099-G"
	tr$(6)="6 1099-INT"
	tr$(7)="A 1099-MISC"
	tr$(8)="D 1099-OID"
	tr$(9)="7 1099-PATR"
	tr$(10)="9 1099-R"
	tr$(11)="L 5498"
	tr$(12)="W W-2G"

	open #20: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,input
	read #20,using 'Form POS 1,3*C 40,C 12': mat a$,b$
	close #20:

	ficamax=oldmax
L2210: p1=pos(b$,"-",1)
	if p1=0 then goto L2250
	b$(p1:p1)=""
	goto L2210
L2250: b1=0 : b1=val(b$) conv L2260
L2260: p1=pos(a$(3),",",1)
	if p1=0 then p1=pos(a$(3)," ",1)
	ct$=a$(3)(1:p1-1)
	st$=a$(3)(p1+2:p1+3)
	p2=len(rtrm$(a$(3)))
	p1=p2-4
	zip$=a$(3)(p1:p2)
	if val(date$(1:2))-1 <70 then
		yr=2000+val(date$(1:2))-1
	else
		yr=1900+val(date$(1:2))-1
	end if
L2340: io1$(01)="04,36,C 40,UT,N"
	io1$(02)="05,36,C 40,UT,N"
	io1$(03)="06,36,C 40,UT,N"
	io1$(04)="08,40,N 09,UT,N"
	io1$(05)="09,40,N 04,UT,N"
	io1$(06)="10,40,C 04,UT,N"
	io1$(07)="11,40,Cu 1,UT,N"
	io1$(08)="12,40,C 05,UT,N"
	io1$(09)="13,40,Cu 1,UT,N"
	io1$(10)="15,36,C 40,UT,N"
	io1$(11)="16,36,Nz 10,UT,N"
	io1$(12)="18,58,Cu 1,UT,N"
	io1$(13)="19,58,Nz 10,UT,N"
	ibm$="IBM"
	namcde$="F"
	typemp$="R"
	pr newpage
MAIN: !
	fnTos
	mylen=30: mypos=mylen+3 : right=1
	fnLbl(1,15,"Insert Diskette for Electronic 1099s in Drive A",55,0)
	fnLbl(3,1,"Company Namme:",mylen,right)
	fnTxt(3,mypos,40,0,0,"",0,"The filer's company name.",0 )
	resp$(1)=a$(1)
	fnLbl(4,1,"Address:",mylen,right)
	fnTxt(4,mypos,40,0,0,"",0,"The filer's address.",0 )
	resp$(2)=a$(2)
	fnLbl(5,1,"City State Zip:",mylen,right)
	fnTxt(5,mypos,40,0,0,"",0,"The filer's city state zip.",0 )
	resp$(3)=a$(3)
	mylen=40: mypos=mylen+3
	fnLbl(7,1,"Federal ID Number:",mylen,right)
	fnTxt(7,mypos,12,0,0,"",0,"Enter the Federal ID number without slashes,dashes, or spaces.",0 )
	resp$(4)=str$(b1)
	fnLbl(8,1,"Payment Year:",mylen,right)
	fnTxt(8,mypos,4,0,0,"1030",0,"The payment year must be entered and will be in ccyy format.",0 )
	resp$(5)=str$(yr)
	fnLbl(9,1,"4 Character Payer Name Control Code:",mylen,right)
	fnTxt(9,mypos,4,0,0,"",0,"The Payer Name Control Code can be obtained from the mail label on the 1099 Package that you received from IRS.",0 )
	resp$(6)=pnc$
	fnChk(11,mypos,"Combined Federal/State Filer:",1)
	resp$(7)=cfsy$
	fnLbl(12,1,"5 Character Transmitter Code:",mylen,right)
	fnTxt(12,mypos,5,0,0,"",0,"When you apply with the IRS to submit by magnetic media, you will be issued a five character transmitter code.",0 )
	resp$(8)=pnc$
	fnChk(13,mypos,"Is Payer a Foreign Corporation:",1)
	resp$(9)=tcc$
	fnLbl(14,1,"Contact Name:",mylen,right)
	fnTxt(14,mypos,40,0,0,"",0,"",0 )
	resp$(10)=cn$
	fnLbl(15,1,"Contact Phone Number:",mylen,right)
	fnTxt(15,mypos,10,0,0,"30",0,"",0 )
	resp$(11)=str$(cpn)
	mylen=60: mypos=mylen+3
	fnLbl(17,1,"(O)riginal, (R)eplacdment or (C)orrection file (O/R/C):",mylen,right)
	fnTxt(17,mypos,1,0,0,"",0,"",0 )
	resp$(12)=orc$
	fnLbl(18,1,"Payer Phone Number:",mylen,right)
	fnTxt(18,mypos,10,0,0,"30",0,"",0 )
	resp$(13)=str$(ppn)
	fnCmdKey("&Next",1,1,0,"Moves to next questions.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu.")
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
! Input #101,Fields MAT IO1$: MAT A$,B1,YR,PNC$,CFSYN$,TCC$,FCIYN$,CN$,CPN,ORC$,PPN Conv CONV1
	if cfsyn$="Y" then cfs$="1" else cfs$=" "
	if fciyn$="Y" then fci$="1" else fci$=" "
	if yr<1000 then goto MAIN
	if b1=0 then goto MAIN
	if rtrm$(pnc$)="" then goto MAIN
	if rtrm$(tcc$)="" then goto MAIN
	if fci$=" " or fci$="1" then goto L2930 else goto MAIN
L2930: if rtrm$(cn$)="" then goto MAIN
	if rtrm$(uprc$(orc$))="O" or rtrm$(uprc$(orc$))="R" or rtrm$(uprc$(orc$))="C" then goto L2950 else goto MAIN
L2950: if uprc$(orc$)="O" then orc2$="1  "
	if uprc$(orc$)="R" then orc2$=" 1 "
	if uprc$(orc$)="C" then orc2$="  1"
	csz$=a$(3)(1:20): gosub CSZ
	if cfs$=" " then goto L3180
	if cfs$><"1" then goto MAIN
SELECT_ST: !
	resp=0
	fnTos
	mylen=28 : mypos=mylen+3
	fnLbl(1,1,"Place a 1 by each State participating in the combined Federal/State Filer",80,0)
	for j=1 to 17
		for x=1 to 3
! fnChk(J+1,X*20,ST$(STOP2),1)
			! rESP$(RESP+=1)=ST$(J*3-2)
		next x
	next j
	fnCmdKey("&Next",1,1,0,"Moves to next questions.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu.")
	ckey=fnAcs(mat resp$)
	for j=1 to 51
		if resp$(j)="True" then stu(j)=1
	next j

L3180: ! pr NEWPAGE ! commenting this line might screw up window 101 repeat processing
	close #102: ioerr ignore
	open #102: "SROW=6,SCOL=20,ERow=20,ECOL=58,Border=SR,Caption=<"&env$('program_caption'),display,outIn
	pr #102: newpage
	pr #102,fields "2,2,C 17,N": "Select Form Type:"
	for j=1 to 13
		wrd2$(j)=cnvrt$("N 2",j)&".  "&tr$(j)(3:18)
		io2$(j)=str$(j+1)&",20,C 18"
	next j
	pr f "21,35,C 09,B,5": "Exit (F5)"
L3280: rinput #102,select mat io2$,attr "H": mat wrd2$
	ti1=curfld
	if ti1<1 or ti1>udim(tr$) then goto L3280
	ti$=tr$(ti1)(1:1)
	close #101: ioerr L3330
L3330: open #101: "SROW=3,SCOL=3,ERow=22,ECOL=78,Border=DR,Caption=<"&env$('program_caption'),display,outIn
	pr #101: newpage
	pr f "4,05,C 04,R,N": "Type"
	pr f "4,11,C 40,R,N": "Category"
	for j=1 to 14
		if j<10 then pr f str$(j+4)&",11,C 65": amt$(ti1,j)(8:70)
		io1$(j)=str$(j+4)&",7,N 2,UT,N"
	next j
	pr f "17,5,C 65,R,N": "Vendor Type in your file that matches each category:"
	pr f "23,30,C 09,B,1": "Next (F1)"
	pr f "23,41,C 09,B,5": "Exit (F5)"
L3440: input fields mat io1$: mat ti2 conv L3440
	if ce>0 then io1$(ce)(ce1:ce2)="U": ce=0
	if cmdkey>0 then goto L3530 else ce=curfld+1
	if ce>udim(io1$) then ce=1
L3480: io1$(ce)=rtrm$(uprc$(io1$(ce))) : ce1=pos(io1$(ce),"U",1)
	ce2=ce1+1 : io1$(ce)(ce1:ce1)="UC" : goto L3440
CONV2: if ce>0 then io1$(ce)(ce1:ce2)="U"
	ce=cnt+1
ERR2: pr f "24,78,C 1": bell : goto L3480
L3530: if cmdkey=5 then goto Xit
	if sum(ti2)<1 then goto L3440
	ai$=""
	for j=1 to 12
		if ti2(j)=0 then goto L3590
		ai$(j:j)=str$(j)
L3590: next j
	gosub PROCESS
ASKDAT: !
	fnTos
	mylen=28 : mypos=mylen+3
	fnLbl(1,1,"Transaction Starting Date:",mylen,1)
	fnTxt(1,mypos,8,0,0,'CCYYMMDD',0,'Normally you would enter the first day of the calendar year.')
	resp$(1)=str$(transactionstartingdate)
	fnLbl(2,1,"Transaction Ending Date:",mylen,1)
	fnTxt(2,mypos,8,0,0,'CCYYMMDD',0,'You should enter the last day of the calendar year.')
	resp$(2)=str$(transactionendingdate)
	fnLbl(2,1,"",45,1)
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)

	transactionstartingdate=val(resp$(1))
	transactionendingdate=val(resp$(2))
	open #paymstr=1: "Name=[Q]\GLmstr\PayMstr.h[cno],Version=1,KFName=[Q]\GLmstr\PayIdx1.h[cno],Shr",internal,outIn,keyed
	open #trans=2: "Name=[Q]\GLmstr\GLTR1099.h[cno],KFName=[Q]\GLmstr\gltrIdx1.h[cno],Shr",i,i,k
	if ct1=0 then open #22: "Name=IRSTAX,RecL=750,eol=crlf,Replace",d,o
	if ct1=0 then gosub RECT
	gosub RECA
	if lrec(1)=0 then goto L3800
L3790: pr f "12,32,N 3,UT,N": r1/lrec(1)*100
L3800: read #1,using 'Form Pos 1,C 8,4*c 30,x 5,n 2,c 11,x 6,c 12',release: vn$,nam$,ad1$,ad2$,csz$,typ,ss$,ph$ eof END1
	restore #2,key>=vn$: nokey L3800
	ytdp=0 ! do not use ytdp from payee record
L3830: read #trans,using 'Form POS 1,c 8,N 6,PD 5.2,C 12,C 30,PD 3',release: trvn$,dt,am,rn$,de$,nta eof L3900
	if trim$(trvn$)<>trim$(vn$) then goto L3900
	x=fndate_mmddyy_to_ccyymmdd(dt)
	if x<transactionstartingdate then goto L3830
	if x>transactionendingdate then goto L3830
	ytdp+=am
	goto L3830
L3900: ! vN$="12345678": nAM$="JOE JONES": aD1$="11014 HWY 206": aD2$="PO BOX 925": cSZ$="HARRISON, AR 72601": yTDP=655.55: tYP=7: sS$="123456789"
	form pos 1,c 8,c 35,3*c 20,pd 5.2,n 2,c 11
	for j=1 to 12
		if typ=0 then goto L3980
		if typ><ti2(j) then goto L3970
		if val(amt$(ti1,j)(3:6))>ytdp then goto L3790
		amt(j)=ytdp*100 : goto L3990
		L3970: !
	next j
L3980: !
goto L3790
L3990: !
	r1=r1+1
	p1=pos(csz$,",",1)
	if p1=0 then p1=pos(csz$," ",1)
	c$=csz$(1:p1-1)
	s$=uprc$(csz$(p1+2:p1+3))
	p2=len(rtrm$(csz$))
	p1=p2-4
	z$=csz$(p1:p2)
	ssn=0: tin$=" "
	for j=1 to 11
		if ss$(j:j)=>"0" and ss$(j:j)<="9" then goto L4130
		if j=3 then tin$="1"
		if j=4 or j=7 then tin$="2"
		ss$(j:j)=""
L4130: next j
	if len(ss$)><9 then goto L4180
	ssn=val(ss$)
	if tin$><" " then goto L4460

L4180: !
	pr newpage
	close #103: ioerr ignore
	open #103: "SROW=7,SCOL=8,EROW=15,ECOL=72,Border=Sr,Caption=<"&env$('program_caption'),display,outIn
	io3$(1)="7,55,Cu 1,UT,N"
	io3$(2)="8,55,C 9,UT,N"
	pr #103,fields "4,2,C 60,N": "Vendor Number: "&ltrm$(vn$)
	pr #103,fields "5,2,C 60,N": "         Name: "&nam$
	pr #103,fields "2,2,Cc 63,H": "Unable to determine Federal ID or Social Security Number."
	pr #103,fields "7,2,Cr 52,N": "[F]ederal ID, [S]ocial Security Number or [N]either:"
	pr #103,fields "8,2,Cr 52,N": "Federal ID or Social Security Number:"
	ss$=ss$(1:9)
	pr #103,fields "1,1,C 7,N": hex$("07")
L4300: !
	pr f "16,35,C 09,B,5": "Stop (F5)"
	if tin$="1" then tinfs$="F"
	if tin$="2" then tinfs$="S"
	if tin$=" " then tinfs$="N"
L4340: !
	rinput #103,fields mat io3$: tinfs$,ss$ conv L4300
	if tinfs$="F" then tin$="1"
	if tinfs$="S" then tin$="2"
	if tinfs$="N" then tin$=" "
	if tinfs$<>"F" and tinfs$<>"S" and tinfs$<>"N" then goto L4340
	ssn=val(ss$) conv L4340
	if tin$><" " and len(rtrm$(ss$))><9 then goto L4340
	if tin$=" " and len(rtrm$(ss$))><0 then goto L4340
	if tin$><" " and ssn=0 then goto L4300
	if tin$=" " or tin$="1" or tin$="2" then goto L4450 else goto L4300
	close #103:
L4450: !
gosub PROCESS
L4460: !
if tin$="1" then ln4$=uprc$(nam$(1:4)) : goto L4530
	p1=len(rtrm$(nam$))
	if p1=0 then ln4$="": goto L4530
	for j=p1 to 1 step -1
		if nam$(j:j)=" " then goto L4520
	next j
L4520: !
	ln4$=nam$(j+1:j+4)
L4530: !
	for j=1 to 51
		if s$><st$(j)(1:2) then goto L4610
		if stu(j)=1 then ps$=st$(j)(4:5) else ps$="  "
		for j1=1 to 12
			kamt(j,j1)=kamt(j,j1)+amt(j1)
		next j1
		knp(j)=knp(j)+1
		goto L4750
		L4610: !
	next j
	pr newpage
	close #103: ioerr ignore
	open #103: "SROW=7,SCOL=14,EROW=15,ECOL=65,BORDER=SR,CAPTION=<"&env$('program_caption'),display,outIn
	pr #103: newpage
	pr #103,fields "2,2,Cc 50,H,N": "invalid State Code encountered"
	pr #103,fields "1,1,C 7,N": hex$("07")
	pr #103,fields "04,2,C 50,N": "Vendor Number: "&ltrm$(vn$)
	pr #103,fields "05,2,C 50,N": "  Vendor Name: "&nam$
	pr #103,fields "06,2,C 45,N": "  City ST Zip: "&csz$
	pr #103,fields "08,2,C 19,N": "Correct State Code:"
	rinput #103,fields "08,22,Cu 2,UT,N": s$
	gosub PROCESS
goto L4530
L4750: if orc$="C" then cri$="G" else cri$=""
	gosub RECB
goto L3790

RECT: ! r:
	seq=seq+1: pr #22,using L4810: "T",yr,"",b1,tcc$," "," "," ",fic$,a$(1)," ",a$(1)," ",a$(2),city$,st$,zip$," ",1,cn$,cpn," "," "," "," "," ",seq," ","V","Advanced Computer Services, Inc.","P O Box 758","Harrison","AR","72601","Ken Johnson","8707415447","acs1@alltel.net"," "," "
L4810: form pos 1,c 1,n 4,c 1,n 9,c 5,c 2,c 5,c 1,c 1,6*c 40,c 2,c 9,c 15,pic(########),c 40,n 15,c 35,c 2,c 15,c 6,c 83,pic(########),c 10,c 1,c 40,c 40,c 40,c 2,c 9,c 40,c 15,c 35,c 9,c 2
	return ! /r
RECA: ! r:
	seq=seq+1: pr #22,using L4860: "A",yr," ",b1,pnc$," ",cfs$,ti$,ai$," ",orc2$," ",fci$,a$(1)," ",tai,a$(2),city$,st$,zip$,ppn," ",seq," "," "
L4860: form pos 1,c 1,n 4,c 6,g 9,c 4,3*c 1,c 12,c 8,c 3,c 1,c 1,2*c 40,n 1,2*c 40,c 2,c 9,n 15,pos 240,c 260,pos 500,pic(########),c 231,c 2
	return ! /r
RECB: ! r:
	totalb=totalb+1
	seq=seq+1: pr #22,using L4920: "B",yr,cri$," ",tin$,ss$,vn$," "," ",mat amt,"",fci$,nam$,"","",ad1$,"",c$,s$,z$,"",seq,"","","",0,0,"",""
	L4920: form pos 1,c 1,n 4,c 1,c 4,c 1,c 9,c 20,c 4,c 10,12*pic(############),c 48,c 1,6*c 40,c 2,c 9,c 1,pic(########),pos 508,c 36,pos 544,c 119,c 60,2*pic(##########),c 2,c 2
	mat camt=camt+amt
	cnp=cnp+1
	tnp=tnp+1
	mat amt=(0)
	return ! /r
RECC: ! r:
	seq=seq+1: pr #22,using L5010: "C",cnp,"",mat camt,"",seq," "," "
L5010: form pos 1,c 1,pic(########),c 6,12*pic(##################),c 268,pic(########),c 231,c 2
	mat camt=(0)
	cnp=0
return ! /r
RECK: ! r:
	for j=1 to 51
		if knp(j)=0 or stu(j)=0 then goto L5110
		seq=seq+1: pr #22,using L5100: "K",knp(j),"",kamt(j,1),kamt(j,2),kamt(j,3),kamt(j,4),kamt(j,5),kamt(j,6),kamt(j,7),kamt(j,8),kamt(j,9),kamt(j,10),kamt(j,11),kamt(j,12),"",seq," ","","","",st$(j)(4:5),""
L5100: form pos 1,c 1,pic(########),c 6,12*pic(##################),c 268,pic(########),c 18,c 18,c 4,c 2,c 2
L5110: next j
	mat kamt=(0)
	mat knp=(0)
return ! /r
RECF: ! r:
	seq=seq+1: pr #22,using L5180: "F",tnp,"",totalb,"",seq," "
L5180: form pos 1,c 1,pic(########),"000000000000000000000",c 19,pic(########),c 442,pic(########),c 241,c 2
return ! /r

END1: !
	gosub RECC
	gosub RECK
	close #1:
	pr newpage
	close #104: ioerr ignore
	open #104: "SROW=7,SCOL=8,EROW=09,ECOL=72,Border=SR,Caption=<"&env$('program_caption'),display,outIn
	pr #104,fields "2,2,C 58,N": "Do you have another type of return for this company (Y/N):"
	yn$="N" ! default
L5290: rinput #104,fields "2,61,Cu 1,UT,N": yn$ conv L5290
	if yn$="Y" then ct1=1
	if yn$="N" then ct1=0
	if yn$<>"N" and yn$<>"Y" then goto L5290
	if ct1=1 then goto L2340
	gosub RECF
	close #22: ioerr L5370
	gosub L5600
L5370: goto Xit

Xit: fnXit

PROCESS: ! r:
	pr newpage
	close #101: ioerr ignore
	open #101: "SROW=10,SCOL=20,EROW=12,ECOL=59,BORDER=DR,CAPTION=<"&env$('program_caption'),display,outIn
	pr f "10,35,C 10,N": "processing"
	pr f "13,34,C 11,B,5": "Cancel (F5)"
	pr f "12,32,C 20": "  0% COMPLETED"
return ! /r
 
CSZ: ! r: EXTRACT  CITY$,STATE$,ZIP$ FORM CSZ$
	L5500: p1=pos(csz$,".",1)
	if p1>0 then csz$(p1:p1)="": goto L5500
	p1=pos(csz$,",",1)-1
	if p1=-1 then p1=pos(csz$," ",1)-1
	p2=pos(csz$," ",p1+3)
	city$=uprc$(rtrm$(csz$(1:p1))(1:15))
	state$=uprc$(rtrm$(csz$(p2-2:p2))(1:2))
	zip$=uprc$(ltrm$(rtrm$(csz$(p2+1:25)))(1:9))
return ! /r
 
L5600: ! r:
	close #24: ioerr ignore
	dim a$*750
	close #25: ioerr ignore
	open #24: "Name=X,RecL=751,EOL=NONE,Replace",external,output
	open #25: "Name=irstax,RecL=750",display,input
	do
		linput #25: a$ eof L5700
		if a$(750:750)="X" then a$(750:750)=""
		write #24,using L5680: rpad$(a$,750),chr$(10)
		L5680: form pos 1,c 750,c 1
	loop
	L5700: !
	close #24:
	close #25:
	fnCopy('x','a:irstax')
return ! /r
include: ertn
