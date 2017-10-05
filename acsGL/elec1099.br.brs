00010 !  Replace S:\acsGL\Elec1099
00020 ! Create Electronic 1099s !:
        ! modified for new 750 recl for 1998 (this change not made until 2/26/99          so everybodys will be wrong! must do conversion pgm to change to new            format when disketts returned
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnerror,fndate_mmddyy_to_ccyymmdd,fntos,fnlbl,fntxt,fncmdset,fnacs,fncmdkey,fnchk
00050   fntop(program$,cap$="Create Electronic 1099s")
00060   on error goto ERTN
00070 ! ______________________________________________________________________
00080   dim vn$*8,nam$*30,ad1$*30,ad2$*30,csz$*30,ss$*11,cap$*128
00090   dim a$(3)*40,b$*12,knp(51),st$(51)*22,wrd2$(13),io2$(13)
00100   dim io1$(51),pnc$*4,cfs$*1,ti$*1,ai$*9,spn$*40,dsc$*2,ln4$*4,tin$*1
00110   dim amt(12),c$*29,s$*2,z$*9,ps$*2,camt(12),kamt(51,12),stu(51)
00120   dim ti2(12),cn$*40,orc$*1,de$*30,resp$(60)*40
00130   dim amt$(13,9)*70,tr$(13)*12
00140 ! ______________________________________________________________________
00170 ! 
00180   on fkey 5 goto L5370
00190 ! ______________________________________________________________________
00200   data AL-01-Alabama
00210   data AK-  -Alaska
00220   data AZ-04-Arizona
00230   data AR-05-Arkansas
00240   data CA-06-California
00250   data CO-  -Colorado
00260   data CT-  -Connecticut
00270   data DE-10-Delaware
00280   data FL-  -Florida
00290   data GA-13-Georgia
00300   data HI-15-Hawaii
00310   data ID-16-Idaho
00320   data IL-  -Illinois
00330   data IN-18-Indiana
00340   data IA-19-Iowa
00350   data KS-20-Kansas
00360   data KY-  -Kentucky
00370   data LA-  -Louisiana
00380   data ME-23-Maine
00390   data MD-  -Maryland
00400   data MA-25-Massachusetts
00410   data MI-  -Michigan
00420   data MN-27-Minnesota
00430   data MS-28-Mississippi
00440   data MO-29-Missouri
00450   data MT-30-Montana
00460   data NE-  -Nebraska
00470   data NV-  -Nevada
00480   data NH-  -New Hampshire
00490   data NJ-34-New Jersey
00500   data NM-35-New Mexico
00510   data NY-36-New York
00520   data NC-37-North Carolina
00530   data ND-38-North Dakota
00540   data OH-  -Ohio
00550   data OK-  -Oklahoma
00560   data OR-41-Oregon
00570   data PA-  -Pennsylvania
00580   data RI-  -Rhode Island
00590   data SC-45-South Carolina
00600   data SD-  -South Dakota
00610   data TN-47-Tennessee
00620   data TX-  -Texas
00630   data UT-  -Utah
00640   data VT-  -Vermont
00650   data VA-  -Virginia
00660   data WA-  -Washington
00670   data DC-11-Washington DC
00680   data WV-  -West Virginia
00690   data WI-55-Wisconsin
00700   data WY-  -Wyoming
00710   read mat st$
00720 ! ________________________________________________-
00730 ! FORM 1098
00740   data 1-0600-Mortgage interest received from payer/borrower
00750   data 2
00760   data 3
00770   data 4
00780   data 5
00790   data 6
00800   data 7
00810   data 8
00820   data 9
00830 ! FORM 1099-A
00840   data 1
00850   data 2-0010-Amount of debt outstanding
00860   data 3-0010-Amount of debt satisfied
00870   data 4-0010-Fair market value of property at acquisition or abandonment
00880   data 5
00890   data 6
00900   data 7
00910   data 8
00920   data 9
00930 ! FORM 1099-B
00940   data 1
00950   data 2-0001-Stocks bonds etc
00960   data 3-0001-Bartering
00970   data 4-0001-Federal income tax withheld
00980   data 5
00990   data 6-0001-Profit (or loss) realized current year
01000   data 7-0001-Unrealized profit (or loss) on open contracts 12/31/87
01010   data 8-0001-Unrealized profit (or loss) on open contracts 12/31/88
01020   data 9-0001-Aggregate profit (or loss)
01030 ! FORM 1099-DIV
01040   data 1-0010-Ordinary dividends
01050   data 2-0010-Total capital gains distributions
01060   data 3-0010-28% rate gain
01070   data 4-0010-Qualified 5-year gain
01080   data 5-0010-Unrecaptured section 1250 gain
01090   data 6-0010-Section 1202 gain
01100   data 7-0010-Nontaxable distributions
01110   data 8-0010-Federal income tax withheld
01120   data 9-0010-Investment expenses
01130 ! FORM 1099-G
01140   data 1-0010-Unemployment compensation
01150   data 2-0010-State or local income tax refunds
01160   data 3
01170   data 4-0010-Federal income tax withheld
01180   data 5-0600-Discharge of indebtedness
01190   data 6-0600-Taxable grants
01200   data 7-0600-Agriculture payments
01210   data 8
01220   data 9
01230 ! FORM 1099-INT
01240   data 1-0010-Earnings from savings
01250   data 2-0010-Early withdrawal penalty
01260   data 3-0010-U S Savings bonds etc
01270   data 4-0010-Federal income tax withheld
01280   data 5-0010-Foreigh tax paid
01290   data 6
01300   data 7
01310   data 8
01320   data 9
01330 ! FORM 1099-MISC
01340   data 1-0600-Rents
01350   data 2-0600-Royalties
01360   data 3-0600-Prizes and awards
01370   data 4-0600-Federal income tax withheld
01380   data 5-0600-Fishing boat proceeds
01390   data 6-0600-Medical and health care payments
01400   data 7-0600-Nonemployee compensation
01410   data 8-0600-Substitute payments in lieu of dividends or interest
01420   data 9-5000-Direct sales indicator
01430 ! FORM 1099-OID
01440   data 1-0010-Total original issue discount for year
01450   data 2-0010-Other periodic interest
01460   data 3-0010-Early withdrawal penalty
01470   data 4-0010-Federal income tax withheld
01480   data 5
01490   data 6
01500   data 7
01510   data 8
01520   data 9
01530 ! FORM 1099-PATR
01540   data 1-0010-Patronage dividends
01550   data 2-0010-Nonpatronage distributions
01560   data 3-0010-Per-unit retain allocations
01570   data 4-0010-Federal income tax withheld
01580   data 5-0010-Redemption of nonqualified notices and allocations
01590   data 6-0010-Investment credit
01600   data 7-0010-Energy investment credit
01610   data 8-0010-Jobs credit
01620   data 9-0010-Low-income housing credit
01630 ! FORM 1099-R
01640   data 1-0001-Gross distribution
01650   data 2-0001-Taxable amount
01660   data 3-0001-Amount eligible for capital gain election
01670   data 4-0001-Federal income tax withheld
01680   data 5-0001-Employee contributions or insurance premiums
01690   data 6-0001-Net unrealized appreciation in employers securities
01700   data 7
01710   data 8-0001-Other
01720   data 9-0001-State income tax withheld
01730 ! FORM 1099-S
01740   data 1
01750   data 2-0001-Gross proceeds
01760   data 3
01770   data 4
01780   data 5
01790   data 6
01800   data 7
01810   data 8
01820   data 9
01830 ! FORM 5498
01840   data 1-0001-Regular IRA contributions
01850   data 2-0001-Rollover IRA contributions
01860   data 3-0001-Life insurance cost included
01870   data 4-0001-Fair market value of the IRA or SEP at year end
01880   data 5
01890   data 6
01900   data 7
01910   data 8
01920   data 9
01930 ! FORM W-2G
01940   data 1-0600-Gross winnings
01950   data 2-0001-Federal income tax withheld
01960   data 3-0001-State income tax withheld
01970   data 4
01980   data 5
01990   data 6
02000   data 7-0600-Winnings from identical wagers
02010   data 8
02020   data 9
02030   read mat amt$
02040 ! ______________________________________________________________________
02050   tr$(1)="3 1098"
02060   tr$(2)="4 1099-A"
02070   tr$(3)="B 1099-B"
02080   tr$(4)="1 1099-DIV"
02090   tr$(5)="F 1099-G"
02100   tr$(6)="6 1099-INT"
02110   tr$(7)="A 1099-MISC"
02120   tr$(8)="D 1099-OID"
02130   tr$(9)="7 1099-PATR"
02140   tr$(10)="9 1099-R"
02150   tr$(11)="L 5498"
02160   tr$(12)="W W-2G"
02170 ! ______________________________________________________________________
02180   open #20: "Name="&env$('Q')&"\GLmstr\Company.h"&env$('cno')&",Shr",internal,input  !:
        read #20,using 'Form POS 1,3*C 40,C 12': mat a$,b$ !:
        close #20: 
02190 ! ______________________________________________________________________
02200   ficamax=oldmax
02210 L2210: p1=pos(b$,"-",1)
02220   if p1=0 then goto L2250
02230   b$(p1:p1)=""
02240   goto L2210
02250 L2250: b1=0 : b1=val(b$) conv L2260
02260 L2260: p1=pos(a$(3),",",1)
02270   if p1=0 then p1=pos(a$(3)," ",1)
02280   ct$=a$(3)(1:p1-1)
02290   st$=a$(3)(p1+2:p1+3)
02300   p2=len(rtrm$(a$(3)))
02310   p1=p2-4
02320   zip$=a$(3)(p1:p2)
02330   if val(date$(1:2))-1 <70 then !:
          yr=2000+val(date$(1:2))-1 else !:
          yr=1900+val(date$(1:2))-1
02340 L2340: io1$(01)="04,36,C 40,UT,N"
02350   io1$(02)="05,36,C 40,UT,N"
02360   io1$(03)="06,36,C 40,UT,N"
02370   io1$(04)="08,40,N 09,UT,N"
02380   io1$(05)="09,40,N 04,UT,N"
02390   io1$(06)="10,40,C 04,UT,N"
02400   io1$(07)="11,40,Cu 1,UT,N"
02410   io1$(08)="12,40,C 05,UT,N"
02420   io1$(09)="13,40,Cu 1,UT,N"
02430   io1$(10)="15,36,C 40,UT,N"
02440   io1$(11)="16,36,Nz 10,UT,N"
02450   io1$(12)="18,58,Cu 1,UT,N"
02460   io1$(13)="19,58,Nz 10,UT,N"
02470   ibm$="IBM"
02480   namcde$="F"
02490   typemp$="R"
02500   pr newpage
02510 MAIN: ! 
02520   fntos(sn$="Elec1099") !:
        mylen=30: mypos=mylen+3 : right=1
02530   fnlbl(1,15,"Insert Diskette for Electronic 1099s in Drive A",55,0)
02540   fnlbl(3,1,"Company Namme:",mylen,right)
02550   fntxt(3,mypos,40,0,0,"",0,"The filer's company name.",0 ) !:
        resp$(1)=a$(1)
02560   fnlbl(4,1,"Address:",mylen,right)
02570   fntxt(4,mypos,40,0,0,"",0,"The filer's address.",0 ) !:
        resp$(2)=a$(2)
02580   fnlbl(5,1,"City State Zip:",mylen,right)
02590   fntxt(5,mypos,40,0,0,"",0,"The filer's city state zip.",0 ) !:
        resp$(3)=a$(3)
02600   mylen=40: mypos=mylen+3
02610   fnlbl(7,1,"Federal ID Number:",mylen,right)
02620   fntxt(7,mypos,12,0,0,"",0,"Enter the Federal ID number without slashes,dashes, or spaces.",0 ) !:
        resp$(4)=str$(b1)
02630   fnlbl(8,1,"Payment Year:",mylen,right)
02640   fntxt(8,mypos,4,0,0,"1030",0,"The payment year must be entered and will be in ccyy format.",0 ) !:
        resp$(5)=str$(yr)
02650   fnlbl(9,1,"4 Character Payer Name Control Code:",mylen,right)
02660   fntxt(9,mypos,4,0,0,"",0,"The Payer Name Control Code can be obtained from the mail label on the 1099 Package that you received from IRS.",0 ) !:
        resp$(6)=pnc$
02670   fnchk(11,mypos,"Combined Federal/State Filer:",1) !:
        resp$(7)=cfsy$
02680   fnlbl(12,1,"5 Character Transmitter Code:",mylen,right)
02690   fntxt(12,mypos,5,0,0,"",0,"When you apply with the IRS to submit by magnetic media, you will be issued a five character transmitter code.",0 ) !:
        resp$(8)=pnc$
02700   fnchk(13,mypos,"Is Payer a Foreign Corporation:",1) !:
        resp$(9)=tcc$
02710   fnlbl(14,1,"Contact Name:",mylen,right)
02720   fntxt(14,mypos,40,0,0,"",0,"",0 ) !:
        resp$(10)=cn$
02730   fnlbl(15,1,"Contact Phone Number:",mylen,right)
02740   fntxt(15,mypos,10,0,0,"30",0,"",0 ) !:
        resp$(11)=str$(cpn)
02750   mylen=60: mypos=mylen+3
02760   fnlbl(17,1,"(O)riginal, (R)eplacdment or (C)orrection file (O/R/C):",mylen,right)
02770   fntxt(17,mypos,1,0,0,"",0,"",0 ) !:
        resp$(12)=orc$
02780   fnlbl(18,1,"Payer Phone Number:",mylen,right)
02790   fntxt(18,mypos,10,0,0,"30",0,"",0 ) !:
        resp$(13)=str$(ppn)
02800   fncmdkey("&Next",1,1,0,"Moves to next questions.")
02810   fncmdkey("&Cancel",5,0,1,"Returns to menu.")
02820   fnacs(sn$,0,mat resp$,ckey)
02830   if ckey=5 then goto XIT
02850 ! Input #101,Fields MAT IO1$: MAT A$,B1,YR,PNC$,CFSYN$,TCC$,FCIYN$,CN$,CPN,ORC$,PPN Conv CONV1
02860   if cfsyn$="Y" then cfs$="1" else cfs$=" "
02870   if fciyn$="Y" then fci$="1" else fci$=" "
02880   if yr<1000 then goto MAIN
02890   if b1=0 then goto MAIN
02900   if rtrm$(pnc$)="" then goto MAIN
02910   if rtrm$(tcc$)="" then goto MAIN
02920   if fci$=" " or fci$="1" then goto L2930 else goto MAIN
02930 L2930: if rtrm$(cn$)="" then goto MAIN
02940   if rtrm$(uprc$(orc$))="O" or rtrm$(uprc$(orc$))="R" or rtrm$(uprc$(orc$))="C" then goto L2950 else goto MAIN
02950 L2950: if uprc$(orc$)="O" then orc2$="1  "
02960   if uprc$(orc$)="R" then orc2$=" 1 "
02970   if uprc$(orc$)="C" then orc2$="  1"
02980   csz$=a$(3)(1:20): gosub CSZ
02990   if cfs$=" " then goto L3180
03000   if cfs$><"1" then goto MAIN
03010 SELECT_ST: ! 
03015   resp=0
03020   fntos(sn$="elec10992") !:
        mylen=28 : mypos=mylen+3
03035   fnlbl(1,1,"Place a 1 by each State participating in the combined Federal/State Filer",80,0)
03040   for j=1 to 17
03060     for x=1 to 3
03070 ! fnCHK(J+1,X*20,ST$(STOP2),1) !:
            ! rESP$(RESP+=1)=ST$(J*3-2)
03080     next x
03090   next j
03100   fncmdkey("&Next",1,1,0,"Moves to next questions.")
03110   fncmdkey("&Cancel",5,0,1,"Returns to menu.")
03120   fnacs(sn$,0,mat resp$,ckey)
03130   for j=1 to 51
03140     if resp$(j)="True" then stu(j)=1
03150   next j
03170 ! ______________________________________________________________________
03180 L3180: ! pr NEWPAGE ! commenting this line might screw up window 101 repeat processing
03190   close #102: ioerr L3200
03200 L3200: open #102: "SROW=6,SCOL=20,ERow=20,ECOL=58,Border=SR,Caption=<"&cap$,display,outin 
03210   pr #102: newpage
03220   pr #102,fields "2,2,C 17,N": "Select Form Type:"
03230   for j=1 to 13
03240     wrd2$(j)=cnvrt$("N 2",j)&".  "&tr$(j)(3:18)
03250     io2$(j)=str$(j+1)&",20,C 18"
03260   next j
03270   pr f "21,35,C 09,B,5": "Exit (F5)"
03280 L3280: rinput #102,select mat io2$,attr "H": mat wrd2$
03290   ti1=curfld
03300   if ti1<1 or ti1>udim(tr$) then goto L3280
03310   ti$=tr$(ti1)(1:1)
03320   close #101: ioerr L3330
03330 L3330: open #101: "SROW=3,SCOL=3,ERow=22,ECOL=78,Border=DR,Caption=<"&cap$,display,outin 
03340   pr #101: newpage
03350   pr f "4,05,C 04,R,N": "Type"
03360   pr f "4,11,C 40,R,N": "Category"
03370   for j=1 to 14
03380     if j<10 then pr f str$(j+4)&",11,C 65": amt$(ti1,j)(8:70)
03390     io1$(j)=str$(j+4)&",7,N 2,UT,N"
03400   next j
03410   pr f "17,5,C 65,R,N": "Vendor Type in your file that matches each category:"
03420   pr f "23,30,C 09,B,1": "Next (F1)"
03430   pr f "23,41,C 09,B,5": "Exit (F5)"
03440 L3440: input fields mat io1$: mat ti2 conv L3440
03450   if ce>0 then io1$(ce)(ce1:ce2)="U": ce=0
03460   if cmdkey>0 then goto L3530 else ce=curfld+1
03470   if ce>udim(io1$) then ce=1
03480 L3480: io1$(ce)=rtrm$(uprc$(io1$(ce))) : ce1=pos(io1$(ce),"U",1)
03490   ce2=ce1+1 : io1$(ce)(ce1:ce1)="UC" : goto L3440
03500 CONV2: if ce>0 then io1$(ce)(ce1:ce2)="U"
03510   ce=cnt+1
03520 ERR2: pr f "24,78,C 1": bell : goto L3480
03530 L3530: if cmdkey=5 then goto XIT
03540   if sum(ti2)<1 then goto L3440
03550   ai$=""
03560   for j=1 to 12
03570     if ti2(j)=0 then goto L3590
03580     ai$(j:j)=str$(j)
03590 L3590: next j
03600   gosub PROCESS
03610 ASKDAT: ! 
03620   fntos(sn$="VendorTransList") !:
        mylen=28 : mypos=mylen+3
03630   fnlbl(1,1,"Transaction Starting Date:",mylen,1)
03640   fntxt(1,mypos,8,0,0,'CCYYMMDD',0,'Normally you would enter the first day of the calendar year.') !:
        resp$(1)=str$(transactionstartingdate)
03650   fnlbl(2,1,"Transaction Ending Date:",mylen,1)
03660   fntxt(2,mypos,8,0,0,'CCYYMMDD',0,'You should enter the last day of the calendar year.') !:
        resp$(2)=str$(transactionendingdate)
03670   fnlbl(2,1,"",45,1)
03680   fncmdset(2)
03690   fnacs(sn$,0,mat resp$,ckey)
03700 ! 
03710   transactionstartingdate=val(resp$(1))
03720   transactionendingdate=val(resp$(2))
03730   open #paymstr=1: "Name="&env$('Q')&"\GLmstr\PayMstr.h"&env$('cno')&",Version=1,KFName="&env$('Q')&"\GLmstr\PayIdx1.h"&env$('cno')&",Shr",internal,outin,keyed 
03740   open #trans=2: "Name="&env$('Q')&"\GLmstr\GLTR1099.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\gltrIdx1.h"&env$('cno')&",Shr",internal,input,keyed 
03750   if ct1=0 then open #22: "Name=IRSTAX,RecL=750,eol=crlf,Replace",display,output 
03760   if ct1=0 then gosub RECT
03770   gosub RECA
03780   if lrec(1)=0 then goto L3800
03790 L3790: pr f "12,32,N 3,UT,N": r1/lrec(1)*100
03800 L3800: read #1,using 'Form Pos 1,C 8,4*c 30,x 5,n 2,c 11,x 6,c 12',release: vn$,nam$,ad1$,ad2$,csz$,typ,ss$,ph$ eof END1
03810   restore #2,key>=vn$: nokey L3800
03820   ytdp=0 ! do not use ytdp from payee record
03830 L3830: read #trans,using 'Form POS 1,c 8,N 6,PD 5.2,C 12,C 30,PD 3',release: trvn$,dt,am,rn$,de$,nta eof L3900
03840   if trim$(trvn$)<>trim$(vn$) then goto L3900
03850   x=fndate_mmddyy_to_ccyymmdd(dt)
03860   if x<transactionstartingdate then goto L3830
03870   if x>transactionendingdate then goto L3830
03880   ytdp+=am
03890   goto L3830
03900 L3900: ! vN$="12345678": nAM$="JOE JONES": aD1$="11014 HWY 206": aD2$="PO BOX 925": cSZ$="HARRISON, AR 72601": yTDP=655.55: tYP=7: sS$="123456789"
03910   form pos 1,c 8,c 35,3*c 20,pd 5.2,n 2,c 11
03920   for j=1 to 12
03930     if typ=0 then goto L3980
03940     if typ><ti2(j) then goto L3970
03950     if val(amt$(ti1,j)(3:6))>ytdp then goto L3790
03960     amt(j)=ytdp*100 : goto L3990
03970 L3970: next j
03980 L3980: goto L3790
03990 L3990: r1=r1+1
04000   p1=pos(csz$,",",1)
04010   if p1=0 then p1=pos(csz$," ",1)
04020   c$=csz$(1:p1-1)
04030   s$=uprc$(csz$(p1+2:p1+3))
04040   p2=len(rtrm$(csz$))
04050   p1=p2-4
04060   z$=csz$(p1:p2)
04070   ssn=0: tin$=" "
04080   for j=1 to 11
04090     if ss$(j:j)=>"0" and ss$(j:j)<="9" then goto L4130
04100     if j=3 then tin$="1"
04110     if j=4 or j=7 then tin$="2"
04120     ss$(j:j)=""
04130 L4130: next j
04140   if len(ss$)><9 then goto L4180
04150   ssn=val(ss$)
04160   if tin$><" " then goto L4460
04170 ! ______________________________________________________________________
04180 L4180: pr newpage
04190   close #103: ioerr L4200
04200 L4200: open #103: "SROW=7,SCOL=8,EROW=15,ECOL=72,Border=Sr,Caption=<"&cap$,display,outin 
04210   io3$(1)="7,55,Cu 1,UT,N"
04220   io3$(2)="8,55,C 9,UT,N"
04230   pr #103,fields "4,2,C 60,N": "Vendor Number: "&ltrm$(vn$)
04240   pr #103,fields "5,2,C 60,N": "         Name: "&nam$
04250   pr #103,fields "2,2,Cc 63,H": "Unable to determine Federal ID or Social Security Number."
04260   pr #103,fields "7,2,Cr 52,N": "[F]ederal ID, [S]ocial Security Number or [N]either:"
04270   pr #103,fields "8,2,Cr 52,N": "Federal ID or Social Security Number:"
04280   ss$=ss$(1:9)
04290   pr #103,fields "1,1,C 7,N": hex$("07")
04300 L4300: pr f "16,35,C 09,B,5": "Stop (F5)"
04310   if tin$="1" then tinfs$="F"
04320   if tin$="2" then tinfs$="S"
04330   if tin$=" " then tinfs$="N"
04340 L4340: rinput #103,fields mat io3$: tinfs$,ss$ conv L4300
04350   if tinfs$="F" then tin$="1"
04360   if tinfs$="S" then tin$="2"
04370   if tinfs$="N" then tin$=" "
04380   if tinfs$<>"F" and tinfs$<>"S" and tinfs$<>"N" then goto L4340
04390   ssn=val(ss$) conv L4340
04400   if tin$><" " and len(rtrm$(ss$))><9 then goto L4340
04410   if tin$=" " and len(rtrm$(ss$))><0 then goto L4340
04420   if tin$><" " and ssn=0 then goto L4300
04430   if tin$=" " or tin$="1" or tin$="2" then goto L4450 else goto L4300
04440   close #103: 
04450 L4450: gosub PROCESS
04460 L4460: if tin$="1" then ln4$=uprc$(nam$(1:4)) : goto L4530
04470   p1=len(rtrm$(nam$))
04480   if p1=0 then ln4$="": goto L4530
04490   for j=p1 to 1 step -1
04500     if nam$(j:j)=" " then goto L4520
04510   next j
04520 L4520: ln4$=nam$(j+1:j+4)
04530 L4530: for j=1 to 51
04540     if s$><st$(j)(1:2) then goto L4610
04550     if stu(j)=1 then ps$=st$(j)(4:5) else ps$="  "
04560     for j1=1 to 12
04570       kamt(j,j1)=kamt(j,j1)+amt(j1)
04580     next j1
04590     knp(j)=knp(j)+1
04600     goto L4750
04610 L4610: next j
04620   pr newpage
04630   close #103: ioerr L4640
04640 L4640: open #103: "SROW=7,SCOL=14,EROW=15,ECOL=65,BORDER=SR,CAPTION=<"&cap$,display,outin 
04650   pr #103: newpage
04660   pr #103,fields "2,2,Cc 50,H,N": "invalid State Code encountered"
04670   pr #103,fields "1,1,C 7,N": hex$("07")
04680   pr #103,fields "04,2,C 50,N": "Vendor Number: "&ltrm$(vn$)
04690   pr #103,fields "05,2,C 50,N": "  Vendor Name: "&nam$
04700   pr #103,fields "06,2,C 45,N": "  City ST Zip: "&csz$
04710   pr #103,fields "08,2,C 19,N": "Correct State Code:"
04720   rinput #103,fields "08,22,Cu 2,UT,N": s$
04730   gosub PROCESS
04740   goto L4530
04750 L4750: if orc$="C" then cri$="G" else cri$=""
04760   gosub RECB
04770   goto L3790
04780 ! ______________________________________________________________________
04790 RECT: ! 
04800   seq=seq+1: pr #22,using L4810: "T",yr,"",b1,tcc$," "," "," ",fic$,a$(1)," ",a$(1)," ",a$(2),city$,st$,zip$," ",1,cn$,cpn," "," "," "," "," ",seq," ","V","Advanced Computer Services, Inc.","P O Box 758","Harrison","AR","72601","Ken Johnson","8707415447","acs1@alltel.net"," "," "
04810 L4810: form pos 1,c 1,n 4,c 1,n 9,c 5,c 2,c 5,c 1,c 1,6*c 40,c 2,c 9,c 15,pic(########),c 40,n 15,c 35,c 2,c 15,c 6,c 83,pic(########),c 10,c 1,c 40,c 40,c 40,c 2,c 9,c 40,c 15,c 35,c 9,c 2
04820   return 
04830 ! ______________________________________________________________________
04840 RECA: ! 
04850   seq=seq+1: pr #22,using L4860: "A",yr," ",b1,pnc$," ",cfs$,ti$,ai$," ",orc2$," ",fci$,a$(1)," ",tai,a$(2),city$,st$,zip$,ppn," ",seq," "," "
04860 L4860: form pos 1,c 1,n 4,c 6,g 9,c 4,3*c 1,c 12,c 8,c 3,c 1,c 1,2*c 40,n 1,2*c 40,c 2,c 9,n 15,pos 240,c 260,pos 500,pic(########),c 231,c 2
04870   return 
04880 ! ______________________________________________________________________
04890 RECB: ! 
04900   totalb=totalb+1
04910   seq=seq+1: pr #22,using L4920: "B",yr,cri$," ",tin$,ss$,vn$," "," ",mat amt,"",fci$,nam$,"","",ad1$,"",c$,s$,z$,"",seq,"","","",0,0,"",""
04920 L4920: form pos 1,c 1,n 4,c 1,c 4,c 1,c 9,c 20,c 4,c 10,12*pic(############),c 48,c 1,6*c 40,c 2,c 9,c 1,pic(########),pos 508,c 36,pos 544,c 119,c 60,2*pic(##########),c 2,c 2
04930   mat camt=camt+amt
04940   cnp=cnp+1
04950   tnp=tnp+1
04960   mat amt=(0)
04970   return 
04980 ! ______________________________________________________________________
04990 RECC: ! 
05000   seq=seq+1: pr #22,using L5010: "C",cnp,"",mat camt,"",seq," "," "
05010 L5010: form pos 1,c 1,pic(########),c 6,12*pic(##################),c 268,pic(########),c 231,c 2
05020   mat camt=(0)
05030   cnp=0
05040   return 
05050 ! ______________________________________________________________________
05060 RECK: ! 
05070   for j=1 to 51
05080     if knp(j)=0 or stu(j)=0 then goto L5110
05090     seq=seq+1: pr #22,using L5100: "K",knp(j),"",kamt(j,1),kamt(j,2),kamt(j,3),kamt(j,4),kamt(j,5),kamt(j,6),kamt(j,7),kamt(j,8),kamt(j,9),kamt(j,10),kamt(j,11),kamt(j,12),"",seq," ","","","",st$(j)(4:5),""
05100 L5100: form pos 1,c 1,pic(########),c 6,12*pic(##################),c 268,pic(########),c 18,c 18,c 4,c 2,c 2
05110 L5110: next j
05120   mat kamt=(0)
05130   mat knp=(0)
05140   return 
05150 ! ______________________________________________________________________
05160 RECF: ! 
05170   seq=seq+1: pr #22,using L5180: "F",tnp,"",totalb,"",seq," "
05180 L5180: form pos 1,c 1,pic(########),"000000000000000000000",c 19,pic(########),c 442,pic(########),c 241,c 2
05190   return 
05200 ! ______________________________________________________________________
05210 END1: gosub RECC
05220   gosub RECK
05230   close #1: 
05240   pr newpage
05250   close #104: ioerr L5260
05260 L5260: open #104: "SROW=7,SCOL=8,EROW=09,ECOL=72,Border=SR,Caption=<"&cap$,display,outin 
05270   pr #104,fields "2,2,C 58,N": "Do you have another type of return for this company (Y/N):"
05280   yn$="N" ! default
05290 L5290: rinput #104,fields "2,61,Cu 1,UT,N": yn$ conv L5290
05300   if yn$="Y" then ct1=1
05310   if yn$="N" then ct1=0
05320   if yn$<>"N" and yn$<>"Y" then goto L5290
05330   if ct1=1 then goto L2340
05340   gosub RECF
05350   close #22: ioerr L5370
05360   gosub L5600
05370 L5370: goto XIT
05380 ! ______________________________________________________________________
05390 XIT: fnxit
05400 ! ______________________________________________________________________
05410 PROCESS: pr newpage
05420   close #101: ioerr L5430
05430 L5430: open #101: "SROW=10,SCOL=20,EROW=12,ECOL=59,BORDER=DR,CAPTION=<"&cap$,display,outin 
05440   pr f "10,35,C 10,N": "processing"
05450   pr f "13,34,C 11,B,5": "Cancel (F5)"
05460   pr f "12,32,C 20": "  0% COMPLETED"
05470   return 
05480 ! ______________________________________________________________________
05490 CSZ: ! EXTRACT  CITY$,STATE$,ZIP$ FORM CSZ$
05500 L5500: p1=pos(csz$,".",1)
05510   if p1>0 then csz$(p1:p1)="": goto L5500
05520   p1=pos(csz$,",",1)-1
05530   if p1=-1 then p1=pos(csz$," ",1)-1
05540   p2=pos(csz$," ",p1+3)
05550   city$=uprc$(rtrm$(csz$(1:p1))(1:15))
05560   state$=uprc$(rtrm$(csz$(p2-2:p2))(1:2))
05570   zip$=uprc$(ltrm$(rtrm$(csz$(p2+1:25)))(1:9))
05580   return 
05590 ! ______________________________________________________________________
05600 L5600: close #24: ioerr L5620
05610   dim a$*750
05620 L5620: close #25: ioerr L5630
05630 L5630: open #24: "Name=X,RecL=751,EOL=NONE,Replace",external,output 
05640   open #25: "Name=irstax,RecL=750",display,input 
05650 L5650: linput #25: a$ eof L5700
05660   if a$(750:750)="X" then a$(750:750)=""
05670   write #24,using L5680: rpad$(a$,750),chr$(10)
05680 L5680: form pos 1,c 750,c 1
05690   goto L5650
05700 L5700: close #24: 
05710   close #25: 
05720   execute "COPY x a:irstax"
05730   return 
05740 ! ______________________________________________________________________
05750 ! <Updateable Region: ERTN>
05760 ERTN: fnerror(program$,err,line,act$,"xit")
05770   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
05780   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
05790   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
05800 ERTN_EXEC_ACT: execute act$ : goto ERTN
05810 ! /region
05820 ! ______________________________________________________________________
