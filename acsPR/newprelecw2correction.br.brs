00010 ! REPLACE S:\acsPR\newprElecW2correction
00020 ! never could get it work with an RO record if all zeroes; without !:
        ! RO record i had to put an RE record in front of every RW record and !:
        ! had to follow with an RT record - Right now this program will not !:
        ! create an RO record
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnerror,fnwin3b,fnwait,fnDedNames,fnoldmsgbox,fntop,fnxit,fnconsole,fnAcs,fntop,fnLbl,fnTxt,fnTos,fnFra,fnCmdKey,fnmsgbox
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim em$(3)*30,ss$*11,d(14),tcp(32),s(9),t(9),z$*8,cap$*128,message$*40
00080   dim tdc(10),newdedcode(20),newcalcode(20),newdedfed(20),dedfica(20)
00090   dim dedst(20),deduc(20),abrevname$(20)*8,fullname$(20)*20
00100   dim a$(3)*40,b$*12,g$*12,d$(10)*8,tty(10),e$(10)*12,s2(2)
00110   dim fa$(1),fb$(1),fc$(1),fd$(1),l$(10),dedfed(10),w3(2),i2(2),t2(2)
00120   dim emppin$*17,tlcn$*6,contact$*27,contactph$*15,phoneext$*5,email$*40
00130   dim ml$(2)*80,orgw32$*11,w32$*11
00140   dim w2(9),i1(9),t1(9),ct$*20,st$*2,ibm$*8,namcde$*1,typemp$*1,io1$(18)
00150   dim orgt1(9),orgt2(2)
00160   dim orgw2(9),orgw3(2)
00170   dim terminat$*1,first$*15,mid$*15,last$*20,resp$(20)*40,path$*30
00180 ! ______________________________________________________________________
00190   fntop("S:\acsPR\prElecW2",cap$="Electronic W-2")
00230   on fkey 5 goto XIT
00250 ! 
00260   open #1: "Name=[Q]\PRmstr\Company.h[cno],Shr",internal,input 
00270   read #1,using L280: mat a$,b$,mat d$,loccode,mat e$,mat dedfed,oldmax
00280 L280: form pos 1,3*c 40,c 12,pos 150,10*c 8,n 2,pos 317,10*c 12,pos 638,10*n 1,pos 239,pd 4.2
00290   close #1: 
00300 ! ______________________________________________________________________
00310 DATE_SCREEN: ! 
00320 L320: fnTos(sn$="W2-1") !:
        rc=cf=0: mylen=34 : mypos=mylen+3
00330   fnFra(1,1,3,60,"Date Range for Corrected W2's","Normally this would the first and last day of the calendar year",0) !:
        cf+=1 : fratype=cf
00340   fnLbl(1,1,"Starting Date:",mylen,1,0,1)
00350   fnTxt(1,mypos,10,0,1,"3",0,"First day of calendar year",1) !:
        resp$(rc+=1)=str$(beg_date)
00360   fnLbl(2,1,"Ending Date:",mylen,1,0,1)
00370   fnTxt(2,mypos,10,0,1,"3",0,"Last day of calendar year",1) !:
        resp$(rc+=1)=str$(end_date)
00380   fnLbl(3,1,"Output File Designation and Name:",mylen,1,0,1)
00390   fnTxt(3,mypos,30,0,0,"",0,"Destination and file name you wish to use.",1) !:
        resp$(rc+=1)="c:\w2report"
00400   fnFra(7,1,3,60,"Date Range used on Original W2's","This could be any date rqnge entered by mistake",0) !:
        cf+=1 : fratype=cf
00410   fnLbl(1,1,"Original Starting Date:",mylen,1,0,2)
00420   fnTxt(1,mypos,10,0,1,"3",0,"First day of calendar year used on original submission of W2s",2) !:
        resp$(rc+=1)=str$(orgbeg_date)
00430   fnLbl(2,1,"Original Ending Date:",mylen,1,0,2)
00440   fnTxt(2,mypos,10,0,1,"3",0,"Last day of calendar year used on original submission of W2s",2) !:
        resp$(rc+=1)=str$(orgend_date)
00450   fnCmdKey("Next",1,1,0,"Prints the report")
00460   fnCmdKey("Cancel",5,0,1,"Returns to menu")
00470   fnAcs(sn$,0,mat resp$,ckey) !:
        if ckey=5 then goto XIT
00480   beg_date=val(resp$(1))
00490   end_date=val(resp$(2))
00500   path$=resp$(3)
00510   orgbeg_date=val(resp$(4))
00520   orgend_date=val(resp$(5))
00530   if beg_date=0 or end_date=0 then goto L320
00540   if orgbeg_date=0 or orgend_date=0 then goto L320
00550   fnconsole(1)
00560 L560: p1=pos(b$,"-",1)
00570   if p1=0 then goto L600
00580   b$(p1:p1)=""
00590   goto L560
00600 L600: ! 
00610   b1=val(b$)
00620   p1=pos(a$(3),",",1): comma=1
00630   if p1=0 then p1=pos(a$(3)," ",1): comma=0
00640   ct$=a$(3)(1:p1-1)
00650   if comma=1 then st$=a$(3)(p1+2:p1+3) else st$=a$(3)(p1+1:p1+2)
00660   p2=len(rtrm$(a$(3)))
00670   p1=p2-4
00680   zip$=a$(3)(p1:p2)
00690   if val(date$(1:2))-1 <70 then yr=2000+val(date$(1:2))-1 else yr=1900+val(date$(1:2))-1
00700   if date$(4:5)="12" then yr=yr+1 ! if you run in december, add 1 year back
00710   io1$(1)="5,25,C 40,UT,N"
00720   io1$(2)="6,25,C 40,UT,N"
00730   io1$(3)="7,25,C 20,UT,N"
00740   io1$(4)="8,25,C 2,UT,N"
00750   io1$(5)="9,25,C 5,UT,N"
00760   io1$(6)="10,25,N 9,UT,N"
00770   io1$(7)="11,25,N 4,UT,N"
00780   io1$(8)="12,25,N 10.2,UT,N"
00790   io1$(9)="13,25,N 10.4,UT,N"
00800   io1$(10)="14,25,N 10.2,UT,N"
00810   io1$(11)="15,25,N 10.4,UT,N"
00820   io1$(12)="16,75,N 2,UT,N"
00830   io1$(13)="17,47,N 2,UT,N"
00840   io1$(14)="18,61,N 2,UT,N"
00850   io1$(15)="19,65,N 2,UT,N"
00860   io1$(16)="20,35,C 8,UT,N"
00870   io1$(17)="21,52,C 1,UT,N"
00880   io1$(18)="22,38,C 1,UT,N"
00890   ibm$="IBM"
00900   namcde$="F"
00910   typemp$="R"
00920 ! ______________________________________________________________________
00930 SCR1: ! 
00940   goto L1220
00950   fnTos(sn$="ElecW2-2") !:
        rc=cf=0: mylen=30 : mypos=mylen+3
00960   fnLbl(1,1,"Position Diskette in Drive A",mylen+25,1,0,0)
00970   fnLbl(3,1,"Company Name:",mylen,1,0,0)
00980   fnTxt(3,mypos,40,0,0,"",0,"Enter the name of the company submitting the files",0) !:
        resp$(rc+=1)=a$(1)
00990   fnLbl(4,1,"Street Address:",mylen,1,0,0)
01000   fnTxt(4,mypos,40,0,0,"",0,"Enter the address of the company submitting the files",0) !:
        resp$(rc+=1)=a$(2)
01010   fnLbl(5,1,"City:",mylen,1,0,0)
01020   fnTxt(5,mypos,22,0,0,"",0,"Enter the city of the company submitting the files",0) !:
        resp$(rc+=1)=ct$
01030   fnLbl(6,1,"State:",mylen,1,0,0)
01040   fnTxt(6,mypos,2,0,0,"",0,"Enter the state forthe company being submitted.",0) !:
        resp$(rc+=1)=st$
01050   fnLbl(7,1,"Zip Code:",mylen,1,0,0)
01060   fnTxt(7,mypos,5,0,0,"",0,"Enter the zip code for the company being submitted.",0) !:
        resp$(rc+=1)=zip$
01070   fnLbl(8,1,"Federal ID #:",mylen,1,0,0)
01080   fnTxt(8,mypos,9,0,0,"30",0,"Enter the Federal Id number without slashes or dashes.",0) !:
        resp$(rc+=1)=str$(b1)
01090   fnLbl(9,1,"Payment Year:",mylen,1,0,0)
01100   fnTxt(9,mypos,4,0,0,"30",0,"Enter the year for which the wages were paid in ccyy format.",0) !:
        resp$(rc+=1)=str$(yr)
01110   fnLbl(10,1,"Social Security Maximum Wage:",mylen,1,0,0)
01120   fnTxt(10,mypos,10,0,0,"10",0,"Enter the social security maximum wage for the year just completed.",0) !:
        resp$(rc+=1)=str$(ssmax)
01130   fnLbl(11,1,"Social Security Rate:",mylen,1,0,0)
01140   fnTxt(11,mypos,6,0,0,"34",0,"Enter the social security rate for the year just completed.",0) !:
        resp$(rc+=1)=str$(ssrate)
01150   fnLbl(12,1,"Medicare Maximum Wage:",mylen,1,0,0)
01160   fnTxt(12,mypos,10,0,0,"10",0,"Enter the medicare maximum wage for the year just completed.",0) !:
        resp$(rc+=1)=str$(mcmax)
01170   fnLbl(13,1,"Medicare Rate:",mylen,1,0,0)
01180   fnTxt(13,mypos,6,0,0,"34",0,"Enter the medicare rate for the year just completed.",0) !:
        resp$(rc+=1)=str$(mcrate)
01190   fnCmdKey("Next",1,1,0,"Proceed with submission")
01200   fnCmdKey("Cancel",5,0,1,"Returns to menu")
01210   fnAcs(sn$,0,mat resp$,ckey) !:
        if ckey=5 then goto XIT
01220 L1220: pr newpage
01230   close #101: ioerr L1240
01240 L1240: open #101: "SROW=2,SCOL=3,EROW=23,ECOL=77,BORDER=DR,CAPTION=<Create Electronic W2 Diskette for I.R.S.",display,outIn 
01250   pr f "3,15,C 51,R,N": "  INSERT DISKETTE FOR ELECTRONIC W2'S IN DRIVE A:"
01260   pr f "5,5,C 60": "Company Name:"
01270   pr f "6,5,C 60": "Street Address:"
01280   pr f "7,5,C 60": "City:"
01290   pr f "8,5,C 60": "State:"
01300   pr f "9,5,C 60": "Zip Code:"
01310   pr f "10,5,C 60": "Federal ID #:"
01320   pr f "11,5,C 60": "Payment Year:"
01330   pr f "12,5,C 60": "Soc-Sec Maximum:"
01340   pr f "13,5,C 60": "Soc-Sec Rate:"
01350   pr f "14,5,C 60": "Medicare Maximum:"
01360   pr f "15,5,C 60": "Medicare Rate:"
01370   pr f "16,5,C 70": "Miscellaneous Deduction Containing Employer Cost Group-Term Life Ins:"
01380   pr f "17,5,C 70": "Miscellaneous Deduction Used For Pension:"
01390   pr f "18,5,C 70": "Miscellaneous Deduction Used For Deferred Compensation:"
01400   pr f "19,5,C 70": "Miscellaneous Deduction Used For Dependent Care Assistance:"
01410   pr f "20,5,C 60": "Computer Manufacturer's Name:"
01420   pr f "21,5,C 60,N": "F=First Name First or S=Surname First on File:"
01430   pr f "22,5,C 60": "Type of Business Code R=Regular:"
01440   pr f "24,28,C 9,B,1": "Next (F1)"
01450   pr f "24,39,C 11,B,5": "Cancel (F5)"
01460   pr f mat io1$: a$(1),a$(2),ct$,st$,zip$,b1,yr,87900,.062,999999,.0145,ins,pen,dfc,dcan,ibm$,namcde$,typemp$
01470 L1470: input fields mat io1$,attr "R": a$(1),a$(2),ct$,st$,zip$,b1,yr,ssmax,ssrate,mcmax,mcrate,ins,pen,dfc,dcan,ibm$,namcde$,typemp$ conv CONV1
01480   if ce>0 then io1$(ce)(ce1:ce2)="U": ce=0
01490   if cmdkey>0 then goto L1560 else ce=curfld+1
01500   if ce>udim(io1$) then ce=1
01510 L1510: io1$(ce)=rtrm$(uprc$(io1$(ce))) : ce1=pos(io1$(ce),"U",1)
01520   ce2=ce1+1 : io1$(ce)(ce1:ce1)="UC" : goto L1470
01530 CONV1: if ce>0 then io1$(ce)(ce1:ce2)="U"
01540   ce=cnt+1
01550 ERR1: pr f "24,78,C 1": bell : goto L1510
01560 L1560: ! 
01570   if cmdkey=5 then goto XIT
01580   if rtrm$(a$(1))="" then ce=1: goto ERR1
01590   if rtrm$(a$(2))="" then ce=2: goto ERR1
01600   if rtrm$(ct$)="" then ce=3: goto ERR1
01610   if rtrm$(st$)="" then ce=4: goto ERR1
01620   if rtrm$(zip$)="" then ce=5: goto ERR1
01630   if b1=0 then ce=6: goto ERR1
01640   if yr<2001 then ce=7: goto ERR1
01650   ficarate=ssrate+mcrate
01660   if ssmax<53400 then ce=8: goto ERR1
01670   if ins<0 or ins>10 then ce=9: goto ERR1
01680   if pen<0 or pen>10 then ce=10: goto ERR1
01690   if dfc<0 or dfc>10 then ce=11: goto ERR1
01700 ! ______________________________________________________________________
01710   mat io1$(2)
01720   io1$(1)="12,71,N 2,UT,N"
01730   io1$(2)="14,71,N 2,UT,N"
01740   close #101: ioerr L1750
01750 L1750: pr newpage
01760   open #101: "SROW=7,SCOL=2,EROW=16,ECOL=79,BORDER=DR,CAPTION=<Electronic W-2   State Reporting Information",display,outIn 
01770   pr f "8,4,C 72": "Some states require filing W2's on diskette.  Answer the following"
01780   pr f "9,4,C 72": "questions if you wish to create 'RS' records during this run."
01790   pr f "12,8,Cr 62": "State code used in your record to identify the selected state:"
01800   pr f "14,8,Cr 62": "Appropriate FIPS postal numeric code:"
01810   pr f "15,8,C 70": "(See an appendix in your electronic booklet for the postal code!)"
01820   pr f "17,28,C 9,B,1": "Next (F1)"
01830   pr f "17,39,C 11,B,5": "Cancel (F5)"
01840 L1840: input fields mat io1$: sr1,sr2 conv L1840
01850   if ce>0 then io1$(ce)(ce1:ce2)="U": ce=0
01860   if cmdkey>0 then goto L1930 else ce=curfld+1
01870   if ce>udim(io1$) then ce=1
01880 L1880: io1$(ce)=rtrm$(uprc$(io1$(ce))) : ce1=pos(io1$(ce),"U",1)
01890   ce2=ce1+1 : io1$(ce)(ce1:ce1)="UC" : goto L1840
01900 CONV2: if ce>0 then io1$(ce)(ce1:ce2)="U"
01910   ce=cnt+1
01920 ERR2: pr f "24,78,C 1": bell : goto L1880
01930 L1930: if cmdkey=5 then goto XIT
01940   if sr1<0 or sr1>udim(e$) then ce=1: goto ERR2
01950   if sr1>0 and sr2=0 then ce=2: goto ERR2
01960 ! ______________________________________________________________________
01970   gosub SCR2
01980   pr newpage
01990   win=101
02000   message$=""
02020 ! ______________________________________________________________________
02030   fnDedNames(mat fullname$,mat abrevname$,mat newdedcode,mat newcalcode,mat newdedfed,mat dedfica,mat dedst,mat deduc)
02040   open #1: "Name=[Q]\PRmstr\RPMSTR.h[cno],KFName=[Q]\PRmstr\RPINDEX.h[cno],Shr",internal,input,keyed 
02050 ! Open #2: "Name=[Q]\PRmstr\RPTRAIL.h[cno],Shr",Internal,Input,Relative
02060   open #4: "Name=[Q]\PRmstr\payrollchecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno]",internal,outIn,keyed 
02070 L2070: open #22: "Name=W2REPORT,RecL=1024,eol=crlf,replace",display,output 
02080   goto L2140
02090   pr newpage
02100   msgline$(1)="Insert Diskette"
02110   mtype=1
02120   if err=4221 then gosub L4930 ! fnOldMsgBox(MAT RESPONSE$,CAP$,MAT MSGLINE$,MTYPE)
02130   goto L2070
02140 L2140: gosub RECRCA
02150 ! GOSUB RECRE
02160 L2160: ! pr f "12,32,N 3,UT,N": R1/LREC(1)*100
02170 L2170: read #1,using L2190: eno,mat em$,ss$,em6,ta eof END1
02180   gosub L4380
02190 L2190: form pos 1,n 8,3*c 30,c 11,pos 122,n 2,pos 173,pd 3
02200   r1=r1+1
02210   p1=pos(em$(3),",",1) : comma=1
02220   if p1=0 then p1=pos(em$(3)," ",1): comma=0
02230   emct$=em$(3)(1:p1-1)
02240   gosub L5770: emst$=holdst$ ! If COMMA=1 Then eMST$=EM$(3)(P1+2:P1+3) Else eMST$=EM$(3)(P1+1:P1+2)
02250   p2=len(rtrm$(em$(3)))
02260   p1=p2-4
02270   emzip$=em$(3)(p1:p2)
02280 L2280: p1=pos(ss$,"-",1)
02290   if p1>0 then ss$(p1:p1)="": goto L2280 else ssn=val(ss$)
02300   checkkey$=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",0)&cnvrt$("pd 6",0) ! index employee#,department# and payroll date
02310   restore #4,key>=checkkey$: nokey L2170
02320 L2320: read #4,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat tcp eof L2680
02330   if heno<>eno then goto L2680
02340   if prd<beg_date or prd>end_date then goto L2320 ! not this year
02350   form pos 1,n 8,pos 48,n 2,pos 168,21*pd 5.2,pos 468,pd 3
02360   if tcd<1 or tcd>10 then tcd=1
02370 ! finish with this employee
02380   dedret=0
02390   cafded=0
02400   for j=1 to 20
02410     if newdedfed(j)=1 then goto L2420 else goto L2430
02420 L2420: dedret=dedret+tcp(j+4)
02430 L2430: if dedfica(j)=1 then goto L2440 else goto L2450
02440 L2440: cafded=cafded+tcp(j+4)
02450 L2450: next j
02460   w2(1)=min(w2(1)+tcp(31)-tcp(30)-cafded,ssmax-tcp(30)) ! TOTAL SOC-SEC WAGES
02470   w3=w3+tcp(2) ! TOTAL FICA WITHHELD
02480   w3(1)=w3(1)+tcp(31)-cafded ! TOTAL MEDICARE WAGES & TIPS
02490 ! if env$('client')="Washington Parrish" then w3(1)=w3(1)+tcp(5) ! add deferred comp match to medicare wages
02500   w3(1)=min(mcmax,w3(1)) ! MC WAGES CANNOT EXCEED MAXIMUM
02510   if uprc$(med$)="Y" then w2=w2+tcp(2) else w2=round(min(w3/(ssrate+mcrate)*ssrate,ssmax*ssrate),2) ! SS WH
02520   if uprc$(med$)="Y" then w3(2)=w3(2)+tcp(3) else w3(2)=w3-w2 ! MEDICARE WITHHELD
02530   w2(2)=w2(2)+tcp(30) ! FICA TIPS YTD
02540   w2(3)=w2(3)+tcp(31)-dedret ! TOTAL FEDERAL WAGES
02550   w2(4)=w2 ! W2(4)+tcp(2) ! FICA W/H YTD       (COULD BE +W2 INSTEAD OF +tcp(2)IF ONLY FICA PORTION GOES INTO W2 RECORD)
02560 ! w2(4)=W2 ! PUT SS W/H ONLY IN 2-W RECORD (EXCLUDE MEDICARE W/H)
02570   w2(5)=w2(5)+tcp(1) ! FED W/H YTD
02580   if ins>0 then w2(6)=w2(6)+tcp(4+ins) ! EMPLOYER COST GROUP LIFE INS.
02590   w2(7)=w2(7)+0 ! UNCOLLECTED EMPLOYEE FICA TAX ON TIPS
02600   w2(8)=w2(8)+tcp(24) ! EIC TOTAL
02610   w2(9)=w2(9)+0 ! ALLOCATED TIPS
02620   if dfc>0 then dc1=dc1+tcp(4+dfc)*100 ! DEFERRED COMPENSATION
02630   if dcan>0 then dca=dca+tcp(4+dcan)*100 ! DEPENDENT CARE ASSISTANCE
02640   if sr1><tcd then goto L2670
02650   s2(1)=s2(1)+(tcp(31)*100)
02660   s2(2)=s2(2)+(tcp(4)*100)
02670 L2670: goto L2320
02680 L2680: gosub EXTRACT_ORIGINAL
02690   if em6=9 then w2(1)=w2(4)=w3(1)=w3(2)=0: orgw2(1)=orgw2(4)=orgw3(1)=orgw3(2)=0 ! NO SS OR MC
02700   if em6=1 then w3(1)=w3(2)=0: orgw3(1)=orgw3(2)=0 ! NO MEDICARE
02710   if em6=2 then w2(1)=w2(4)=0: orgw2(1)=orgw2(4)=0 ! NO SOC-SEC
02720   if w2(3)=0 and w2(1)=0 then goto L2160
02725   if sum(w2)=sum(orgw2) and sum(w3)=sum(orgw3) then goto L2160 ! no changes to any dollar figusres
02730   gosub RECRCE
02740   gosub RECRCW
02750   gosub RECRS
02760   tw1=tw1+1
02770   tw2=tw2+1
02780   gosub RECRCT
02790   tw2=0
02800   goto L2160
02810 ! ______________________________________________________________________
02820 RECRCA: pr #22,using L2830: "RCA",b1,emppin$(1:8),"","98",a$(1),"",a$(2)(1:22),ct$,st$,zip$,"","","","","",contact$,contactph$,phoneext$,"",email$,"","","2","L","1",tlcn$,""
02830 L2830: form pos 1,c 3,pic(#########),c 8,c 9,c 2,c 57,c 22,c 22,c 22,c 2,c 5,c 4,c 6,c 23,c 15,c 2,c 27,c 15,c 5,c 3,c 40,c 3,c 10,c 1,c 1,c 1,c 6,c 701
02840   return 
02850 ! ______________________________________________________________________
02860 RECRCE: pr #22,using L2870: "RCE",yr,"",b1,"","","","",a$(1)(1:37),"",a$(2)(1:22),ct$,st$,zip$(1:5),"","","","","","","R","","",""
02870 L2870: form pos 1,c 3,pic(####),c 9,pic(#########),c 1,c 9,c 4,c 4,c 57,c 22,c 22,c 22,c 2,c 5,c 4,c 4,c 23,c 15,c 2,c 1,c 1,c 1,c 1,c 799
02880   return 
02890 ! ______________________________________________________________________
02900   form pos 1,c 2,pic(#########),c 15,c 15,c 20,c 4,c 22,c 22,c 22,c 2,c 5,c 4,c 5,c 23,c 15,c 2,18*pic(###########),c 22,2*pic(###########),c 56,n 1,c 1,c 1,n 1,c 23,
02910   pr #22,using L2920: "2E",ct$,st$,"",zip$,namcde$,typemp$,"","","",""
02920 L2920: form pos 1,c 2,g 25,g 10,2*g 5,2*g 1,g 2,g 4,g 2,c 71
02930   return 
02940 ! ______________________________________________________________________
02950 RECRCW: ! 
02960   for j=1 to 9
02970     w2(j)=w2(j)*100
02980     orgw2(j)=orgw2(j)*100
02990   next j
03000   for j=1 to 2
03010     w3(j)=w3(j)*100
03020     orgw3(j)=orgw3(j)*100
03030   next j
03040   if pen=0 then pen$=" " else pen$="1"
03050   if dfc=0 then dfc$="" else dfc$="D"
03060 ! medicare withholdings  exceptions
03070   orgw32$="           ": w32$="           "
03080   if orgw3(2)=w3(2) then orgw32$="           ": w32$="           " : goto L3100 ! if amounts are the same,then report both as blanks
03090   if orgw3(2)>0 or w3(2)>0 then orgw32$=cnvrt$("pic(###########)",orgw3(2)): w32$=cnvrt$("pic(###########)",w3(2)) ! try to stop accuwage errors when either the original medicare withholdings or the new one is >0
03100 L3100: ! federal withholdings exceptions
03110   w25$="           ": orgw25$="           "
03120   if w2(5)=orgw2(5) then goto L3140 ! try to stop accuwage errors when both both federal wh are same
03130   if w2(5)>0 or orgw2(5)>0 then w25$=cnvrt$("pic(###########)",w2(5)): orgw25$=cnvrt$("pic(###########)",orgw2(5)) ! try to stop accuwage errors when both both federal wh are zeros on indiviuals
03140 L3140: !  total wages
03150   w23$="           ": orgw23$="           "
03160   if w2(3)=orgw2(3) then goto L3180 ! try to stop accuwage errors when both both total wages
03170   if w2(3)>0 or orgw2(3)>0 then w23$=cnvrt$("pic(###########)",w2(3)): orgw23$=cnvrt$("pic(###########)",orgw2(3)) ! try to stop accuwage errors when both both total wages are zeros on indiviuals
03180 L3180: ! ss wages
03190   w21$="           ": orgw21$="           "
03200   if w2(1)=orgw2(1) then goto L3220 ! try to stop accuwage errors when both both ss wages agree
03210   if w2(1)>0 or orgw2(1)>0 then w21$=cnvrt$("pic(###########)",w2(1)): orgw21$=cnvrt$("pic(###########)",orgw2(1)) ! try to stop accuwage errors when both both ss wages are zeros on indiviuals
03220 L3220: ! medicare wages
03230   w31$="           ": orgw31$="           "
03240   if w3(1)=orgw3(1) then goto L3260 ! try to stop accuwage errors when both both medicare wages agree
03250   if w3(1)>0 or orgw3(1)>0 then w31$=cnvrt$("pic(###########)",w3(1)): orgw31$=cnvrt$("pic(###########)",orgw3(1)) ! try to stop accuwage errors when both both medicare wages are zeros on indiviuals
03260 L3260: ! ss wh
03270   w24$="           ": orgw24$="           "
03280   if w2(4)=orgw2(4) then orgw2(4)=0: goto L3300 ! try to stop accuwage errors when both both medicare wages agree !! had to set the orgw2(4) to zero because I could never change the pr line to allow me to make it a alpha field  ??? still don't know why i cannot change it
03290   if w2(4)>0 or orgw2(4)>0 then w24$=cnvrt$("pic(###########)",w2(4)): orgw24$=cnvrt$("pic(###########)",orgw2(4)) ! try to stop accuwage errors when both both ss wh are zeros
03300 L3300: pr #22,using L3310: "RCW",0,ssn,first$,mid$,last$,first$,mid$,last$,"",em$(2)(1:22),emct$,emst$,emzip$,"","","","","",orgw23$,w23$,orgw25$,w25$,orgw21$,w21$,orgw2(4),w24$,orgw31$,w31$,orgw32$,w32$,"","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",pen$,pen$,"","",""
03310 L3310: form pos 1,c 3,2*pic(#########),c 15,c 15,c 20,c 15,c 15,c 20,c 22,c 22,c 22,c 2,c 5,c 4,c 5,c 23,c 15,c 2,6*c 11,pic(###########),5*c 11,28*c 11,c 22,2*c 11,8*c 11,c 187,c 1,c 1,c 1,c 1,c 1,c 1,c 16
03320   return 
03330 RECRS: ! STATE RECORD
03340   if sr1=0 then goto L3370 ! NO STATE SELECTED
03350   if s2(1)=0 and s2(2)=0 then goto L3370 ! NO STATE WAGES
03360   form pos 1,c 2,g 2,c 5,pic(#########),c 15,c 15,c 20,c 4,c 22,c 22,c 22,c 2,c 5,c 4,c 5,c 23,c 15,c 2,c 2,c 6,2*pic(###########),pic(##),2*pic(########),c 5,c 20,c 6,g 2,2*pic(###########),c 10,c 1,2*pic(###########),c 7,c 75,c 75,c 25
03370 L3370: t1=t1+1: mat t1=t1+w2 : orgt1=orgt1+1: mat orgt1=orgt1+orgw2
03380   mat i1=i1+w2
03390   mat i2=i2+w3
03400   mat t2=t2+w3 : mat orgt2=orgt2+orgw3
03410   dc2=dc2+dc1
03420   dc3=dc3+dc1
03430   dca2=dca2+dca
03440   dca3=dca3+dca
03450   w2=w3=dca=dc1=orgw2=wrfw3=orgdca=orgdc1=0
03460   mat w2=(0)
03470   mat w3=(0)
03480   mat s2=(0)
03490   mat orgw2=(0)
03500   mat orgw3=(0)
03510   return 
03520 ! ______________________________________________________________________
03530 ! ______________________________________________________________________
03540 RECRCT: ! 
03550 ! total wages
03560   t13$="               " : orgt13$="               "
03570   if orgt1(3)=t1(3) then goto L3590 ! both equal
03580   if orgt1(3)>0 or t1(3)>0 then orgt13$=cnvrt$("pic(###############)",orgt1(3)): t13$=cnvrt$("pic(###############)",t1(3)) ! try to stop accuwage errors when either total wages are >0
03590 L3590: ! ss wages
03600   t11$="               " : orgt11$="               "
03610   if orgt1(1)=t1(1) then goto L3630 ! both equal
03620   if orgt1(1)>0 or t1(1)>0 then orgt11$=cnvrt$("pic(###############)",orgt1(1)): t11$=cnvrt$("pic(###############)",t1(1)) ! try to stop accuwage errors when either total ss wages are >0
03630 L3630: ! ss wh
03640   t14$="               " : orgt14$="               "
03650   if orgt1(4)=t1(4) then goto L3670 ! both equal
03660   if orgt1(4)>0 or t1(4)>0 then orgt14$=cnvrt$("pic(###############)",orgt1(4)): t14$=cnvrt$("pic(###############)",t1(4)) ! try to stop accuwage errors when either total ss wh are >0
03670 L3670: ! total medicare withholding
03680   t22$="               " : orgt22$="               "
03690   if orgt2(2)=t2(2) then goto L3710 ! both equal
03700   if orgt2(2)>0 or t2(2)>0 then orgt22$=cnvrt$("pic(###############)",orgt2(2)): t22$=cnvrt$("pic(###############)",t2(2)) ! try to stop accuwage errors when either medicare withholdings are >0
03710 L3710: ! medicare wages
03720   orgt21$="               " : t21$="               "
03730   if orgt2(1)=t2(1) then goto L3750
03740   if orgt2(1)>0 or t2(1)>0 then orgt21$=cnvrt$("pic(###############)",orgt2(1)): t21$=cnvrt$("pic(###############)",t2(1)) ! try to stop accuwage errors when either the old or new is greater than zero
03750 L3750: ! federal wh
03760   orgt15$="             " : t15$="             "
03770   if t1(5)=orgt1(5) then goto L3800 ! both are same
03780   if t1(5)>0 or orgt1(5)>0 then t15$=cnvrt$("pic(###############)",t1(5)): orgt15$=cnvrt$("pic(###############)",orgt1(5)) ! try to stop accuwage errors when both were either fed wh >0
03800 L3800: pr #22,using L3820: "RCT",tw2,orgt13$,t13$,orgt15$,t15$,orgt11$,t11$,orgt14$,t14$,orgt21$,t21$,orgt22$,t22$,orgt12$,t12$,"","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""
03810   dc3=0 ! kj 120805
03820 L3820: form pos 1,c 3,pic(#######),14*c 15,26*c 15,c 15,2*c 15,8*c 15,c 234
03830   form pos 1,c 2,pic(#######),7*pic(###############),c 240,9*pic(###############),c 23
03840   t1=0: orgt1=0: mat t1=(0) : mat orgt1=(0)
03850   mat t2=(0): mat orgt2=(0)
03860   return 
03870 ! ______________________________________________________________________
03880 RECRCF: pr #22,using L3890: "RCF",tw1,""
03890 L3890: form pos 1,c 3,pic(#########),c 1012
03900   return 
03910 ! ______________________________________________________________________
03920 END1: ! 
03930 ! Gosub RECRCT
03940   gosub RECRCF
03950   gosub L3980
03960 XIT: fnxit
03970 ! ______________________________________________________________________
03980 L3980: close #24: ioerr L4000
03990   dim a$*1024
04000 L4000: close #22: ioerr L4010
04010 L4010: open #24: "Name=X,RecL=1025,EOL=NONE,REPLACE",external,output 
04020   open #22: "Name=w2report,RecL=1024",display,input 
04030 L4030: linput #22: a$ eof L4080
04040   if a$(1024:1024)="X" then a$(1024:1024)=""
04050   write #24,using L4060: rpad$(a$,1024),chr$(10)
04060 L4060: form pos 1,c 1024,c 1
04070   goto L4030
04080 L4080: close #24: 
04090   close #22: 
04100   execute "COPY x "&path$&" -n"
04110   return 
04120 SCR2: ! 
04130   dim contact$*27,email$*40
04140   win=101
04150   win_height=12: win_width=75: display_cnam=1: button_option=2: gosub L4560 ! fnWin3b(WIN,CAP$,12,62,1,2)
04160   pr #win,fields "04,2,Cr 31,N": "Personal ID Number:" !:
        pr #win,fields "05,2,Cr 31,N": "Resub Indicator:" !:
        pr #win,fields "06,2,Cr 31,N": "Resub TLCN:" !:
        pr #win,fields "07,2,Cr 31,N": "Contact Name:" !:
        pr #win,fields "08,2,Cr 31,N": "Contact Phone Number:" !:
        pr #win,fields "09,2,Cr 31,N": "Contact Phone Extension:" !:
        pr #win,fields "10,2,Cr 31,N": "Contact E-Mail:" !:
        pr #win,fields "11,2,Cr 31,N": "Terminating Business Indicator:"
04170   scr2_io$(1)="04,34,C 17,UT,N" !:
        scr2_io$(2)="05,34,C 01,UT,N" !:
        scr2_io$(3)="06,34,C 06,UT,N" !:
        scr2_io$(4)="07,34,C 27,UT,N" !:
        scr2_io$(5)="08,34,C 15,UT,N" !:
        scr2_io$(6)="09,34,C 05,UT,N" !:
        scr2_io$(7)="10,34,C 40,UT,N" !:
        scr2_io$(8)="11,34,C 01,UT,N"
04180   if resub$="" then resub$="0"
04190 ! If TLCN$="" Then tLCN$="0"
04200   if terminat$="" then terminat$="0"
04210 L4210: rinput #win,fields mat scr2_io$: emppin$,resub$,tlcn$,contact$,contactph$,phoneext$,email$,terminat$ conv CONV_SCR2
04220   med$="Y"
04230   if ce>0 then scr2_io$(ce)(ce1:ce2)="U": ce=0
04240   if cmdkey>0 then goto L4310 else ce=curfld
04250 L4250: ce=ce+1: if ce>udim(scr2_io$) then ce=1
04260 L4260: scr2_io$(ce)=rtrm$(scr2_io$(ce)) : ce1=pos(scr2_io$(ce),"U",9) : if ce1=0 then goto L4250
04270   ce2=ce1+1 : scr2_io$(ce)(ce1:ce1)="UC" : goto L4210
04280 CONV_SCR2: if ce>0 then scr2_io$(ce)(ce1:ce2)="U"
04290   ce=cnt+1
04300 ERR_SCR2: pr f "24,78,C 1": bell : goto L4260
04310 L4310: if resub$<>"0" and resub$<>"1" then ce=2 !:
          goto ERR_SCR2
04320   if resub$="1" and rtrm$(tlcn$)="" then ce=3 !:
          goto ERR_SCR2
04330   if terminat$<>"0" and terminat$<>"1" then ce=8 !:
          goto ERR_SCR2
04340   if uprc$(med$)="Y" or uprc$(med$)="N" then goto L4350 else ce=9: goto ERR_SCR2
04350 L4350: close #win: 
04360   if cmdkey=5 then goto SCR1
04370   return 
04380 L4380: dim first$*15,mid$*15,last$*20,em$(3)*30
04390   em$(1)=uprc$(rtrm$(em$(1))): ! nAMCDE$="s"
04400   x1=pos(em$(1)," ",1)
04410   x2=pos(em$(1)," ",x1+1)
04420   x3=pos(em$(1)," ",x2+1)
04430   if uprc$(namcde$)="S" then goto L4480
04440   first$=em$(1)(1:min(15,max(x1-1,1)))
04450   if x2>0 then mid$=em$(1)(x1+1:x2-1): last$=em$(1)(x2+1:len(em$(1)))
04460   if x2=0 then last$=em$(1)(x1+1:len(em$(1))): mid$=""
04470   goto L4540
04480 L4480: ! last name first
04490   if x1=0 then x1=pos(em$(1),",",1)
04500   if x1>0 and em$(1)(x1-1:x1-1)="," then last$=em$(1)(1:x1-2) else last$=em$(1)(1:max(x1-1,1))
04510   if x2>0 then first$=em$(1)(x1+1:x2-1): mid$=em$(1)(x2+1:len(em$(1)))
04520   if x2=0 then first$=em$(1)(x1+1:len(em$(1)))(1:15): mid$=""
04530   x=pos(first$,",",1): if x>0 then first$(x:x)=""
04540 L4540: ! pr FIRST$,MID$,LAST$
04550   return 
04560 L4560: ! Def Library fnWin3b(WIN,&CAP$,WIN_HEIGHT,WIN_WIDTH,DISPLAY_CNAM,BUTTON_OPTION)
04570 ! Win= Window number to be opened
04580 ! Cap$= Caption to be used
04590 ! Win_Height and Win_Width= seems pretty obvious don't it
04600 ! Display_CNam= 1. Display Cnam and Cno in top 2 lines of window !:
        !               2. Display only Cno in top 1 line
04610 ! button_option= 0. Does nothing. !:
        !                1. Cancel(F5)  !:
        !                2. Next(F1),Cancel(F5) !:
        !                3. Print(F1),Cancel(F5)  !:
        !                4. Save (F1),Cancel(F5) !:
        !                5. Next (F1),Cancel(F5),Search(F6) !:
        !                6. Next (F1),Back(F2),Cancel(F5) !:
        !                7. Save (F1),Delete(F4),Cancel(F5) !:
        !                8. (undefinded)  !:
        ! (button_option>0 is set to have Cancel(F5))
04630   if exists("C:\ACS\Local\Settings\No_Print_Newpage.txt") then goto L4640 else pr newpage
04640 L4640: screen_width=80
04650   screen_height=24
04660   if display_cnam=0 then goto L4680
04680 L4680: sc=max(int(((screen_width-win_width)/2)+1),2)
04690   ec=min(sc+win_width-1,79)
04700   sr=max(int(((screen_height-win_height)/2)+1),2)
04710   er=min(sr+win_height-1,23)
04720 !     pr "win_height="&STR$(WIN_HEIGHT),"win_width="&STR$(WIN_WIDTH)
04730 !     pr "sr="&STR$(SR),"sc="&STR$(SC)
04740 !     pr "er="&STR$(ER),"ec="&STR$(EC) : Pause
04750   close #win: ioerr L4760
04760 L4760: open #win: "SRow="&str$(sr)&",SCol="&str$(sc)&",ERow="&str$(er)&",ECol="&str$(ec)&",Border=Sr,Caption=<"&cap$,display,outIn 
04770   pr #win: newpage
04780   if display_cnam=0 then goto L4810
04790   if display_cnam=1 then !:
          pr #win,fields "1,1,Cc "&str$(win_width)&",R,N": env$('cnam')(1:min(40,win_width)) !:
          pr #win,fields "2,1,Cc "&str$(win_width)&",R,N": "Company Number [cno]"(1:min(40,win_width))
04800   if display_cnam=2 then !:
          pr #win,fields "1,1,Cc "&str$(win_width)&",R,N": "Company Number [cno]"(1:min(40,win_width))
04810 L4810: if button_option=0 then goto L4920
04820   mat fkey$=("") : em$="" : es=0
04830   fkey$(5)="Cancel" ! included by default
04840   if button_option=2 then !:
          fkey$(1)="Next"
04850   if button_option=3 then !:
          fkey$(1)="Print"
04860   if button_option=4 then !:
          fkey$(1)="Save"
04870   if button_option=5 then !:
          fkey$(1)="Next" !:
          fkey$(6)="Search"
04880   if button_option=6 then !:
          fkey$(1)="Next" !:
          fkey$(2)="Back"
04890   if button_option=7 then !:
          fkey$(1)="Save" !:
          fkey$(4)="Delete"
04900   scrline=er+1: gosub L5560 !  fnFKEY(ER+1,MAT FKEY$,MAT DISFK,EM$,ES)
04910 ! 
04920 L4920: return  ! Fnend
04930 L4930: ! mtype=0 means splash    - returns no response                                 ! mostly for "please wait..." and "printing..."                                 ! (anywhere no response is required - no buttons are displyed either)
04940 ! mtype=1 means OK only   - returns no response
04950 ! mtype=2 means Yes or No - returns "Y" or "N"
04960 ! mtype=3 means Yes, No, Cancel - returns "Y" or "N" or ""
04970 ! response$(1)= code you're looking for 2-5 are reserved for future use
04980   close #104: ioerr L4990
04990 L4990: endrow=12
05000   for j=2 to udim(msgline$)
05010     if msgline$(j)<>"" then endrow=endrow+1
05020   next j
05030   open #104: "SRow=10,SCol=09,ERow="&str$(endrow)&",ECol=70,Border=SR,Caption=<"&cap$,display,outIn 
05040   pr #104: newpage
05050   mglinerow=2
05060   for j=1 to udim(msgline$)
05070     pr #104,fields str$(mglinerow+j-1)&",2,Cc 60,N": msgline$(j)
05080   next j
05090   if mtype=1 then pr f str$(endrow+1)&",38,Cc 4,B,1": "Ok"
05100   if mtype=1 then input fields str$(endrow)&",09,C 1,AE,N": pause$
05110   if mtype=2 then pr f str$(endrow+1)&",35,Cc 4,B,21": "Yes"
05120   if mtype=2 then pr f str$(endrow+1)&",40,Cc 4,B,22": "No"
05130 L5130: if mtype=2 then input fields str$(endrow)&",09,Cu 1,AE,N": response$(1)
05140   if mtype=2 and cmdkey=22 then response$(1)="N"
05150   if mtype=2 and cmdkey=21 then response$(1)="Y"
05160   if mtype=2 and response$(1)<>"Y" and response$(1)<>"N" then pr f "24,1,C 7,N": bell$ : goto L5130
05170   if mtype=3 then pr f str$(endrow+1)&",29,Cc 4,B,21": "Yes"
05180   if mtype=3 then pr f str$(endrow+1)&",34,Cc 4,B,22": "No"
05190   if mtype=3 then pr f str$(endrow+1)&",39,C 12,B,22": "Cancel (Esc)"
05200   if mtype=3 then input fields str$(endrow)&",09,Cu 1,AE,N": response$(1)
05210   if mtype=3 and cmdkey=22 then response$(1)="N"
05220   if mtype=3 and cmdkey=21 then response$(1)="Y"
05230   if mtype=3 and cmdkey=99 then response$(1)=""
05240   if mtype=3 and response$(1)<>"Y" and response$(1)<>"N" and response$(1)<>"" then pr f "24,1,C 7,N": bell$ : goto L5130
05250   close #104: ioerr ignore
05260 return  ! Fnend
05410 ! Def Library FNOPENWIN(WIN,SR,SC,ER,EC,&CAP$)
05430   if sr<1 then sr=10
05440   if sc<1 then sc=20
05450   if er<1 then er=14
05460   if ec<1 then ec=59
05470   win_width=ec-sc+1
05480   close #win: ioerr L5490
05490 L5490: open #win: "SRow="&str$(sr)&",SCol="&str$(sc)&",ERow="&str$(er)&",ECol="&str$(ec)&",Border=Sr,Caption=<"&cap$,display,outIn 
05500   pr #win: newpage
05510   pr #win,fields "1,1,Cc "&str$(win_width)&",R,N": env$('cnam')(1:min(40,win_width))
05520   pr #win,fields "2,1,Cc "&str$(win_width)&",R,N": "Company Number [cno]"(1:min(40,win_width))
05530 ! 
05540 ! 
05550   return  ! Fnend
05560 L5560: ! Def Library FNFKEY(SCRLINE,MAT FKEY$,MAT DISFK,&EM$,ES)
05570   totallen=0 !:
        startpos=0
05580   for j=1 to udim(fkey$) ! add ' (Fx)' to each button
05590     if fkey$(j)="" then goto L5620
05600     fkey$(j)=fkey$(j)&" (F"&str$(j)&")" !:
          ! add ' (Fx)' to each button
05610     totallen=totallen+len(fkey$(j))+1
05620 L5620: next j
05630   totallen=totallen+len(rtrm$(em$))+min(len(rtrm$(em$)),1)+es
05640   totallen=totallen-1
05650   startpos=int((80-totallen)/2)+1
05660   pr f str$(scrline)&","&str$(startpos)&",C "&str$(totallen)&",N": rpt$("Ä",totallen)
05670   for j=1 to udim(fkey$)
05680     if fkey$(j)="" then goto L5730
05690     if disfk(j)=1 then pr f str$(scrline)&","&str$(startpos)&",C "&str$(len(fkey$(j)))&",R,"&str$(j): fkey$(j)
05700     if disfk(j)=1 then goto L5720
05710     pr f str$(scrline)&","&str$(startpos)&",C "&str$(len(fkey$(j)))&",B,"&str$(j): fkey$(j)
05720 L5720: startpos=startpos+len(fkey$(j))+1
05730 L5730: next j
05740   if rtrm$(em$)="" then goto L5760
05750   pr f str$(scrline)&","&str$(startpos)&",C "&str$(len(rtrm$(em$))+es)&",R,N": rtrm$(em$)
05760 L5760: return  ! Fnend
05770 L5770: ! extract state name
05780   holdst$="          "
05790   p3=oldp3=0
05800   p4=10
05810   for j=1 to 10
05820     p3=pos(rtrm$(em$(3))," ",p3+1)
05830     if oldp3>p3 then goto L5860 ! end of address reached
05840     if p3>0 then oldp3=p3 else goto L5850
05850 L5850: next j
05860 L5860: for j=1 to 10
05870     if rtrm$(em$(3)(oldp3-j:oldp3-j))="" or em$(3)(oldp3-j:oldp3-j)="," then goto L5880 else p4=p4-1: holdst$(p4:p4)=em$(3)(oldp3-j:oldp3-j): goto L5890
05880 L5880: if rtrm$(holdst$)="" then goto L5890 else goto L5900
05890 L5890: next j
05900 L5900: holdst$=ltrm$(holdst$)(1:2)
05910   if holdst$="TE" then holdst$="TX"
05920   return 
05930 ! ______________________________________________________________________
05940 ! <Updateable Region: ERTN>
05950 ERTN: fnerror(program$,err,line,act$,"xit")
05960   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
05970   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
05980   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
05990 ERTN_EXEC_ACT: execute act$ : goto ERTN
06000 ! /region
06010 ! ______________________________________________________________________
06020 EXTRACT_ORIGINAL: ! 
06030   restore #4,key>=checkkey$: nokey L2170
06040 L6040: read #4,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat tcp eof L6370
06050   if heno<>eno then goto L6370
06060   if prd<orgbeg_date or prd>orgend_date then goto L6040 ! not this year
06070   form pos 1,n 8,pos 48,n 2,pos 168,21*pd 5.2,pos 468,pd 3
06080   if tcd<1 or tcd>10 then tcd=1
06090 ! finish with this employee
06100   dedret=0
06110   cafded=0
06120   for j=1 to 20
06130     if newdedfed(j)=1 then goto L6140 else goto L6150
06140 L6140: dedret=dedret+tcp(j+4)
06150 L6150: if dedfica(j)=1 then goto L6160 else goto L6170
06160 L6160: cafded=cafded+tcp(j+4)
06170 L6170: next j
06180   orgw2(1)=min(orgw2(1)+tcp(31)-tcp(30)-cafded,ssmax-tcp(30)) ! TOTAL SOC-SEC WAGES
06190   orgw3=orgw3+tcp(2) ! TOTAL FICA WITHHELD
06200   orgw3(1)=orgw3(1)+tcp(31)-cafded ! TOTAL MEDICARE WAGES & TIPS
06210 ! if env$('client')="Washington Parrish" then orgw3(1)=orgw3(1)+tcp(5) ! add deferred comp match to medicare wages
06220   orgw3(1)=min(mcmax,orgw3(1)) ! MC WAGES CANNOT EXCEED MAXIMUM
06230   if uprc$(med$)="Y" then orgw2=orgw2+tcp(2) else orgw2=round(min(orgw3/(ssrate+mcrate)*ssrate,ssmax*ssrate),2) ! SS WH
06240   if uprc$(med$)="Y" then orgw3(2)=orgw3(2)+tcp(3) else orgw3(2)=orgw3-orgw2 ! MEDICARE WITHHELD
06250   orgw2(2)=orgw2(2)+tcp(30) ! FICA TIPS YTD
06260   orgw2(3)=orgw2(3)+tcp(31)-dedret ! TOTAL FEDERAL WAGES
06270   orgw2(4)=orgw2 ! orgw2(4)+tcp(2) ! FICA W/H YTD       (COULD BE +W2 INSTEAD OF +tcp(2)IF ONLY FICA PORTION GOES INTO W2 RECORD)
06280 ! orgw2(4)=orgW2 ! PUT SS W/H ONLY IN 2-W RECORD (EXCLUDE MEDICARE W/H)
06290   orgw2(5)=orgw2(5)+tcp(1) ! FED W/H YTD
06300   if ins>0 then orgw2(6)=orgw2(6)+tcp(4+ins) ! EMPLOYER COST GROUP LIFE INS.
06310   orgw2(7)=orgw2(7)+0 ! UNCOLLECTED EMPLOYEE FICA TAX ON TIPS
06320   orgw2(8)=orgw2(8)+tcp(24) ! EIC TOTAL
06330   orgw2(9)=orgw2(9)+0 ! ALLOCATED TIPS
06340   if orgdfc>0 then orgdc1=orgdc1+tcp(4+orgdfc)*100 ! DEFERRED COMPENSATION
06350   if orgdcan>0 then orgdca=orgdca+tcp(4+orgdcan)*100 ! DEPENDENT CARE ASSISTANCE
06360   goto L6040
06370 L6370: if em6=9 then orgw2(1)=orgw2(4)=orgw3(1)=orgw3(2)=0 ! NO SS OR MC
06380   if em6=1 then orgw3(1)=orgw3(2)=0 ! NO MEDICARE
06390   if em6=2 then orgw2(1)=orgw2(4)=0 ! NO SOC-SEC
06400   return 
