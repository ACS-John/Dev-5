00010 !  Replace S:\acsGL\glElecW2
00020 ! Create Electronic W-2s
00030 !
00040   library 'S:\Core\Library': fnxit,fntop, fnopenprn,fncloseprn,fnerror,fndat,fnprocess
00050   on error goto Ertn
00060 !
00070   dim em$(3)*30,ss$*11,d(14),m(36),s(9),t(9),z$*8
00080   dim a$(3)*40,b$*12,g$*12,d$(10)*8,tty(10),e$(10)*12,s2(2)
00090   dim fa$(1),fb$(1),fc$(1),fd$(1),l$(10),dedfed(10),w3(2),i2(2),t2(2)
00100   dim cap$*128,message$*40
00110   dim emppin$*17,tlcn$*6,contact$*27,contactph$*15,phoneext$*5,email$*40
00120   dim terminat$*1,first$*15,mid$*15,last$*20
00130   dim w2(9),i1(9),t1(9),ct$*20,st$*2,ibm$*8,namcde$*1,typemp$*1,io1$(18)
00140 !
00150   fntop(program$,cap$="Create Electronic W-2s")
00175   pr newpage
00180   open #1: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,input  !:
        read #1,using 'Form POS 1,3*C 40,C 12,POS 618,50*N 1': mat a$,b$,mat dedcode,mat dedfed,mat dedfica,mat dedst,mat deduc !:
        close #1: 
00190   on fkey 5 goto XIT
00200 !
00210 L210: p1=pos(b$,"-",1)
00220   if p1=0 then goto L260
00230   b$(p1:p1)=""
00240   goto L210
00250 !
00260 L260: b1=val(b$)
00270   p1=pos(a$(3),",",1): comma=1
00280   if p1=0 then p1=pos(a$(3)," ",1): comma=0
00290   ct$=a$(3)(1:p1-1)
00300   if comma=1 then st$=a$(3)(p1+2:p1+3) else st$=a$(3)(p1+1:p1+2)
00310   p2=len(rtrm$(a$(3)))
00320   p1=p2-4
00330   zip$=a$(3)(p1:p2)
00340   if val(date$(1:2))-1 <70 then yr=2000+val(date$(1:2))-1 else yr=1900+val(date$(1:2))-1
00350   if date$(4:5)="12" then yr=yr+1 ! add one to year if run in december
00360   io1$(1)="5,25,C 40,UT,N"
00370   io1$(2)="6,25,C 40,UT,N"
00380   io1$(3)="7,25,C 20,UT,N"
00390   io1$(4)="8,25,C 2,UT,N"
00400   io1$(5)="9,25,C 5,UT,N"
00410   io1$(6)="10,25,N 9,UT,N"
00420   io1$(7)="11,25,N 4,UT,N"
00430   io1$(8)="12,25,N 10.2,UT,N"
00440   io1$(9)="13,25,N 10.4,UT,N"
00450   io1$(10)="14,25,N 10.2,UT,N"
00460   io1$(11)="15,25,N 10.4,UT,N"
00470   io1$(12)="16,75,N 2,UT,N"
00480   io1$(13)="17,47,N 2,UT,N"
00490   io1$(14)="18,61,N 2,UT,N"
00500   io1$(15)="19,65,N 2,UT,N"
00510   io1$(16)="20,35,C 8,UT,N"
00520   io1$(17)="21,52,C 1,UT,N"
00530   io1$(18)="22,38,C 1,UT,N"
00540   ibm$="IBM"
00550   namcde$="F"
00560   typemp$="R"
00570 !
00580 SCR1: ! 
00590   pr newpage
00600   close #101: ioerr L610
00610 L610: open #101: "SROW=2,SCOL=3,EROW=23,ECOL=77,BORDER=DR,CAPTION=<Create Electronic W2 Diskette for I.R.S.",display,outIn 
00620   pr f "3,15,C 51,R,N": "  INSERT DISKETTE FOR ELECTRONIC W2'S IN DRIVE A:"
00630   pr f "5,5,C 60": "Company Name:"
00640   pr f "6,5,C 60": "Street Address:"
00650   pr f "7,5,C 60": "City:"
00660   pr f "8,5,C 60": "State:"
00670   pr f "9,5,C 60": "Zip Code:"
00680   pr f "10,5,C 60": "Federal ID #:"
00690   pr f "11,5,C 60": "Payment Year:"
00700   pr f "12,5,C 60": "Soc-Sec Maximum:"
00710   pr f "13,5,C 60": "Soc-Sec Rate:"
00720   pr f "14,5,C 60": "Medicare Maximum:"
00730   pr f "15,5,C 60": "Medicare Rate:"
00740   pr f "16,5,C 70": "Miscellaneous Deduction Containing Employer Cost Group-Term Life Ins:"
00750   pr f "17,5,C 70": "Miscellaneous Deduction Used For Pension:"
00760   pr f "18,5,C 70": "Miscellaneous Deduction Used For Deferred Compensation:"
00770   pr f "19,5,C 70": "Miscellaneous Deduction Used For Dependent Care Assistance:"
00780   pr f "20,5,C 60": "Computer Manufacturer's Name:"
00790   pr f "21,5,C 60,N": "F=First Name First or S=Surname First on File:"
00800   pr f "22,5,C 60": "Type of Business Code R=Regular:"
00810   pr f "24,28,C 9,B,1": "Next (F1)"
00820   pr f "24,39,C 11,B,5": "Cancel (F5)"
00830   pr f mat io1$: a$(1),a$(2),ct$,st$,zip$,b1,yr,87900,.062,999999,.0145,ins,pen,dfc,dcan,ibm$,namcde$,typemp$
00840 L840: input fields mat io1$,attr "R": a$(1),a$(2),ct$,st$,zip$,b1,yr,ssmax,ssrate,mcmax,mcrate,ins,pen,dfc,dcan,ibm$,namcde$,typemp$ conv CONV1
00850   if ce>0 then io1$(ce)(ce1:ce2)="U": ce=0
00860   if cmdkey>0 then goto L930 else ce=curfld+1
00870   if ce>udim(io1$) then ce=1
00880 L880: io1$(ce)=rtrm$(uprc$(io1$(ce))) : ce1=pos(io1$(ce),"U",1)
00890   ce2=ce1+1 : io1$(ce)(ce1:ce1)="UC" : goto L840
00900 CONV1: if ce>0 then io1$(ce)(ce1:ce2)="U"
00910   ce=cnt+1
00920 ERR1: pr f "24,78,C 1": bell : goto L880
00930 L930: ! 
00940   if cmdkey=5 then goto XIT
00950   if rtrm$(a$(1))="" then ce=1: goto ERR1
00960   if rtrm$(a$(2))="" then ce=2: goto ERR1
00970   if rtrm$(ct$)="" then ce=3: goto ERR1
00980   if rtrm$(st$)="" then ce=4: goto ERR1
00990   if rtrm$(zip$)="" then ce=5: goto ERR1
01000   if b1=0 then ce=6: goto ERR1
01010   if yr<2001 then ce=7: goto ERR1
01020   ficarate=ssrate+mcrate
01030   if ssmax<53400 then ce=8: goto ERR1
01040   if ins<0 or ins>10 then ce=9: goto ERR1
01050   if pen<0 or pen>10 then ce=10: goto ERR1
01060   if dfc<0 or dfc>10 then ce=11: goto ERR1
01070 !
01080   mat io1$(2)
01090   io1$(1)="12,71,N 2,UT,N"
01100   io1$(2)="14,71,N 2,UT,N"
01110   close #101: ioerr L1120
01120 L1120: pr newpage
01130   open #101: "SROW=7,SCOL=2,EROW=15,ECOL=79,BORDER=DR,CAPTION=<Electronic W-2   State Reporting Information",display,outIn 
01140   pr f "8,4,C 72": "Some states require filing W2's on diskette.  Answer the following"
01150   pr f "9,4,C 72": "questions if you wish to create 'RS' records during this run."
01160   pr f "12,8,Cr 62": "State code used in your record to identify the selected state:"
01170   pr f "14,8,Cr 62": "Appropriate FIPS postal numeric code:"
01180   pr f "16,28,C 9,B,1": "Next (F1)"
01190   pr f "16,39,C 11,B,5": "Cancel (F5)"
01200 L1200: input fields mat io1$: sr1,sr2 conv L1200
01210   if ce>0 then io1$(ce)(ce1:ce2)="U": ce=0
01220   if cmdkey>0 then goto L1290 else ce=curfld+1
01230   if ce>udim(io1$) then ce=1
01240 L1240: io1$(ce)=rtrm$(uprc$(io1$(ce))) : ce1=pos(io1$(ce),"U",1)
01250   ce2=ce1+1 : io1$(ce)(ce1:ce1)="UC" : goto L1200
01260 CONV2: if ce>0 then io1$(ce)(ce1:ce2)="U"
01270   ce=cnt+1
01280 ERR2: pr f "24,78,C 1": bell : goto L1240
01290 L1290: if cmdkey=5 then goto XIT
01300   if sr1<0 or sr1>udim(e$) then ce=1: goto ERR2
01310   if sr1>0 and sr2=0 then ce=2: goto ERR2
01320 !
01330   gosub SCR2
01340   pr newpage
01350   win=101
01360   message$=""
01370   stopable=1: gosub L3970 ! fnWAIT(MESSAGE$,1)
01380 !
01390   open #1: "Name=[Q]\GLmstr\PRmstr.h[cno],KFName=[Q]\GLmstr\PRINDEX.h[cno],Shr",internal,input,keyed 
01400 L1400: open #22: "Name=W2REPORT,RecL=512,eol=crlf,replace",display,output 
01410   goto L1480
01420 !
01430   pr newpage
01440   msgline$(1)="Insert Diskette"
01450   mtype=1
01460   if err=4221 then gosub L3610 ! fnOldMsgBox(MAT RESPONSE$,CAP$,MAT MSGLINE$,MTYPE)
01470   goto L1400
01480 L1480: gosub RECRA
01490 ! Gosub RECRE
01500 L1500: ! pr f "12,32,N 3,UT,N": R1/LREC(1)*100
01510   pr f "12,32,N 3,N": r1/lrec(1)*100
01520   read #1,using L1610: eno,mat em$,ss$,mat m eof END1
01530   gosub L3030
01540   dedfed=dedfica=dedst=0
01550   for j=1 to 10
01560     if dedcode(j)><1 then goto L1600
01570     if dedfed(j)=1 then dedfed=dedfed+m(j*2+9)
01580     if dedfica(j)=1 then dedfica=dedfica+m(j*2+9)
01590     if dedst(j)=1 then dedst=dedst+m(j*2+9)
01600 L1600: next j
01610 L1610: form pos 1,n 4,3*c 25,c 11,36*pd 5.2
01620   p1=pos(em$(3),",",1) : comma=1
01630   if p1=0 then p1=pos(em$(3)," ",1): comma=0
01640   emct$=em$(3)(1:p1-1)
01650   if comma=1 then emst$=em$(3)(p1+2:p1+3) else emst$=em$(3)(p1+1:p1+2)
01660   emst$=em$(3)(p1+2:p1+3)
01670   p2=len(rtrm$(em$(3)))
01680   p1=p2-4
01690   emzip$=em$(3)(p1:p2)
01700 L1700: p1=pos(ss$,"-",1)
01710   if p1>0 then ss$(p1:p1)="": goto L1700 else ssn=val(ss$)
01720   w2(1)=min(w2(1)+m(1)-m(31)-dedfica,ssmax-m(31)) ! TOTAL SOC-SEC WAGES
01730   w3=w3+m(5) ! TOTAL FICA WITHHELD
01740   w3(1)=w3(1)+m(1)-dedfica ! TOTAL MEDICARE WAGES & TIPS
01750   w3(1)=min(mcmax,w3(1)) ! MC wages cannot exceen maximum
01760   w2=round(min(w3/(ssrate+mcrate)*ssrate,ssmax*ssrate),2) ! SS WH
01770   w3(2)=w3-w2 ! Medicare withheld
01780   w2(2)=w2(2)+m(31) ! FICA tips YTD
01790   w2(3)=w2(3)+m(1)-dedfed ! TOTAL FEDERAL WAGES
01800   w2(4)=w2(4)+w2 ! FICA W/H YTD
01810 ! w2(4)=W2 ! SS WH only in W-2 record ( EXCLUDE MEDICARE W/H)
01820   w2(5)=w2(5)+m(3) ! FED W/H YTD
01830   if ins>0 then w2(6)=w2(6)+m(9+(ins*2)) ! EMPLOYER COST GROUP LIFE INS
01840   w2(7)=w2(7)+0 ! uncollected employee fica tax on tips
01850   w2(8)=w2(8)+m(35) ! EIC TOTAL
01860   w2(9)=w2(9)+0 ! ALLOCATED TIPS
01870   if dfc>0 then dc1=dc1+m(9+(dfc*2))*100 ! DEFERRED COMPENSATION
01880   if dcan>0 then dca=dca+m(9+(dcan*2))*100 ! Dependent care assistance
01890   if sr1=0 then goto L1920
01900   s2(1)=s2(1)+(m(1)*100)
01910   s2(2)=s2(2)+(m(7)*100)
01920 L1920: if em6=9 then w2(1)=w2(4)=w3(1)=w3(2)=0
01930   gosub RECRE
01940   gosub RECRW
01950   gosub RECRS
01960   tw1=tw1+1
01970   tw2=tw2+1
01980   gosub RECRT
01990   tw2=0
02000   goto L1500
02010 !
02020 RECRA: pr #22,using L2030: "RA",rpad$(ltrm$(str$(b1)),9),emppin$,resub$,tlcn$,"98",a$(1),"",a$(2)(1:22),ct$,st$,zip$,"","","","","",a$(1),"",a$(2)(1:22),ct$,st$,zip$,"","","","","",contact$,contactph$,phoneext$,"",email$,"","","2","L",""
02030 L2030: form pos 1,c 2,pic(#########),c 17,c 1,c 6,c 2,c 57,c 22,c 22,c 22,c 2,c 5,c 4,c 5,c 23,c 15,c 2,c 57,c 22,c 22,c 22,c 2,c 5,c 4,c 5,c 23,c 15,c 2,c 27,c 15,c 5,c 3,c 40,c 3,c 10,c 1,c 1,c 12
02040   return 
02050 !
02060 RECRE: pr #22,using L2070: "RE",yr,"",rpad$(ltrm$(str$(b1)),9),"",terminat$,"","",a$(1),"",a$(2)(1:22),ct$,st$,zip$,"","","","","","R","",0,""
02070 L2070: form pos 1,c 2,pic(####),c 1,pic(#########),c 9,c 1,c 4,c 9,c 57,c 22,c 22,c 22,c 2,c 5,c 4,c 5,c 23,c 15,c 2,c 1,c 1,n 1,c 291
02080   return 
02090 !
02100   form pos 1,c 2,pic(#########),c 15,c 15,c 20,c 4,c 22,c 22,c 22,c 2,c 5,c 4,c 5,c 23,c 15,c 2,18*pic(###########),c 22,2*pic(###########),c 56,n 1,c 1,c 1,n 1,c 23,
02110   pr #22,using L2120: "2E",ct$,st$,"",zip$,namcde$,typemp$,"","","",""
02120 L2120: form pos 1,c 2,g 25,g 10,2*g 5,2*g 1,g 2,g 4,g 2,c 71
02130   return 
02140 !
02150 RECRW: for j=1 to 9: w2(j)=w2(j)*100: next j
02160   for j=1 to 2: w3(j)=w3(j)*100 : next j
02170   if pen=0 then pen$="0" else pen$="1"
02180   if dfc=0 then dfc$="" else dfc$="D"
02190   pr #22,using L2200: "RW",ssn,first$,mid$,last$,"","",em$(2)(1:22),emct$,emst$,emzip$,"","","","","",w2(3),w2(5),w2(1),w2(4),w3(1),w3(2),w2(2),w2(8),dca,dc1,0,0,0,0,0,0,0,0,0,"",w2(6),0,0,0,0,"",0,"",pen$,0,""
02200 L2200: form pos 1,c 2,pic(#########),c 15,c 15,c 20,c 4,c 22,c 22,c 22,c 2,c 5,c 4,c 5,c 23,c 15,c 2,19*pic(###########),c 11,5*pic(###########),c 23,pic(#),c 1,c 1,pic(#),c 23
02210 ! pr #22,Using 2270: "RO","",W2(9),W2(7),0,0,0,0,0,"","","",0,0,0,0,0,0,0,"",0,0,""
02220   form pos 1,c 2,c 9,7*pic(###########),c 176,c 1,c 9,7*pic(###########),c 11,2*pic(###########),c 128
02230   return 
02240 !
02250 RECRS: ! STATE RECORD
02260   if sr1=0 then goto L2300 ! NO STATE SELECTED
02270   if s2(1)=0 and s2(2)=0 then goto L2300 ! NO STATE WAGES
02280   pr #22,using L2290: "RS",sr2,"",ssn,first$,mid$,last$,"","",em$(2)(1:22),emct$,emst$,emzip$,"","","","","","","",0,0,0,0,0,"","","",sr2,s2(1),s2(2),"","",0,0,"","","",""
02290 L2290: form pos 1,c 2,g 2,c 5,pic(#########),c 15,c 15,c 20,c 4,c 22,c 22,c 22,c 2,c 5,c 4,c 5,c 23,c 15,c 2,c 2,c 6,2*pic(###########),pic(##),2*pic(########),c 5,c 20,c 6,g 2,2*pic(###########),c 10,c 1,2*pic(###########),c 7,c 75,c 75,c 25
02300 L2300: t1=t1+1: mat t1=t1+w2
02310   mat i1=i1+w2
02320   mat i2=i2+w3
02330   mat t2=t2+w3
02340   dc2=dc2+dc1
02350   dc3=dc3+dc1
02360   dca2=dca2+dca
02370   dca3=dca3+dca
02380   w2=w3=dca=dc1=0
02390   mat w2=(0)
02400   mat w3=(0)
02410   mat s2=(0)
02420   return 
02430 !
02440 RECRT: ! 
02450   pr #22,using L2460: "RT",tw2,t1(3),t1(5),t1(1),t1(4),t2(1),t2(2),t1(2),t1(8),dca3,dc3,0,0,0,0,0,0,0,0,0,"",t1(6),0,0,0,0,0,""
02460 L2460: form pos 1,c 2,pic(#######),19*pic(###############),c 15,6*pic(###############),c 113
02470 ! pr #22,Using 2520: "RU",TW2,T1(9),T1(7),0,0,0,0,0,"",0,0,0,0,0,0,0,0,0,""
02480   form pos 1,c 2,pic(#######),7*pic(###############),c 240,9*pic(###############),c 23
02490   t1=0: mat t1=(0)
02500   mat t2=(0)
02510   return 
02520 !
02530 RECRF: pr #22,using L2540: "RF"," ",tw1,""
02540 L2540: form pos 1,c 2,c 5,pic(#########),c 496
02550   return 
02560 !
02570 END1: ! 
02580 ! Gosub RECRT
02590   gosub RECRF
02600   gosub L2630
02610 XIT: fnxit
02620 !
02630 L2630: close #24: ioerr L2650
02640   dim a$*512
02650 L2650: close #22: ioerr L2660
02660 L2660: open #24: "Name=X,RecL=513,EOL=NONE,REPLACE",external,output 
02670   open #22: "Name=w2report,RecL=512",display,input 
02680 L2680: linput #22: a$ eof L2730
02690   if a$(512:512)="X" then a$(512:512)=""
02700   write #24,using L2710: rpad$(a$,512),chr$(10)
02710 L2710: form pos 1,c 512,c 1
02720   goto L2680
02730 L2730: close #24: 
02740   close #22: 
02750   execute "COPY x f:w2report"
02760   return 
02770 !
02780 SCR2: ! 
02790   dim contact$*27,email$*40
02800   win=101
02810   win_height=12: win_width=75: display_cnam=1: button_option=2: gosub L3200
02820   pr #win,fields "04,2,Cr 31,N": "Personal ID Number:" !:
        pr #win,fields "05,2,Cr 31,N": "Resub Indicator:" !:
        pr #win,fields "06,2,Cr 31,N": "Resub TLCN:" !:
        pr #win,fields "07,2,Cr 31,N": "Contact Name:" !:
        pr #win,fields "08,2,Cr 31,N": "Contact Phone Number:" !:
        pr #win,fields "09,2,Cr 31,N": "Contact Phone Extension:" !:
        pr #win,fields "10,2,Cr 31,N": "Contact E-Mail:" !:
        pr #win,fields "11,2,Cr 31,N": "Terminating Business Indicator:"
02830   scr2_io$(1)="04,34,C 17,UT,N" !:
        scr2_io$(2)="05,34,C 01,UT,N" !:
        scr2_io$(3)="06,34,C 06,UT,N" !:
        scr2_io$(4)="07,34,C 27,UT,N" !:
        scr2_io$(5)="08,34,C 15,UT,N" !:
        scr2_io$(6)="09,34,C 05,UT,N" !:
        scr2_io$(7)="10,34,C 40,UT,N" !:
        scr2_io$(8)="11,34,C 01,UT,N"
02840   if resub$="" then resub$="0"
02850 ! If TLCN$="" Then tLCN$="0"
02860   if terminat$="" then terminat$="0"
02870 L2870: rinput #win,fields mat scr2_io$: emppin$,resub$,tlcn$,contact$,contactph$,phoneext$,email$,terminat$ conv CONV_SCR2
02880   if ce>0 then scr2_io$(ce)(ce1:ce2)="U": ce=0
02890   if cmdkey>0 then goto L2960 else ce=curfld
02900 L2900: ce=ce+1: if ce>udim(scr2_io$) then ce=1
02910 L2910: scr2_io$(ce)=rtrm$(scr2_io$(ce)) : ce1=pos(scr2_io$(ce),"U",1) : if ce1=0 then goto L2900
02920   ce2=ce1+1 : scr2_io$(ce)(ce1:ce1)="UC" : goto L2870
02930 CONV_SCR2: if ce>0 then scr2_io$(ce)(ce1:ce2)="U"
02940   ce=cnt+1
02950 ERR_SCR2: pr f "24,78,C 1": bell : goto L2910
02960 L2960: if resub$<>"0" and resub$<>"1" then ce=2 !:
          goto ERR_SCR2
02970   if resub$="1" and rtrm$(tlcn$)="" then ce=3 !:
          goto ERR_SCR2
02980   if terminat$<>"0" and terminat$<>"1" then ce=8 !:
          goto ERR_SCR2
02990   close #win: 
03000   if cmdkey=5 then goto SCR1
03010   return 
03020 !
03030 L3030: dim first$*15,mid$*15,last$*20,em$(3)*30
03040   em$(1)=uprc$(rtrm$(em$(1))): ! nAMCDE$="s"
03050   x1=pos(em$(1)," ",1)
03060   x2=pos(em$(1)," ",x1+1)
03070   x3=pos(em$(1)," ",x2+1)
03080   if uprc$(namcde$)="S" then goto L3130
03090   first$=em$(1)(1:max(x1-1,1))
03100   if x2>0 then mid$=em$(1)(x1+1:x2-1): last$=em$(1)(x2+1:len(em$(1)))
03110   if x2=0 then last$=em$(1)(x1+1:len(em$(1))): mid$=""
03120   goto L3170
03130 L3130: ! last name first
03140   if x1>0 and em$(1)(x1-1:x1-1)="," then last$=em$(1)(1:x1-2) else last$=em$(1)(1:max(x1-1,1))
03150   if x2>0 then first$=em$(1)(x1+1:x2-1): mid$=em$(1)(x2+1:len(em$(1)))
03160   if x2=0 then first$=em$(1)(x1+1:len(em$(1))): mid$=""
03170 L3170: ! pr FIRST$,MID$,LAST$
03180   return 
03190 !
03200 L3200: ! 
03270   if exists("C:\ACS\Local\Settings\No_Print_Newpage.txt") then goto L3280 else pr newpage
03280 L3280: screen_width=80
03290   screen_height=24
03300   if display_cnam=0 then goto L3350
03350 L3350: sc=max(int(((screen_width-win_width)/2)+1),2)
03360   ec=min(sc+win_width-1,79)
03370   sr=max(int(((screen_height-win_height)/2)+1),2)
03380   er=min(sr+win_height-1,23)
03390 !     pr "win_height="&STR$(WIN_HEIGHT),"win_width="&STR$(WIN_WIDTH)
03400 !     pr "sr="&STR$(SR),"sc="&STR$(SC)
03410 !     pr "er="&STR$(ER),"ec="&STR$(EC) : Pause
03420   close #win: ioerr L3430
03430 L3430: open #win: "SRow="&str$(sr)&",SCol="&str$(sc)&",ERow="&str$(er)&",ECol="&str$(ec)&",Border=Sr,Caption=<"&cap$,display,outIn 
03440   pr #win: newpage
03450   if display_cnam=0 then goto L3480
03460   if display_cnam=1 then !:
          pr #win,fields "1,1,Cc "&str$(win_width)&",R,N": env$('cnam')(1:min(40,win_width)) !:
          pr #win,fields "2,1,Cc "&str$(win_width)&",R,N": "Company Number [cno]"(1:min(40,win_width))
03470   if display_cnam=2 then !:
          pr #win,fields "1,1,Cc "&str$(win_width)&",R,N": "Company Number [cno]"(1:min(40,win_width))
03480 L3480: if button_option=0 then goto L3590
03490   mat fkey$=("") : em$="" : es=0
03500   fkey$(5)="Cancel" ! included by default
03510   if button_option=2 then !:
          fkey$(1)="Next"
03520   if button_option=3 then !:
          fkey$(1)="Print"
03530   if button_option=4 then !:
          fkey$(1)="Save"
03540   if button_option=5 then !:
          fkey$(1)="Next" !:
          fkey$(6)="Search"
03550   if button_option=6 then !:
          fkey$(1)="Next" !:
          fkey$(2)="Back"
03560   if button_option=7 then !:
          fkey$(1)="Save" !:
          fkey$(4)="Delete"
03570   scrline=er+1: gosub L4320 !  fnFKEY(ER+1,MAT FKEY$,MAT DISFK,EM$,ES)
03580 ! 
03590 L3590: return  ! Fnend
03600 !
03610 L3610: ! Def Library fnOldMsgBox(mat RESPONSE$,&CAP$,mat MSGLINE$,MTYPE)
03620 ! mtype=0 means splash    - returns no response                                 ! mostly for "please wait..." and "printing..."                                 ! (anywhere no response is required - no buttons are displyed either)
03630 ! mtype=1 means OK only   - returns no response
03640 ! mtype=2 means Yes or No - returns "Y" or "N"
03650 ! mtype=3 means Yes, No, Cancel - returns "Y" or "N" or ""
03660 ! response$(1)= code you're looking for 2-5 are reserved for future use
03670   close #104: ioerr L3680
03680 L3680: endrow=12
03690   for j=2 to udim(msgline$)
03700     if msgline$(j)<>"" then endrow=endrow+1
03710   next j
03720   open #104: "SRow=10,SCol=09,ERow="&str$(endrow)&",ECol=70,Border=SR,Caption=<"&cap$,display,outIn 
03730   pr #104: newpage
03740   mglinerow=2
03750   for j=1 to udim(msgline$)
03760     pr #104,fields str$(mglinerow+j-1)&",2,Cc 60,N": msgline$(j)
03770   next j
03780   if mtype=1 then pr f str$(endrow+1)&",38,Cc 4,B,1": "Ok"
03790   if mtype=1 then input fields str$(endrow)&",09,C 1,AE,N": pause$
03800   if mtype=2 then pr f str$(endrow+1)&",35,Cc 4,B,21": "Yes"
03810   if mtype=2 then pr f str$(endrow+1)&",40,Cc 4,B,22": "No"
03820 L3820: if mtype=2 then input fields str$(endrow)&",09,Cu 1,AE,N": response$(1)
03830   if mtype=2 and cmdkey=22 then response$(1)="N"
03840   if mtype=2 and cmdkey=21 then response$(1)="Y"
03850   if mtype=2 and response$(1)<>"Y" and response$(1)<>"N" then pr f "24,1,C 7,N": bell$ : goto L3820
03860   if mtype=3 then pr f str$(endrow+1)&",29,Cc 4,B,21": "Yes"
03870   if mtype=3 then pr f str$(endrow+1)&",34,Cc 4,B,22": "No"
03880   if mtype=3 then pr f str$(endrow+1)&",39,C 12,B,22": "Cancel (Esc)"
03890   if mtype=3 then input fields str$(endrow)&",09,Cu 1,AE,N": response$(1)
03900   if mtype=3 and cmdkey=22 then response$(1)="N"
03910   if mtype=3 and cmdkey=21 then response$(1)="Y"
03920   if mtype=3 and cmdkey=99 then response$(1)=""
03930   if mtype=3 and response$(1)<>"Y" and response$(1)<>"N" and response$(1)<>"" then pr f "24,1,C 7,N": bell$ : goto L3820
03940   close #104: ioerr L3950
03950 L3950: return  ! Fnend
03960 !
03970 L3970: ! Def Library FNWAIT(&MESSAGE$,STOPABLE)
03980 ! if stopable=1 will display "Cancel (F5)" button
03990 ! win = window number
04040   close #win: ioerr ignore
04050   open #win: "Srow=10,SCol=20,ERow=14,ECol=59,Border=Sr,Caption=<"&cap$,display,outIn 
04060   pr #win: newpage
04070   pr #win,fields "1,1,Cc 40,R,N": env$('cnam')
04080   pr #win,fields "2,1,Cc 40,R,N": "Company Number [cno]"
04090   pr #win,fields "4,1,Cc 40,N": message$
04100   if rtrm$(message$)="" then pr #win,fields "4,1,Cc 40,N": "Please wait..."
04110   if stopable=0 then pr f "15,34,C 11,R,N": "Do Not Stop"
04120   if stopable=1 then pr f "15,34,C 11,B,5": "Cancel (F5)"
04130   return  ! Fnend
04140 ! Def Library FNOPENWIN(WIN,SR,SC,ER,EC,&CAP$)
04190 L4190: if sr<1 then sr=10
04200   if sc<1 then sc=20
04210   if er<1 then er=14
04220   if ec<1 then ec=59
04230   win_width=ec-sc+1
04240   close #win: ioerr L4250
04250 L4250: open #win: "SRow="&str$(sr)&",SCol="&str$(sc)&",ERow="&str$(er)&",ECol="&str$(ec)&",Border=Sr,Caption=<"&cap$,display,outIn 
04260   pr #win: newpage
04270   pr #win,fields "1,1,Cc "&str$(win_width)&",R,N": env$('cnam')(1:min(40,win_width))
04280   pr #win,fields "2,1,Cc "&str$(win_width)&",R,N": "Company Number [cno]"(1:min(40,win_width))
04290 ! 
04300 !
04310   return  ! Fnend
04320 L4320: ! Def Library FNFKEY(SCRLINE,MAT FKEY$,MAT DISFK,&EM$,ES)
04330   totallen=0 !:
        startpos=0
04340   for j=1 to udim(fkey$) ! add ' (Fx)' to each button
04350     if fkey$(j)="" then goto L4380
04360     fkey$(j)=fkey$(j)&" (F"&str$(j)&")" !:
          ! add ' (Fx)' to each button
04370     totallen=totallen+len(fkey$(j))+1
04380 L4380: next j
04390   totallen=totallen+len(rtrm$(em$))+min(len(rtrm$(em$)),1)+es
04400   totallen=totallen-1
04410   startpos=int((80-totallen)/2)+1
04420   pr f str$(scrline)&","&str$(startpos)&",C "&str$(totallen)&",N": rpt$("Ä",totallen)
04430   for j=1 to udim(fkey$)
04440     if fkey$(j)="" then goto L4490
04450     if disfk(j)=1 then pr f str$(scrline)&","&str$(startpos)&",C "&str$(len(fkey$(j)))&",R,"&str$(j): fkey$(j)
04460     if disfk(j)=1 then goto L4480
04470     pr f str$(scrline)&","&str$(startpos)&",C "&str$(len(fkey$(j)))&",B,"&str$(j): fkey$(j)
04480 L4480: startpos=startpos+len(fkey$(j))+1
04490 L4490: next j
04500   if rtrm$(em$)="" then goto L4520
04510   pr f str$(scrline)&","&str$(startpos)&",C "&str$(len(rtrm$(em$))+es)&",R,N": rtrm$(em$)
04520 L4520: return  ! Fnend
04530 !
04540 ! <Updateable Region: ERTN>
04550 ERTN: fnerror(program$,err,line,act$,"xit")
04560   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
04570   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
04580   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
04590 ERTN_EXEC_ACT: execute act$ : goto ERTN
04600 ! /region
04610 !
