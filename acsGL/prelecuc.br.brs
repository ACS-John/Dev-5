00010 ! REPLACE S:\acsGL\prElecuc
00020 ! never could get it work with an RO record if all zeroes; without !:
        ! RO record i had to put an RE record in front of every RW record and !:
        ! had to follow with an RT record - Right now this program will not !:
        ! create an RO record
00030 ! ______________________________________________________________________
00032   library 'S:\Core\Library': fntop,fnxit,fnerror,fndate_mmddyy_to_ccyymmdd
00034   fntop(program$,cap$="PR ELEC UC")
00040   on error goto ERTN
00050 ! ______________________________________________________________________
00060   dim em$(3)*30,ss$*11,d(14),ty(21),tqm(17),s(9),t(9),z$*8,cap$*128,message$*40
00070   dim a$(3)*40,b$*12,g$*12,d$(10)*8,e$(10)*12,s2(2)
00080   dim k(1),k$(3)*25,l$(1)*11,d(14),m(36),n(2)
00090   dim fa$(1),fb$(1),fc$(1),fd$(1),l$(10),dedfed(10),w3(2),i2(2),t2(2)
00100   dim emppin$*17,tlcn$*6,contact$*27,contactph$*15,phoneext$*5,email$*40
00110   dim w2(9),i1(9),t1(9),ct$*20,st$*2,ibm$*8,namcde$*1,typemp$*1,io1$(15)
00120   dim terminat$*1,first$*15,mid$*15,last$*20,m(10),r(10),e$(10)*12
00130 ! ______________________________________________________________________
00260   on fkey 5 goto XIT
00270   open #1: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,input 
00290   read #1,using FORM_COMPANY: mat a$,b$,mat d$,loccode,mat e$,mat dedfed,oldmax,mat m,mat r,mat e$,mat dedcode
00292 FORM_COMPANY: form pos 1,3*c 40,c 12,pos 150,10*c 8,n 2,pos 317,10*c 12,pos 638,10*n 1,pos 239,pd 4.2,pos 247,10*pd 4.2,10*pd 3.3,10*c 12,pos 618,10*n 1
00300 ! read #1,using FORM_PRCOINFO: mat a$,b$,c$,oldmax,mat dedfed
00320 ! FORM_PRCOINFO: form pos 1,3*c 40,c 12,x 12,c 5,pos 188,pd 7.2,pos 658,10*n 1
00330   close #1: 
00340 L340: p1=pos(b$,"-",1)
00350   if p1=0 then goto L380
00360   b$(p1:p1)=""
00370   goto L340
00380 L380: b1=val(b$)
00390   p1=pos(a$(3),",",1): comma=1
00400   if p1=0 then p1=pos(a$(3)," ",1): comma=0
00410   ct$=a$(3)(1:p1-1)
00420   if comma=1 then st$=a$(3)(p1+2:p1+3) else st$=a$(3)(p1+1:p1+2)
00430   if uprc$(a$(3)(p1+2:p1+4))="TEX" then st$="Tx"
00440   p2=len(rtrm$(a$(3)))
00450   p1=p2-4
00460   zip$=a$(3)(p1:p2)
00470   io1$(1)="5,25,C 40,UT,N"
00480   io1$(2)="6,25,C 40,UT,N"
00490   io1$(3)="7,25,C 20,UT,N"
00500   io1$(4)="8,25,c 2,UT,N"
00510   io1$(5)="9,25,C 5,UT,N"
00520   io1$(6)="10,25,N 9,UT,N"
00530   io1$(7)="11,25,N 6,UT,N"
00540   io1$(8)="12,52,C 1,UT,N"
00550   io1$(9)="13,38,C 1,UT,N"
00560   io1$(10)="14,68,n 2,UT,N"
00570   io1$(11)="15,43,n 2,UT,N"
00580   io1$(12)="17,25,c 3,UT,N"
00590   io1$(13)="18,25,c 6,UT,N"
00600   io1$(14)="19,57,c 1,UT,N"
00610   io1$(15)="20,49,c 1,UT,N"
00620   namcde$="F"
00630   typemp$="R"
00640 ! ______________________________________________________________________
00650 SCR1: ! 
00660   pr newpage
00670   close #101: ioerr L680
00680 L680: open #101: "SROW=2,SCOL=3,EROW=23,ECOL=77,BORDER=DR,CAPTION=<Create Electronic U/C Diskette for state.",display,outIn 
00690   pr f "3,15,C 51,R,N": "  Insert diskette for elecronic U/C in drive A:"
00700   pr f "5,5,C 60": "Company Name:"
00710   pr f "6,5,C 60": "Street Address:"
00720   pr f "7,5,C 60": "City:"
00730   pr f "8,5,C 60": "State:"
00740   pr f "9,5,C 60": "Zip Code:"
00750   pr f "10,5,C 60": "Federal ID #:"
00760   pr f "11,5,C 60": "Quarter Ending Date:"
00770   pr f "12,5,C 60,N": "F=First Name First or S=Surname First on File:"
00780   pr f "13,5,C 60": "Type of Business Code R=Regular:"
00790   pr f "14,5,C 65": "State code used in your record to identify the selected state:"
00800   pr f "15,5,C 60": "Appropriate FIPS postal numeric code:"
00810   pr f "16,5,C 70": "(See an appendix in your electronic booklet for the postal code!)"
00820   pr f "17,5,C 60": "Country Code:"
00830   pr f "18,5,C 60": "NAICS Code:"
00840   pr f "19,5,C 60": "Deduct Cafiteria Plans for Calculating Wages (Y/N)?"
00850   pr f "20,5,C 60": "Deduct Pension for Calculating Wages (Y/N)?"
00860   pr f "22,28,C 9,B,1": "Next (F1)"
00870   pr f "22,39,C 11,B,5": "Cancel (F5)"
00880   if b1>999999999 then b1=0
00890   pr f mat io1$: a$(1),a$(2),ct$,st$,zip$,b1,yr,namcde$,typemp$,1,48
00900 L900: input fields mat io1$,attr "R": a$(1),a$(2),ct$,st$,zip$,b1,endingdate,namcde$,typemp$,sr1,sr2,country$,naics$,cafiteria$,pension$ conv CONV1
00910   if ce>0 then io1$(ce)(ce1:ce2)="U": ce=0
00920   if cmdkey>0 then goto L990 else ce=curfld+1
00930   if ce>udim(io1$) then ce=1
00940 L940: io1$(ce)=rtrm$(uprc$(io1$(ce))) : ce1=pos(io1$(ce),"U",1)
00950   ce2=ce1+1 : io1$(ce)(ce1:ce1)="UC" : goto L900
00960 CONV1: if ce>0 then io1$(ce)(ce1:ce2)="U"
00970   ce=cnt+1
00980 ERR1: pr f "24,78,C 1": bell : goto L940
00990 L990: ! 
01000   if cmdkey=5 then goto XIT
01010   if rtrm$(a$(1))="" then ce=1: goto ERR1
01020   if rtrm$(a$(2))="" then ce=2: goto ERR1
01030   if rtrm$(ct$)="" then ce=3: goto ERR1
01040   if rtrm$(st$)="" then ce=4: goto ERR1
01050   if rtrm$(zip$)="" then ce=5: goto ERR1
01060   if b1=0 then ce=6: goto ERR1
01070   if endingdate<010100 or endingdate>123199 then ce=7: goto ERR1
01080   if sr1<0 or sr1>10 then ce=10: goto ERR1
01090   if sr2<0 or sr1>99 then ce=11: goto ERR1
01100   cafiteria$=uprc$(cafiteria$): pension$=uprc$(pension$)
01110   if cafiteria$="Y" or cafiteria$="N" then goto L1120 else ce=14: goto ERR1
01120 L1120: if pension$="Y" or pension$="N" then goto L1130 else ce=15: goto ERR1
01130 L1130: monthyr$=cnvrt$("pic(######)",endingdate)(1:2)&"20"&cnvrt$("pic(######)",endingdate)(5:6)
01140   yr=endingdate-(int(endingdate/100)*100)+2000
01150 ! ______________________________________________________________________
01160   gosub SCR2
01170   pr newpage
01180   win=101
01190   message$=""
01200   stopable=1: gosub L3390 ! fnWAIT(MESSAGE$,1)
01210 ! ______________________________________________________________________
01220   open #1: "Name=[Q]\GLmstr\PRmstr.h[cno],KFName=[Q]\GLmstr\PRIndex.h[cno],Shr",internal,input,keyed 
01230 L1230: open #22: "Name=[Q]\UCReport,RecL=512,eol=crlf,replace",display,output 
01240 ! ______________________________________________________________________
01250   goto BEGINNING_OF_FILE
01260   pr newpage
01270   msgline$(1)="Insert Diskette"
01280   mtype=1
01290   if err=4221 then gosub L3040
01300   goto L1230
01310 ! ______________________________________________________________________
01320 BEGINNING_OF_FILE: gosub RECRA : gosub RECRE
01325   open #255: "Name=PRN:/SELECT,PAGEOFLOW=58,RecL=220",display,output 
01330   pr #255,using "form SKIP 2,pos 20,cc 40,skip 1,pos 20,cc 40": "Electronic Edit List",cnvrt$("pic(zz/zz/zzzz",endingdate)
01340 ! READ_EMPLOYEE: Read #1,Using 1370: ENO,MAT EM$,SS$,EM6,EM16,TA Eof END1
01350 READ_EMPLOYEE: read #1,using L1360: eno,mat em$,ss$,mat m eof END1
01360 L1360: form pos 1,n 4,3*c 25,c 11,36*pd 5.2,2*n 5
01370   m1=m2=m3=m4=0
01380   gosub NAME_BREAKDOWN
01390 ! Form POS 1,N 8,3*C 30,C 11,POS 122,N 2,POS 156,N 6,POS 173,PD 3
01400   r1=r1+1
01410   p1=pos(em$(3),",",1) : comma=1
01420   if p1=0 then p1=pos(em$(3)," ",1): comma=0
01430   emct$=em$(3)(1:p1-1)
01440   gosub STATE_BREAKDOWN: emst$=holdst$ ! If COMMA=1 Then eMST$=EM$(3)(P1+2:P1+3) Else eMST$=EM$(3)(P1+1:P1+2)
01450   p2=len(rtrm$(em$(3)))
01460   p1=p2-4
01470   emzip$=em$(3)(p1:p2)
01480 L1480: p1=pos(ss$,"-",1)
01490   if p1>0 then ss$(p1:p1)="": goto L1480 else ssn=val(ss$)
01500 ! READ_DEPARTMENT: Read #2,Using 1500,Rec=TA: TENO,TCD,MAT TY,MAT TQM,TA
01510 ! If SS$="459499366" Then Pause
01520 ! Form POS 1,N 8,POS 48,N 2,POS 168,38*PD 5.2,POS 468,PD 3
01530 ! If TCD<1 OR TCD>10 Then tCD=1
01540   gosub CALCULATEUC ! determine wages for quarter
01550 ! If TA>0 Then Goto 1480
01560 ! Gosub RECRE
01570   gosub RECRS
01580   tw1=tw1+1 ! counter
01590   goto READ_EMPLOYEE
01600 ! ______________________________________________________________________
01610 RECRA: pr #22,using L1620: "RA",b1,"","98",a$(1),"",a$(2)(1:22),ct$,st$,zip$,"","","","",country$(1:2),a$(1),"",a$(2)(1:22),ct$,st$,zip$,"","","","",country$(1:2),contact$,contactph$,phoneext$,"",email$,"","",""
01620 L1620: form pos 1,c 2,pic(#########),c 24,c 2,c 57,c 22,c 22,c 22,c 2,c 5,c 4,c 5,c 23,c 15,c 2,c 57,c 22,c 22,c 22,c 2,c 5,c 4,c 5,c 23,c 15,c 2,c 27,c 15,c 5,c 3,c 40,c 3,c 10,c 14
01630   return 
01640 ! ______________________________________________________________________
01650 RECRE: pr #22,using L1660: "RE",yr,"",b1,"","0","","",a$(1),"",a$(2)(1:22),ct$,st$,zip$,"","",e$(sr1)(1:9),monthyr$,"",r(sr1)*.01,"",naics$,""
01660 L1660: form pos 1,c 2,pic(####),c 1,pic(#########),c 9,c 1,c 4,c 9,c 57,c 22,c 22,c 22,c 2,c 5,c 4,c 126,c 9,c 6,c 1,n 5.4,c 1,c 6,c 185
01670   return 
01680 ! ______________________________________________________________________
01690   form pos 1,c 2,pic(#########),c 15,c 15,c 20,c 4,c 22,c 22,c 22,c 2,c 5,c 4,c 5,c 23,c 15,c 2,18*pic(###########),c 22,2*pic(###########),c 56,n 1,c 1,c 1,n 1,c 23,
01700   pr #22,using L1710: "2E",ct$,st$,"",zip$,namcde$,typemp$,"","","",""
01710 L1710: form pos 1,c 2,g 25,g 10,2*g 5,2*g 1,g 2,g 4,g 2,c 71
01720   return 
01730 ! ______________________________________________________________________
01740 RECRS: ! STATE RECORD
01750   if sr1=0 then goto L1830 ! NO STATE SELECTED
01760   if m1=0 then goto L1830 ! NO quarterly wages
01770   bd=fndate_mmddyy_to_ccyymmdd(em16): y=int(bd/10000): x=bd-y*10000: z=x*10000+y
01780   pr #22,using L1790: "RS",sr2,"UTAX",ssn,first$,mid$,last$,"","",monthyr$,m1*100,h2*100,0,z,0,"",e$(sr1)(1:9),"","",country$(1:2),"",naics$,"","","","",""
01790 L1790: form pos 1,c 2,g 2,c 5,pic(#########),c 15,c 15,c 20,c 4,c 124,c 6,2*pic(###########),n 2,n 8,n 8,c 5,c 9,c 81,c 3,c 3,c 1,c 6,c 1,c 10,c 1,c 5,c 145
01800   pr #255,using L1810: ssn,trim$(first$)&trim$(last$),m1,h2
01810 L1810: form pos 1,n 12,x 2,c 25,2*pic(zz,zzz,zzz.##)
01820   totwage+=m1: tottaxable+=h2: totemployees+=1
01830 L1830: t1=t1+1: mat t1=t1+w2
01840   mat i1=i1+w2
01850   mat i2=i2+w3
01860   mat t2=t2+w3
01870   dc2=dc2+dc1
01880   dc3=dc3+dc1
01890   dca2=dca2+dca
01900   dca3=dca3+dca
01910   w2=w3=dca=dc1=0
01920   mat w2=(0)
01930   mat w3=(0)
01940   mat s2=(0)
01950   return 
01960 ! ______________________________________________________________________
01970 RECRF: pr #22,using L1980: "RF"," ",tw1,""
01980 L1980: form pos 1,c 2,c 5,pic(#########),c 496
01990   return 
02000 ! ______________________________________________________________________
02010 END1: ! 
02020   pr #255,using "form skip 1,pos 1,c 14,pic(zz,zzz,zzz.##)": "Total wages:",totwage,"Total Taxable:",tottaxable
02030   pr #255,using "form pos 1,c 16,pic(zz,zzz,zzz)": "Total employees:",totemployees
02040   gosub L2070
02050 XIT: fnxit
02060 ! ______________________________________________________________________
02070 L2070: close #24: ioerr L2090
02080   dim a$*512
02090 L2090: close #22: ioerr L2100
02100 L2100: open #24: "Name="&env$('temp')&"\x,RecL=514,EOL=NONE,REPLACE",external,output 
02110   open #22: "Name=[Q]\UCReport,RecL=512",display,input 
02120 L2120: linput #22: a$ eof L2170
02130   if a$(512:512)="X" then a$(512:512)=""
02140   write #24,using L2150: rpad$(a$,512),chr$(13),chr$(10)
02150 L2150: form pos 1,c 512,c 1,c 1
02160   goto L2120
02170 L2170: close #24: 
02180   close #22: 
02190   execute "COPY "&env$('temp')&"\x a:UCReport"
02200   return 
02210 SCR2: ! 
02220   dim contact$*27,email$*40
02230   win=101
02240   win_height=12: win_width=75: display_cnam=1: button_option=2: gosub L2680
02250   pr #win,fields "04,2,Cr 31,N": "Personal ID Number:" !:
        pr #win,fields "05,2,Cr 31,N": "Resub Indicator:" !:
        pr #win,fields "06,2,Cr 31,N": "Resub TLCN:" !:
        pr #win,fields "07,2,Cr 31,N": "Contact Name:" !:
        pr #win,fields "08,2,Cr 31,N": "Contact Phone Number:" !:
        pr #win,fields "09,2,Cr 31,N": "Contact Phone Extension:" !:
        pr #win,fields "10,2,Cr 31,N": "Contact E-Mail:" !:
        pr #win,fields "11,2,Cr 31,N": "Terminating Business Indicator:" !:
        pr #win,fields "12,2,Cr 66,N": "Is Medicare W/H a separate field in the employee record (Y/N):"
02260   scr2_io$(1)="04,34,C 17,UT,N" !:
        scr2_io$(2)="05,34,C 01,UT,N" !:
        scr2_io$(3)="06,34,C 06,UT,N" !:
        scr2_io$(4)="07,34,C 27,UT,N" !:
        scr2_io$(5)="08,34,C 15,UT,N" !:
        scr2_io$(6)="09,34,C 05,UT,N" !:
        scr2_io$(7)="10,34,C 40,UT,N" !:
        scr2_io$(8)="11,34,C 01,UT,N" !:
        scr2_io$(9)="12,68,Cu 01,UT,N"
02270   if resub$="" then resub$="0"
02280 ! If TLCN$="" Then tLCN$="0"
02290   if terminat$="" then terminat$="0"
02300   med$="Y"
02310 L2310: rinput #win,fields mat scr2_io$: emppin$,resub$,tlcn$,contact$,contactph$,phoneext$,email$,terminat$,med$ conv CONV_SCR2
02320   if ce>0 then scr2_io$(ce)(ce1:ce2)="U": ce=0
02330   if cmdkey>0 then goto L2400 else ce=curfld
02340 L2340: ce=ce+1: if ce>udim(scr2_io$) then ce=1
02350 L2350: scr2_io$(ce)=rtrm$(scr2_io$(ce)) : ce1=pos(scr2_io$(ce),"U",9) : if ce1=0 then goto L2340
02360   ce2=ce1+1 : scr2_io$(ce)(ce1:ce1)="UC" : goto L2310
02370 CONV_SCR2: if ce>0 then scr2_io$(ce)(ce1:ce2)="U"
02380   ce=cnt+1
02390 ERR_SCR2: pr f "24,78,C 1": bell : goto L2350
02400 L2400: if resub$<>"0" and resub$<>"1" then ce=2 !:
          goto ERR_SCR2
02410   if resub$="1" and rtrm$(tlcn$)="" then ce=3 !:
          goto ERR_SCR2
02420   if terminat$<>"0" and terminat$<>"1" then ce=8 !:
          goto ERR_SCR2
02430   if uprc$(med$)="Y" or uprc$(med$)="N" then goto L2440 else ce=9: goto ERR_SCR2
02440 L2440: close #win: 
02450   if cmdkey=5 then goto SCR1
02460   return 
02470 ! ______________________________________________________________________
02480 NAME_BREAKDOWN: ! 
02490   dim first$*15,mid$*15,last$*20,em$(3)*30
02500   em$(1)=uprc$(rtrm$(em$(1))): ! nAMCDE$="s"
02510   x1=pos(em$(1)," ",1)
02520   x2=pos(em$(1)," ",x1+1)
02530   x3=pos(em$(1)," ",x2+1)
02540   if uprc$(namcde$)="S" or uprc$(namcde$)="L" then goto L2590
02550   first$=em$(1)(1:min(15,max(x1-1,1)))
02560   if x2>0 then mid$=em$(1)(x1+1:x2-1): last$=em$(1)(x2+1:len(em$(1)))
02570   if x2=0 then last$=em$(1)(x1+1:len(em$(1))): mid$=""
02580   goto L2640
02590 L2590: ! last name first
02600   if x1=0 then x1=pos(em$(1),",",1)
02610   if x1>0 and em$(1)(x1-1:x1-1)="," then last$=em$(1)(1:x1-2) else last$=em$(1)(1:max(x1-1,1))
02620   if x2>0 then first$=em$(1)(x1+1:x2-1): mid$=em$(1)(x2+1:len(em$(1)))
02630   if x2=0 then first$=em$(1)(x1+1:len(em$(1)))(1:15): mid$=""
02640 L2640: x=pos(first$,",",1): if x>0 then first$(x:x)=""
02650   x=pos(last$,",",1): if x>0 then last$(x:x)=""
02660 ! pr FIRST$,MID$,LAST$
02670   return 
02680 L2680: ! 
02750   if exists("C:\ACS\Local\Settings\No_Print_Newpage.txt") then goto L2760 else pr newpage
02760 L2760: screen_width=80
02770   screen_height=24
02780   if display_cnam=0 then goto L2790
02790 L2790: sc=max(int(((screen_width-win_width)/2)+1),2)
02800   ec=min(sc+win_width-1,79)
02810   sr=max(int(((screen_height-win_height)/2)+1),2)
02820   er=min(sr+win_height-1,23)
02830 !     pr "win_height="&STR$(WIN_HEIGHT),"win_width="&STR$(WIN_WIDTH)
02840 !     pr "sr="&STR$(SR),"sc="&STR$(SC)
02850 !     pr "er="&STR$(ER),"ec="&STR$(EC) : Pause
02860   close #win: ioerr L2870
02870 L2870: open #win: "SRow="&str$(sr)&",SCol="&str$(sc)&",ERow="&str$(er)&",ECol="&str$(ec)&",Border=Sr,Caption=<"&cap$,display,outIn 
02880   pr #win: newpage
02890   if display_cnam=0 then goto L2920
02900   if display_cnam=1 then !:
          pr #win,fields "1,1,Cc "&str$(win_width)&",R,N": env$('cnam')(1:min(40,win_width)) !:
          pr #win,fields "2,1,Cc "&str$(win_width)&",R,N": "Company Number [cno]"(1:min(40,win_width))
02910   if display_cnam=2 then !:
          pr #win,fields "1,1,Cc "&str$(win_width)&",R,N": "Company Number [cno]"(1:min(40,win_width))
02920 L2920: if button_option=0 then goto L3030
02930   mat fkey$=("") : em$="" : es=0
02940   fkey$(5)="Cancel" ! included by default
02950   if button_option=2 then !:
          fkey$(1)="Next"
02960   if button_option=3 then !:
          fkey$(1)="Print"
02970   if button_option=4 then !:
          fkey$(1)="Save"
02980   if button_option=5 then !:
          fkey$(1)="Next" !:
          fkey$(6)="Search"
02990   if button_option=6 then !:
          fkey$(1)="Next" !:
          fkey$(2)="Back"
03000   if button_option=7 then !:
          fkey$(1)="Save" !:
          fkey$(4)="Delete"
03010   scrline=er+1: gosub L3660 !  fnFKEY(ER+1,MAT FKEY$,MAT DISFK,EM$,ES)
03020 ! 
03030 L3030: return  ! Fnend
03040 L3040: ! Def Library fnOldMsgBox(mat RESPONSE$,&CAP$,mat MSGLINE$,MTYPE)
03050 ! mtype=0 means splash    - returns no response                                 ! mostly for "please wait..." and "printing..."                                 ! (anywhere no response is required - no buttons are displyed either)
03060 ! mtype=1 means OK only   - returns no response
03070 ! mtype=2 means Yes or No - returns "Y" or "N"
03080 ! mtype=3 means Yes, No, Cancel - returns "Y" or "N" or ""
03090 ! response$(1)= code you're looking for 2-5 are reserved for future use
03100   close #104: ioerr L3110
03110 L3110: endrow=12
03120   for j=2 to udim(msgline$)
03130     if msgline$(j)<>"" then endrow=endrow+1
03140   next j
03150   open #104: "SRow=10,SCol=09,ERow="&str$(endrow)&",ECol=70,Border=SR,Caption=<"&cap$,display,outIn 
03160   pr #104: newpage
03170   mglinerow=2
03180   for j=1 to udim(msgline$)
03190     pr #104,fields str$(mglinerow+j-1)&",2,Cc 60,N": msgline$(j)
03200   next j
03210   if mtype=1 then pr f str$(endrow+1)&",38,Cc 4,B,1": "Ok"
03220   if mtype=1 then input fields str$(endrow)&",09,C 1,AE,N": pause$
03230   if mtype=2 then pr f str$(endrow+1)&",35,Cc 4,B,21": "Yes"
03240   if mtype=2 then pr f str$(endrow+1)&",40,Cc 4,B,22": "No"
03250 L3250: if mtype=2 then input fields str$(endrow)&",09,Cu 1,AE,N": response$(1)
03260   if mtype=2 and cmdkey=22 then response$(1)="N"
03270   if mtype=2 and cmdkey=21 then response$(1)="Y"
03280   if mtype=2 and response$(1)<>"Y" and response$(1)<>"N" then pr f "24,1,C 7,N": bell$ : goto L3250
03290   if mtype=3 then pr f str$(endrow+1)&",29,Cc 4,B,21": "Yes"
03300   if mtype=3 then pr f str$(endrow+1)&",34,Cc 4,B,22": "No"
03310   if mtype=3 then pr f str$(endrow+1)&",39,C 12,B,22": "Cancel (Esc)"
03320   if mtype=3 then input fields str$(endrow)&",09,Cu 1,AE,N": response$(1)
03330   if mtype=3 and cmdkey=22 then response$(1)="N"
03340   if mtype=3 and cmdkey=21 then response$(1)="Y"
03350   if mtype=3 and cmdkey=99 then response$(1)=""
03360   if mtype=3 and response$(1)<>"Y" and response$(1)<>"N" and response$(1)<>"" then pr f "24,1,C 7,N": bell$ : goto L3250
03370   close #104: ioerr L3380
03380 L3380: return  ! Fnend
03390 L3390: ! Def Library FNWAIT(&MESSAGE$,STOPABLE)
03400 ! if stopable=1 will display "Cancel (F5)" button
03410 ! win = window number
03420   close #win: ioerr L3430
03430 L3430: open #win: "Srow=10,SCol=20,ERow=14,ECol=59,Border=Sr,Caption=<"&cap$,display,outIn 
03440   pr #win: newpage
03450   pr #win,fields "1,1,Cc 40,R,N": env$('cnam')
03460   pr #win,fields "2,1,Cc 40,R,N": "Company Number [cno]"
03470   pr #win,fields "4,1,Cc 40,N": message$
03480   if rtrm$(message$)="" then pr #win,fields "4,1,Cc 40,N": "Please wait..."
03490   if stopable=0 then pr f "15,34,C 11,R,N": "Do Not Stop"
03500   if stopable=1 then pr f "15,34,C 11,B,5": "Cancel (F5)"
03510   return  ! Fnend
03520 ! Def Library FNOPENWIN(WIN,SR,SC,ER,EC,&CAP$)
03530   if sr<1 then sr=10
03540   if sc<1 then sc=20
03550   if er<1 then er=14
03560   if ec<1 then ec=59
03570   win_width=ec-sc+1
03580   close #win: ioerr L3590
03590 L3590: open #win: "SRow="&str$(sr)&",SCol="&str$(sc)&",ERow="&str$(er)&",ECol="&str$(ec)&",Border=Sr,Caption=<"&cap$,display,outIn 
03600   pr #win: newpage
03610   pr #win,fields "1,1,Cc "&str$(win_width)&",R,N": env$('cnam')(1:min(40,win_width))
03620   pr #win,fields "2,1,Cc "&str$(win_width)&",R,N": "Company Number [cno]"(1:min(40,win_width))
03630 ! 
03640 ! 
03650   return  ! Fnend
03660 L3660: ! Def Library FNFKEY(SCRLINE,MAT FKEY$,MAT DISFK,&EM$,ES)
03670   totallen=0 !:
        startpos=0
03680   for j=1 to udim(fkey$) ! add ' (Fx)' to each button
03690     if fkey$(j)="" then goto L3720
03700     fkey$(j)=fkey$(j)&" (F"&str$(j)&")" !:
          ! add ' (Fx)' to each button
03710     totallen=totallen+len(fkey$(j))+1
03720 L3720: next j
03730   totallen=totallen+len(rtrm$(em$))+min(len(rtrm$(em$)),1)+es
03740   totallen=totallen-1
03750   startpos=int((80-totallen)/2)+1
03760   pr f str$(scrline)&","&str$(startpos)&",C "&str$(totallen)&",N": rpt$("Ä",totallen)
03770   for j=1 to udim(fkey$)
03780     if fkey$(j)="" then goto L3830
03790     if disfk(j)=1 then pr f str$(scrline)&","&str$(startpos)&",C "&str$(len(fkey$(j)))&",R,"&str$(j): fkey$(j)
03800     if disfk(j)=1 then goto L3820
03810     pr f str$(scrline)&","&str$(startpos)&",C "&str$(len(fkey$(j)))&",B,"&str$(j): fkey$(j)
03820 L3820: startpos=startpos+len(fkey$(j))+1
03830 L3830: next j
03840   if rtrm$(em$)="" then goto L3860
03850   pr f str$(scrline)&","&str$(startpos)&",C "&str$(len(rtrm$(em$))+es)&",R,N": rtrm$(em$)
03860 L3860: return  ! Fnend
03870 STATE_BREAKDOWN: ! extract state name
03880   holdst$="          "
03890   p3=oldp3=0
03900   p4=10
03910   for j=1 to 10
03920     p3=pos(rtrm$(em$(3))," ",p3+1)
03930     if oldp3>p3 then goto L3960 ! end of address reached
03940     if p3>0 then oldp3=p3 else goto L3950
03950 L3950: next j
03960 L3960: for j=1 to 10
03970     if rtrm$(em$(3)(oldp3-j:oldp3-j))="" or em$(3)(oldp3-j:oldp3-j)="," then goto L3980 else p4=p4-1: holdst$(p4:p4)=em$(3)(oldp3-j:oldp3-j): goto L3990
03980 L3980: if rtrm$(holdst$)="" then goto L3990 else goto L4000
03990 L3990: next j
04000 L4000: holdst$=ltrm$(holdst$)(1:2)
04010   if holdst$="TE" then holdst$="TX"
04020   return 
04030 ! ______________________________________________________________________
04040 ! <Updateable Region: ERTN>
04050 ! ______________________________________________________________________
04060 CALCULATEUC: ! determine quarterly wages
04070   dcy=dcq=0
04080   for j=1 to 10
04090     if dedfed(j)=1 then dcy=dcy+m(j*2+9)
04100     if dedfed(j)=1 then dcq=dcq+m(j*2+10)
04110   next j
04120   m2=m2+m(1)-dcy
04130   m1=m1+m(2)-dcq
04140   if m2=0 then goto L4280 ! skip if total wage =0
04150   if m1=0 then goto L4280 ! SKIP IF QUARTERLY WAGE=0
04160   p3=p3+1
04170   if m2<m(sr1) then goto L4230
04180   if m2-m1>m(sr1) then goto L4210
04190   h2=m(sr1)-(m2-m1)
04200   goto L4240
04210 L4210: h2=0
04220   goto L4240
04230 L4230: h2=m1
04240 L4240: h3=m1-h2
04250   t1=t1+m1
04260   t2=t2+h3
04270   t3=t3+h2
04280 L4280: return 
04290 ERTN: ! 
04300   if err=61 then pr f "23,1,C 80,N": "THIS PROGRAM IS TRYING TO ACCESS A RECORD THAT IS IN USE!" else goto L4320
04310   goto L4360
04320 L4320: pr newpage
04330   if err=4148 then pr f "23,1,C 80,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L4350
04340   goto L4360
04350 L4350: pr f "23,1,C 80,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
04360 L4360: pr f "24,1,C 80,N": "PRESS ENTER TO RETRY; ELSE ENTER  Q  TO QUIT"
04370   input fields "24,60,C 1,N": quitcode$
04380   if rtrm$(uprc$(quitcode$))="Q" then goto L4420
04390   pr f "23,1,C 80,N": ""
04400   pr f "24,1,C 80,N": ""
04410   retry 
04420 L4420: fnxit
