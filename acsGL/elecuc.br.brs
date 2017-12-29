00010 ! REPLACE S:\acsGL\Elecuc
00020 ! never could get it work with an RO record if all zeroes; without !:
        ! RO record i had to put an RE record in front of every RW record and !:
        ! had to follow with an RT record - Right now this program will not !:
        ! create an RO record
00030 ! ______________________________________________________________________
00040   library "S:\Core\Library": fntop,fnxit, fnerror,fnoldmsgbox,fntop,fnxit,fnopenprn,fncloseprn,fndate_mmddyy_to_ccyymmdd
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim em$(3)*30,ss$*11,d(14),ty(21),tqm(17),s(9),t(9),z$*8,cap$*128,message$*40
00080   dim a$(3)*40,b$*12,g$*12,d$(10)*8,e$(10)*12,s2(2)
00090   dim fa$(1),fb$(1),fc$(1),fd$(1),l$(10),dedfed(10),w3(2),i2(2),t2(2)
00100   dim emppin$*17,tlcn$*6,contact$*27,contactph$*15,phoneext$*5,email$*40
00110   dim w2(9),i1(9),t1(9),ct$*20,st$*2,ibm$*8,namcde$*1,typemp$*1,io1$(15)
00120   dim terminat$*1,first$*15,mid$*15,last$*20,m(10),r(10),e$(10)*12
00130 ! ______________________________________________________________________
00140   fntop(program$,cap$="Electronic U/C")
00220   on fkey 5 goto XIT
00230 ! 
00250   open #1: "Name="&env$('Q')&"\GLmstr\Company.h"&env$('cno')&",SHR",internal,input 
00260   read #1,using L270: mat a$,b$,mat d$,loccode,mat e$,mat dedfed,oldmax,mat m,mat r,mat e$,mat dedcode
00270 L270: form pos 1,3*c 40,c 12,pos 150,10*c 8,n 2,pos 317,10*c 12,pos 638,10*n 1,pos 239,pd 4.2,pos 247,10*pd 4.2,10*pd 3.3,10*c 12,pos 618,10*n 1
00280   close #1: 
00290 ! ______________________________________________________________________
00300 L300: p1=pos(b$,"-",1)
00310   if p1=0 then goto L340
00320   b$(p1:p1)=""
00330   goto L300
00340 L340: b1=val(b$)
00350   p1=pos(a$(3),",",1): comma=1
00360   if p1=0 then p1=pos(a$(3)," ",1): comma=0
00370   ct$=a$(3)(1:p1-1)
00380   if comma=1 then st$=a$(3)(p1+2:p1+3) else st$=a$(3)(p1+1:p1+2)
00390   if uprc$(a$(3)(p1+2:p1+4))="TEX" then st$="Tx"
00400   p2=len(rtrm$(a$(3)))
00410   p1=p2-4
00420   zip$=a$(3)(p1:p2)
00430   io1$(1)="5,25,C 40,UT,N"
00440   io1$(2)="6,25,C 40,UT,N"
00450   io1$(3)="7,25,C 20,UT,N"
00460   io1$(4)="8,25,c 2,UT,N"
00470   io1$(5)="9,25,C 5,UT,N"
00480   io1$(6)="10,25,N 9,UT,N"
00490   io1$(7)="11,25,N 6,UT,N"
00500   io1$(8)="12,52,C 1,UT,N"
00510   io1$(9)="13,38,C 1,UT,N"
00520   io1$(10)="14,68,n 2,UT,N"
00530   io1$(11)="15,43,n 2,UT,N"
00540   io1$(12)="17,25,c 3,UT,N"
00550   io1$(13)="18,25,c 6,UT,N"
00560   io1$(14)="19,57,c 1,UT,N"
00570   io1$(15)="20,49,c 1,UT,N"
00580   namcde$="F"
00590   typemp$="R"
00600 ! ______________________________________________________________________
00610 SCR1: ! 
00620   pr newpage
00630   close #101: ioerr L640
00640 L640: open #101: "SROW=2,SCOL=3,EROW=23,ECOL=77,BORDER=DR,CAPTION=<Create Electronic U/C Diskette for state.",display,outIn 
00650   pr f "3,15,C 51,R,N": "  Insert diskette for elecronic U/C in drive A:"
00660   pr f "5,5,C 60": "Company Name:"
00670   pr f "6,5,C 60": "Street Address:"
00680   pr f "7,5,C 60": "City:"
00690   pr f "8,5,C 60": "State:"
00700   pr f "9,5,C 60": "Zip Code:"
00710   pr f "10,5,C 60": "Federal ID #:"
00720   pr f "11,5,C 60": "Quarter Ending Date:"
00730   pr f "12,5,C 60,N": "F=First Name First or S=Surname First on File:"
00740   pr f "13,5,C 60": "Type of Business Code R=Regular:"
00750   pr f "14,5,C 65": "State code used in your record to identify the selected state:"
00760   pr f "15,5,C 60": "Appropriate FIPS postal numeric code:"
00770   pr f "16,5,C 70": "(See an appendix in your electronic booklet for the postal code!)"
00780   pr f "17,5,C 60": "Country Code:"
00790   pr f "18,5,C 60": "NAICS Code:"
00800   pr f "19,5,C 60": "Deduct Cafiteria Plans for Calculating Wages (Y/N)?"
00810   pr f "20,5,C 60": "Deduct Pension for Calculating Wages (Y/N)?"
00820   pr f "22,28,C 9,B,1": "Next (F1)"
00830   pr f "22,39,C 11,B,5": "Cancel (F5)"
00835   if b1>999999999 then b1=0
00840   pr f mat io1$: a$(1),a$(2),ct$,st$,zip$,b1,yr,namcde$,typemp$,1,48
00850 L850: input fields mat io1$,attr "R": a$(1),a$(2),ct$,st$,zip$,b1,endingdate,namcde$,typemp$,sr1,sr2,country$,naics$,cafiteria$,pension$ conv CONV1
00860   if ce>0 then io1$(ce)(ce1:ce2)="U": ce=0
00870   if cmdkey>0 then goto L940 else ce=curfld+1
00880   if ce>udim(io1$) then ce=1
00890 L890: io1$(ce)=rtrm$(uprc$(io1$(ce))) : ce1=pos(io1$(ce),"U",1)
00900   ce2=ce1+1 : io1$(ce)(ce1:ce1)="UC" : goto L850
00910 CONV1: if ce>0 then io1$(ce)(ce1:ce2)="U"
00920   ce=cnt+1
00930 ERR1: pr f "24,78,C 1": bell : goto L890
00940 L940: ! 
00950   if cmdkey=5 then goto XIT
00960   if rtrm$(a$(1))="" then ce=1: goto ERR1
00970   if rtrm$(a$(2))="" then ce=2: goto ERR1
00980   if rtrm$(ct$)="" then ce=3: goto ERR1
00990   if rtrm$(st$)="" then ce=4: goto ERR1
01000   if rtrm$(zip$)="" then ce=5: goto ERR1
01010   if b1=0 then ce=6: goto ERR1
01020   if endingdate<010100 or endingdate>123199 then ce=7: goto ERR1
01030   if sr1<0 or sr1>10 then ce=10: goto ERR1
01040   if sr2<0 or sr1>99 then ce=11: goto ERR1
01050   cafiteria$=uprc$(cafiteria$): pension$=uprc$(pension$)
01060   if cafiteria$="Y" or cafiteria$="N" then goto L1070 else ce=14: goto ERR1
01070 L1070: if pension$="Y" or pension$="N" then goto L1080 else ce=15: goto ERR1
01080 L1080: monthyr$=cnvrt$("pic(######)",endingdate)(1:2)&"20"&cnvrt$("pic(######)",endingdate)(5:6)
01090   yr=endingdate-(int(endingdate/100)*100)+2000
01100 ! ______________________________________________________________________
01110   gosub SCR2
01120   pr newpage
01130   win=101
01140   message$=""
01150   stopable=1: gosub L3370 ! fnWAIT(MESSAGE$,1)
01160 ! ______________________________________________________________________
01170   open #1: "Name="&env$('Q')&"\GLmstr\RPMSTR.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\RPIndex.h"&env$('cno')&",SHR",internal,input,keyed 
01180   open #2: "Name="&env$('Q')&"\GLmstr\RPTrail.h"&env$('cno')&",SHR",internal,input,relative 
01190 L1190: open #22: "Name="&env$('Q')&"\UCReport,RECL=512,eol=crlf,replace",display,output 
01200 ! ______________________________________________________________________
01210   goto BEGINNING_OF_FILE
01220   pr newpage
01230   msgline$(1)="Insert Diskette"
01240   mtype=1
01250   if err=4221 then gosub L3020
01260   goto L1190
01270 ! ______________________________________________________________________
01280 BEGINNING_OF_FILE: gosub RECRA : gosub RECRE: fnopenprn
01290   pr #255,using "form pos 20,cc 40,skip 1,pos 20,cc 40": "Electronic Edit List",cnvrt$("pic(zz/zz/zzzz",endingdate)
01300 READ_EMPLOYEE: read #1,using L1330: eno,mat em$,ss$,em6,em16,ta eof END1
01310   m1=m2=m3=m4=0
01320   gosub NAME_BREAKDOWN
01330 L1330: form pos 1,n 8,3*c 30,c 11,pos 122,n 2,pos 156,n 6,pos 173,pd 3
01340   r1=r1+1
01350   p1=pos(em$(3),",",1) : comma=1
01360   if p1=0 then p1=pos(em$(3)," ",1): comma=0
01370   emct$=em$(3)(1:p1-1)
01380   gosub STATE_BREAKDOWN: emst$=holdst$ ! If COMMA=1 Then eMST$=EM$(3)(P1+2:P1+3) Else eMST$=EM$(3)(P1+1:P1+2)
01390   p2=len(rtrm$(em$(3)))
01400   p1=p2-4
01410   emzip$=em$(3)(p1:p2)
01420 L1420: p1=pos(ss$,"-",1)
01430   if p1>0 then ss$(p1:p1)="": goto L1420 else ssn=val(ss$)
01440 READ_DEPARTMENT: read #2,using L1460,rec=ta: teno,tcd,mat ty,mat tqm,ta
01450 ! If SS$="459499366" Then Pause
01460 L1460: form pos 1,n 8,pos 48,n 2,pos 168,38*pd 5.2,pos 468,pd 3
01470   if tcd<1 or tcd>10 then tcd=1
01480   gosub CALCULATEUC ! determine wages for quarter
01510   if ta>0 then goto READ_DEPARTMENT
01520 ! Gosub RECRE
01530   gosub RECRS
01540   tw1=tw1+1 ! counter
01550   goto READ_EMPLOYEE
01560 ! ______________________________________________________________________
01570 RECRA: pr #22,using L1580: "RA",b1,"","98",a$(1),"",a$(2)(1:22),ct$,st$,zip$,"","","","",country$(1:2),a$(1),"",a$(2)(1:22),ct$,st$,zip$,"","","","",country$(1:2),contact$,contactph$,phoneext$,"",email$,"","",""
01580 L1580: form pos 1,c 2,pic(#########),c 24,c 2,c 57,c 22,c 22,c 22,c 2,c 5,c 4,c 5,c 23,c 15,c 2,c 57,c 22,c 22,c 22,c 2,c 5,c 4,c 5,c 23,c 15,c 2,c 27,c 15,c 5,c 3,c 40,c 3,c 10,c 14
01590   return 
01600 ! ______________________________________________________________________
01610 RECRE: pr #22,using L1620: "RE",yr,"",b1,"","0","","",a$(1),"",a$(2)(1:22),ct$,st$,zip$,"","",e$(sr1)(1:9),monthyr$,"",r(sr1)*.01,"",naics$,""
01620 L1620: form pos 1,c 2,pic(####),c 1,pic(#########),c 9,c 1,c 4,c 9,c 57,c 22,c 22,c 22,c 2,c 5,c 4,c 126,c 9,c 6,c 1,n 5.4,c 1,c 6,c 185
01630   return 
01640 ! ______________________________________________________________________
01650   form pos 1,c 2,pic(#########),c 15,c 15,c 20,c 4,c 22,c 22,c 22,c 2,c 5,c 4,c 5,c 23,c 15,c 2,18*pic(###########),c 22,2*pic(###########),c 56,n 1,c 1,c 1,n 1,c 23,
01660   pr #22,using L1670: "2E",ct$,st$,"",zip$,namcde$,typemp$,"","","",""
01670 L1670: form pos 1,c 2,g 25,g 10,2*g 5,2*g 1,g 2,g 4,g 2,c 71
01680   return 
01690 ! ______________________________________________________________________
01700 RECRS: ! STATE RECORD
01710   if sr1=0 then goto L1790 ! NO STATE SELECTED
01720   if m1=0 then goto L1790 ! NO quarterly wages
01730   bd=fndate_mmddyy_to_ccyymmdd(em16): y=int(bd/10000): x=bd-y*10000: z=x*10000+y
01740   pr #22,using L1750: "RS",sr2,"UTAX",ssn,first$,mid$,last$,"","",monthyr$,m1*100,h2*100,0,z,0,"",e$(sr1)(1:9),"","",country$(1:2),"",naics$,"","","","",""
01750 L1750: form pos 1,c 2,g 2,c 5,pic(#########),c 15,c 15,c 20,c 4,c 124,c 6,2*pic(###########),n 2,n 8,n 8,c 5,c 9,c 81,c 3,c 3,c 1,c 6,c 1,c 10,c 1,c 5,c 145
01760   pr #255,using L1770: ssn,trim$(first$)&trim$(last$),m1,h2
01770 L1770: form pos 1,n 12,x 2,c 25,2*pic(zz,zzz,zzz.##)
01780   totwage+=m1: tottaxable+=h2: totemployees+=1
01790 L1790: t1=t1+1: mat t1=t1+w2
01800   mat i1=i1+w2
01810   mat i2=i2+w3
01820   mat t2=t2+w3
01830   dc2=dc2+dc1
01840   dc3=dc3+dc1
01850   dca2=dca2+dca
01860   dca3=dca3+dca
01870   w2=w3=dca=dc1=0
01880   mat w2=(0)
01890   mat w3=(0)
01900   mat s2=(0)
01910   return 
01920 ! ______________________________________________________________________
01930 RECRF: pr #22,using L1940: "RF"," ",tw1,""
01940 L1940: form pos 1,c 2,c 5,pic(#########),c 496
01950   return 
01960 ! ______________________________________________________________________
01970 END1: ! 
01980   pr #255,using "form skip 1,pos 1,c 14,pic(zz,zzz,zzz.##)": "Total wages:",totwage,"Total Taxable:",tottaxable
01990   pr #255,using "form pos 1,c 16,pic(zz,zzz,zzz)": "Total employees:",totemployees
02000   fncloseprn
02010   gosub L2040
02020 XIT: fnxit
02030 ! ______________________________________________________________________
02040 L2040: close #24: ioerr L2060
02050   dim a$*512
02060 L2060: close #22: ioerr L2070
02070 L2070: open #24: "NAME="&env$('temp')&"\x,RECL=514,EOL=NONE,REPLACE",external,output 
02080   open #22: "Name="&env$('Q')&"\UCReport,RECL=512",display,input 
02090 L2090: linput #22: a$ eof L2140
02100   if a$(512:512)="X" then a$(512:512)=""
02110   write #24,using L2120: rpad$(a$,512),chr$(13),chr$(10)
02120 L2120: form pos 1,c 512,c 1,c 1
02130   goto L2090
02140 L2140: close #24: 
02150   close #22: 
02152   savelocation$=trim$(savelocation$)
02154   if savelocation$(len(savelocation$):len(savelocation$))<>"\" then savelocation$=savelocation$&"\"
02160   execute "COPY "&env$('temp')&"\x "&savelocation$&"UCReport"
02170   return 
02180 SCR2: ! 
02190   dim contact$*27,email$*40,savelocation$*40
02200   win=101
02210   win_height=13: win_width=75: display_cnam=1: button_option=2: gosub L2650
02220   pr #win,fields "04,2,Cr 31,N": "Personal ID Number:" !:
        pr #win,fields "05,2,Cr 31,N": "Resub Indicator:" !:
        pr #win,fields "06,2,Cr 31,N": "Resub TLCN:" !:
        pr #win,fields "07,2,Cr 31,N": "Contact Name:" !:
        pr #win,fields "08,2,Cr 31,N": "Contact Phone Number:" !:
        pr #win,fields "09,2,Cr 31,N": "Contact Phone Extension:" !:
        pr #win,fields "10,2,Cr 31,N": "Contact E-Mail:" !:
        pr #win,fields "11,2,Cr 31,N": "Terminating Business Indicator:" !:
        pr #win,fields "12,2,Cr 66,N": "Is Medicare W/H a seperate field in the employee record (Y/N):" !:
        pr #win,fields "13,2,Cr 25,N": "Location to save output:"
02230   scr2_io$(1)="04,34,C 17,UT,N" !:
        scr2_io$(2)="05,34,C 01,UT,N" !:
        scr2_io$(3)="06,34,C 06,UT,N" !:
        scr2_io$(4)="07,34,C 27,UT,N" !:
        scr2_io$(5)="08,34,C 15,UT,N" !:
        scr2_io$(6)="09,34,C 05,UT,N" !:
        scr2_io$(7)="10,34,C 40,UT,N" !:
        scr2_io$(8)="11,34,C 01,UT,N" !:
        scr2_io$(9)="12,68,Cu 01,UT,N" !:
        scr2_io$(10)="13,27,C 40,UT,N"
02240   if resub$="" then resub$="0"
02250 ! If TLCN$="" Then tLCN$="0"
02260   if terminat$="" then terminat$="0"
02270   med$="Y" : savelocation$="A:\"
02280 L2280: rinput #win,fields mat scr2_io$: emppin$,resub$,tlcn$,contact$,contactph$,phoneext$,email$,terminat$,med$,savelocation$ conv CONV_SCR2
02290   if ce>0 then scr2_io$(ce)(ce1:ce2)="U": ce=0
02300   if cmdkey>0 then goto L2370 else ce=curfld
02310 L2310: ce=ce+1: if ce>udim(scr2_io$) then ce=1
02320 L2320: scr2_io$(ce)=rtrm$(scr2_io$(ce)) : ce1=pos(scr2_io$(ce),"U",9) : if ce1=0 then goto L2310
02330   ce2=ce1+1 : scr2_io$(ce)(ce1:ce1)="UC" : goto L2280
02340 CONV_SCR2: if ce>0 then scr2_io$(ce)(ce1:ce2)="U"
02350   ce=cnt+1
02360 ERR_SCR2: pr f "24,78,C 1": bell : goto L2320
02370 L2370: if resub$<>"0" and resub$<>"1" then ce=2 !:
          goto ERR_SCR2
02380   if resub$="1" and rtrm$(tlcn$)="" then ce=3 !:
          goto ERR_SCR2
02390   if terminat$<>"0" and terminat$<>"1" then ce=8 !:
          goto ERR_SCR2
02400   if uprc$(med$)="Y" or uprc$(med$)="N" then goto L2410 else ce=9: goto ERR_SCR2
02410 L2410: close #win: 
02420   if cmdkey=5 then goto SCR1
02430   return 
02440 ! ______________________________________________________________________
02450 NAME_BREAKDOWN: ! 
02460   dim first$*15,mid$*15,last$*20,em$(3)*30
02470   em$(1)=uprc$(rtrm$(em$(1))): ! nAMCDE$="s"
02480   x1=pos(em$(1)," ",1)
02490   x2=pos(em$(1)," ",x1+1)
02500   x3=pos(em$(1)," ",x2+1)
02510   if uprc$(namcde$)="S" or uprc$(namcde$)="L" then goto L2560
02520   first$=em$(1)(1:min(15,max(x1-1,1)))
02530   if x2>0 then mid$=em$(1)(x1+1:x2-1): last$=em$(1)(x2+1:len(em$(1)))
02540   if x2=0 then last$=em$(1)(x1+1:len(em$(1))): mid$=""
02550   goto L2610
02560 L2560: ! last name first
02570   if x1=0 then x1=pos(em$(1),",",1)
02580   if x1>0 and em$(1)(x1-1:x1-1)="," then last$=em$(1)(1:x1-2) else last$=em$(1)(1:max(x1-1,1))
02590   if x2>0 then first$=em$(1)(x1+1:x2-1): mid$=em$(1)(x2+1:len(em$(1)))
02600   if x2=0 then first$=em$(1)(x1+1:len(em$(1)))(1:15): mid$=""
02610 L2610: x=pos(first$,",",1): if x>0 then first$(x:x)=""
02620   x=pos(last$,",",1): if x>0 then last$(x:x)=""
02630 ! pr FIRST$,MID$,LAST$
02640   return 
02650 L2650: ! 
02720   if exists("C:\ACS\Local\Settings\No_Print_Newpage.txt") then goto L2730 else pr newpage
02730 L2730: screen_width=80
02740   screen_height=24
02750   if display_cnam=0 then goto L2770
02770 L2770: sc=max(int(((screen_width-win_width)/2)+1),2)
02780   ec=min(sc+win_width-1,79)
02790   sr=max(int(((screen_height-win_height)/2)+1),2)
02800   er=min(sr+win_height-1,23)
02810 !     pr "win_height="&STR$(WIN_HEIGHT),"win_width="&STR$(WIN_WIDTH)
02820 !     pr "sr="&STR$(SR),"sc="&STR$(SC)
02830 !     pr "er="&STR$(ER),"ec="&STR$(EC) : Pause
02840   close #win: ioerr L2850
02850 L2850: open #win: "SRow="&str$(sr)&",SCol="&str$(sc)&",ERow="&str$(er)&",ECol="&str$(ec)&",Border=Sr,Caption=<"&cap$,display,outIn 
02860   pr #win: newpage
02870   if display_cnam=0 then goto L2900
02880   if display_cnam=1 then !:
          pr #win,fields "1,1,Cc "&str$(win_width)&",R,N": env$('cnam')(1:min(40,win_width)) !:
          pr #win,fields "2,1,Cc "&str$(win_width)&",R,N": "Company Number "&env$('cno')(1:min(40,win_width))
02890   if display_cnam=2 then !:
          pr #win,fields "1,1,Cc "&str$(win_width)&",R,N": "Company Number "&env$('cno')(1:min(40,win_width))
02900 L2900: if button_option=0 then goto L3010
02910   mat fkey$=("") : em$="" : es=0
02920   fkey$(5)="Cancel" ! included by default
02930   if button_option=2 then !:
          fkey$(1)="Next"
02940   if button_option=3 then !:
          fkey$(1)="Print"
02950   if button_option=4 then !:
          fkey$(1)="Save"
02960   if button_option=5 then !:
          fkey$(1)="Next" !:
          fkey$(6)="Search"
02970   if button_option=6 then !:
          fkey$(1)="Next" !:
          fkey$(2)="Back"
02980   if button_option=7 then !:
          fkey$(1)="Save" !:
          fkey$(4)="Delete"
02990   scrline=er+1: gosub L3660
03000 ! 
03010 L3010: return  ! Fnend
03020 L3020: ! Def Library fnOldMsgBox(mat RESPONSE$,&CAP$,mat MSGLINE$,MTYPE)
03030 ! mtype=0 means splash    - returns no response                                 ! mostly for "please wait..." and "printing..."                                 ! (anywhere no response is required - no buttons are displyed either)
03040 ! mtype=1 means OK only   - returns no response
03050 ! mtype=2 means Yes or No - returns "Y" or "N"
03060 ! mtype=3 means Yes, No, Cancel - returns "Y" or "N" or ""
03070 ! response$(1)= code you're looking for 2-5 are reserved for future use
03080   close #104: ioerr L3090
03090 L3090: endrow=12
03100   for j=2 to udim(msgline$)
03110     if msgline$(j)<>"" then endrow=endrow+1
03120   next j
03130   open #104: "SRow=10,SCol=09,ERow="&str$(endrow)&",ECol=70,Border=SR,Caption=<"&cap$,display,outIn 
03140   pr #104: newpage
03150   mglinerow=2
03160   for j=1 to udim(msgline$)
03170     pr #104,fields str$(mglinerow+j-1)&",2,Cc 60,N": msgline$(j)
03180   next j
03190   if mtype=1 then pr f str$(endrow+1)&",38,Cc 4,B,1": "Ok"
03200   if mtype=1 then input fields str$(endrow)&",09,C 1,AE,N": pause$
03210   if mtype=2 then pr f str$(endrow+1)&",35,Cc 4,B,21": "Yes"
03220   if mtype=2 then pr f str$(endrow+1)&",40,Cc 4,B,22": "No"
03230 L3230: if mtype=2 then input fields str$(endrow)&",09,Cu 1,AE,N": response$(1)
03240   if mtype=2 and cmdkey=22 then response$(1)="N"
03250   if mtype=2 and cmdkey=21 then response$(1)="Y"
03260   if mtype=2 and response$(1)<>"Y" and response$(1)<>"N" then pr f "24,1,C 7,N": bell$ : goto L3230
03270   if mtype=3 then pr f str$(endrow+1)&",29,Cc 4,B,21": "Yes"
03280   if mtype=3 then pr f str$(endrow+1)&",34,Cc 4,B,22": "No"
03290   if mtype=3 then pr f str$(endrow+1)&",39,C 12,B,22": "Cancel (Esc)"
03300   if mtype=3 then input fields str$(endrow)&",09,Cu 1,AE,N": response$(1)
03310   if mtype=3 and cmdkey=22 then response$(1)="N"
03320   if mtype=3 and cmdkey=21 then response$(1)="Y"
03330   if mtype=3 and cmdkey=99 then response$(1)=""
03340   if mtype=3 and response$(1)<>"Y" and response$(1)<>"N" and response$(1)<>"" then pr f "24,1,C 7,N": bell$ : goto L3230
03350   close #104: ioerr L3360
03360 L3360: return  ! Fnend
03370 L3370: ! Def Library FNWAIT(&MESSAGE$,STOPABLE)
03380 ! if stopable=1 will display "Cancel (F5)" button
03390 ! win = window number
03410   close #win: ioerr L3420
03420 L3420: open #win: "Srow=10,SCol=20,ERow=14,ECol=59,Border=Sr,Caption=<"&cap$,display,outIn 
03430   pr #win: newpage
03440   pr #win,fields "1,1,Cc 40,R,N": env$('cnam')
03450   pr #win,fields "2,1,Cc 40,R,N": "Company Number "&env$('cno')
03460   pr #win,fields "4,1,Cc 40,N": message$
03470   if rtrm$(message$)="" then pr #win,fields "4,1,Cc 40,N": "Please wait..."
03480   if stopable=0 then pr f "15,34,C 11,R,N": "Do Not Stop"
03490   if stopable=1 then pr f "15,34,C 11,B,5": "Cancel (F5)"
03500   return  ! Fnend
03510 ! Def Library FNOPENWIN(WIN,SR,SC,ER,EC,&CAP$)
03530   if sr<1 then sr=10
03540   if sc<1 then sc=20
03550   if er<1 then er=14
03560   if ec<1 then ec=59
03570   win_width=ec-sc+1
03580   close #win: ioerr L3590
03590 L3590: open #win: "SRow="&str$(sr)&",SCol="&str$(sc)&",ERow="&str$(er)&",ECol="&str$(ec)&",Border=Sr,Caption=<"&cap$,display,outIn 
03600   pr #win: newpage
03610   pr #win,fields "1,1,Cc "&str$(win_width)&",R,N": env$('cnam')(1:min(40,win_width))
03620   pr #win,fields "2,1,Cc "&str$(win_width)&",R,N": "Company Number "&env$('cno')(1:min(40,win_width))
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
04050 ERTN: fnerror(program$,err,line,act$,"xit")
04060   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
04070   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
04080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
04090 ERTN_EXEC_ACT: execute act$ : goto ERTN
04100 ! /region
04110 ! ______________________________________________________________________
04120 CALCULATEUC: ! determine quarterly wages
04122   dcy=dcq=0
04130   for j=1 to 10
04140     if dedfed(j)=2 and dedcode(j)=1 and cafiteria$="Y" then !:
            dcy=dcy+ty(j+3): dcq=dcq+tqm(j+3)
04150     if dedfed(j)=1 and dedcode(j)=1 and pension$="Y" then !:
            dcy=dcy+ty(j+3): dcq=dcq+tqm(j+3)
04160   next j
04170   m2=m2+ty(21)-dcy
04180   m1=m1+tqm(16)-dcq
04190   if ta=0 then goto L4200 else goto L4340 ! read_DEPARTMENT
04200 L4200: if m2=0 then goto L4340 ! skip if total wage =0
04210   if m1=0 then goto L4340 ! SKIP IF QUARTERLY WAGE=0
04220   p3=p3+1
04230   if m2<m(sr1) then goto L4290
04240   if m2-m1>m(sr1) then goto L4270
04250   h2=m(sr1)-(m2-m1)
04260   goto L4300
04270 L4270: h2=0
04280   goto L4300
04290 L4290: h2=m1
04300 L4300: h3=m1-h2
04310   t1=t1+m1
04320   t2=t2+h3
04330   t3=t3+h2
04340 L4340: return 
