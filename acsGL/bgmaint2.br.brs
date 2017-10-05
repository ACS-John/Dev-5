00010 ! Replace S:\acsGL\BgMaint2
00020 ! Budget File (2nd screen of budget management system; range of gl #)
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit,fnerror,fnopenprn,fncloseprn,fntos,fnlbl,fncmdkey,fnacs,fncmbbud,fncomboa,fntxt,fncmdset,fnrgl$,fnqgl,fnagl$,fnflexinit1,fnflexadd1,fnchk,fnmsgbox,fndat,fndate_mmddyy_to_ccyymmdd
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim dat$*20,bg(6),bm(13),io1$(3),g1(3),aa(2),gld$*30,ln$*132
00080   dim chdr$(23)*20,cmask$(23)*20,option2$(6)*18,item$(23)*20,holdcmask$(23)*20
00090   dim name$*30,ml$(3)*80,maxan2(11)
00100   dim gd$*50,ex$*50,ios$(22),ins$(22),sc3$(9)*22,io3$(9),fl3$(9),k$(22)
00110   dim bk$(50)*12,t1(6),t(6),s(6),oldbg(6),type$(6)*20,fkey$(12)*30
00120   dim iom$(5),scm$(5)*40,io5$(2),options$(50)*100,resp$(100)*100
00130   dim io4$(25),an1$(11),an2(11),rdate$*40,bud$*50,cap$*128
00140   dim header$*244,line$*244,uline$*244,dline$*244,dline2$*244,uline2$*244
00150   dim btnfld01$(6),dosfld01$(6),btnfld03$(3),dosfld03$(3),item$(14)*50
00160   dim fa$(6),fb$(122),gl$(40)*12,bud$*50,gln(40,3),cap$*128
00170   dim indexfile$*200
00180   dim exec$*132,prg$*20,cogl$(3)*12,pedat$*20,cch$*20,cdk$(22)
00190   dim iom2$(1),scm2$(1)*40,iom3$(2),scm3$(2)*40,btnfld02$(4),dosfld02$(4)
00200 ! ______________________________________________________________________
00210   fntop(program$,cap$="Budget Management")
00230   fndat(dat$,1)
00240   gosub GRID_SPECS
00280   for j=1 to 22: ios$(j)=str$(j+1)&",2,C 12,N" : next j
00290   type$(1)="Heading" !:
        type$(2)="Budget item" !:
        type$(3)="Subtotal" !:
        type$(4)="Total" !:
        type$(5)="Blank line" !:
        type$(6)="New page"
00300   an2(1)=40
00310   for j=2 to 7 : an2(j)=12 : next j ! defaults for column widths
00320   mat maxan2=an2
00330   an2(8)=40 : an2(9)=12 : an2(10)=12: an2(11)=12
00340   open #company=1: "Name="&env$('Q')&"\GLmstr\Company.h"&env$('cno')&",Shr",internal,input  !:
        read #company,using 'Form Pos 150,2*N 1': use_dept,use_sub !:
        close #1: 
00350   open #1: "Name="&env$('Q')&"\GLmstr\GLmstr.H"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\GLIndex.H"&env$('cno')&",Shr",internal,outin,keyed 
00360 ! format:  budget #^bud^n 2,budget name^bud$^c 50,range of g/l #^gl$(40) (1 to 2, 3 to 4, etc)
00370   open #5: "Name="&env$('Q')&"\GLmstr\BudInfo.H"&env$('cno')&",use,KFName="&env$('Q')&"\GLmstr\BudInfo_Index.H"&env$('cno')&",RecL=532,KPs=1,KLn=2",internal,outin,keyed 
00380   read #5,using 'Form N 2,C 50,40*C 12': bud,bud$,mat gl$ norec MAINTAIN_RANGE_FILE, eof MAINTAIN_RANGE_FILE
00390   open #12: "Name="&env$('Q')&"\GLmstr\BudgetInfo.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\BudIndx.h"&env$('cno')&",Use,RecL=28,KPs=1,KLn=14,Shr",internal,outin,keyed 
00400 ! ______________________________________________________________________
00410   open #2: "Name="&env$('Q')&"\GLmstr\Budget"&str$(bud)&".H"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\BgIndx"&str$(bud)&".H"&env$('cno')&",Shr",internal,outin,keyed ioerr MENU1
00420 ! ______________________________________________________________________
00450   mat ml$(2) !:
        ml$(1)="Budget Management is a separately licensed product. " !:
        ml$(2)="Contact your representative or ACS to license the product. " !:
        fnmsgbox(mat ml$,resp$,cap$,0)
00460   goto XIT
00470 L470: gosub DATE_SCREEN
00480 ! ______________________________________________________________________
00490 MENU1: ! 
00500   fntos(sn$="Budget_File") !:
        respc=0
00510   fnlbl(1,1,"Budget File #:",14,right)
00520   execute "Dir "&env$('Q')&"\GLmstr\budget*.H"&env$('cno')&" >"&env$('temp')&"\FlexWork.tmp" ioerr L530
00530 L530: j=0
00540   open #13: "Name="&env$('temp')&"\FlexWork.tmp",display,input ioerr L690
00550 L550: linput #13: ln$ eof L680
00560   ln$=uprc$(ln$)
00570   x=pos(ln$,"KILOBYTES",1)
00580   if x>0 then goto L680
00590   x=pos(ln$(1:3),"DIR",1)
00600   if x>0 or ln$(1:1)="." then goto L550 else goto L610
00610 L610: x=pos(ln$,".",1)
00620   bud=val(ln$(x-1:x-1)) conv L670
00630   bud=val(ln$(x-2:x-1)) conv L640
00640 L640: k$=lpad$(str$(bud),2) !:
        read #5,using L650,key=k$: k$,bud$,mat gl$ nokey L670
00650 L650: form c 2,c 50,40*c 12
00660   options$(j+=1)=str$(bud)&" "&bud$
00670 L670: goto L550
00680 L680: close #13: 
00690 L690: if j<=0 then j=1
00700   mat options$(j)
00710   fen$="CBud.h"&env$('cno')
00720   fncomboa(fen$,1,16,mat options$,"Select from the list of budget files. To add a new budget file, take the Add option.",40,container)
00730 ! fnCMBBUD(INDEXFILE$)
00740   if hact$="" then !:
          resp$(respc+=1)="" else !:
          resp$(respc+=1)=hact$
00750   fncmdkey("&Display",4,1,0,"Display the budget files for this budget.") !:
        fncmdkey("&Add",1,0,0,"Add a new budget file" ) !:
        fncmdkey("E&dit",2,0,0,"Edit the highlited record") !:
        fncmdkey("E&xit",5,0,1,"Returns to menu")
00760   fnacs(sn$,0,mat resp$,ckey) ! ask budget file #
00770   ad1=0
00780   hg1$=g1$=resp$(1)(1:12)
00790   bud=val(resp$(1)(1:2))
00800   if ckey=1 then ad1=1 : goto MAINTAIN_RANGE_FILE !:
        else if ckey=2 then goto MAINTAIN_RANGE_FILE else !:
          if ckey=4 then goto ASK_BEGINNING_ITEM else !:
            if ckey=5 then goto XIT !:
              goto MENU1
00810 ! ______________________________________________________________________
00820 DATE_SCREEN: ! 
00830 fd1=0101*100+val(date$(1:2))
00840 fd2=1231*100+val(date$(1:2))
00850 fntos(sn$="bgmaint2") !:
      respc=0 : right=1
00860 fnlbl(1,47," ",1,1)
00870 fnlbl(1,1,"Beginning Day Of Year:",30,1)
00880 fntxt(1,34,12,0,0,"3",0,"") !:
      resp$(respc+=1)=str$(fd1)
00890 fnlbl(2,1,"Ending Day of Year:",30,1)
00900 fntxt(2,34,12,0,0,"3",0,"") !:
      resp$(respc+=1)=str$(fd2)
00910 fncmdset(2): fnacs(sn$,0,mat resp$,ck)
00920 if ck=5 then goto MENU1
00930 fd1=val(resp$(1)) ! beginning of year
00940 fd2=val(resp$(2)) ! ending day of year
00950 return 
00960 CREATE_NEW_FILE: ! 
00970 close #2: ioerr L1020
00980 open #2: "Name="&env$('Q')&"\GLmstr\BUDGET"&str$(bud)&".H"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\BGINDX"&str$(bud)&".H"&env$('cno')&",Shr",internal,outin,keyed ioerr L1000
00990 goto L1020
01000 L1000: mat ml$(2) !:
      ml$(1)="You already have a budget file # "&str$(bud) ! " !:
      ml$(2)="Take YES to continue, else NO to retain the old file." !:
      fnmsgbox(mat ml$,resp$,cap$,52)
01010 if resp$="Yes" then goto L1020 else goto MENU1
01020 L1020: open #2: "Name="&env$('Q')&"\GLmstr\Budget"&str$(bud)&".H"&env$('cno')&",Replace,KFName="&env$('Q')&"\GLmstr\BgIndx"&str$(bud)&".H"&env$('cno')&",RecL=149,KPS=1/149,KLN=12/1",internal,outin,keyed 
01030 close #2: ioerr L1040
01040 L1040: open #2: "Name="&env$('Q')&"\GLmstr\Budget"&str$(bud)&".H"&env$('cno')&",use,KFName="&env$('Q')&"\GLmstr\BgIndx"&str$(bud)&".H"&env$('cno')&",RecL=149,KPS=1/149,KLN=12/1",internal,outin,keyed 
01050 return 
01060 READD_TOTALS: ! 
01070 ! from general ledger __________________________________________________
01080 g1$=""
01090 restore #1,search>="": nokey END1
01100 L1100: read #1,using 'Form POS 1,C 12,C 50,POS 87,PD 6.2,POS 249,13*PD 6.2': g1$,gd$,cb,mat bm eof END1
01110 for j=1 to 40 step 2
01120   if g1$>=gl$(j) and g1$<=gl$(j+1) then goto L1150
01130 next j
01140 goto L1100
01150 L1150: mat bg=(0) : ex$="" : cd$="B"
01160 ! If TI1=5 Then Goto 1170
01170 read #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1',key=g1$&cd$: g1$,mat bg,gd$,ex$,cd$ nokey L1210
01180 bg(1)=sum(bm) : bg(2)=cb : bg(3)=0 !:
      bg(5)=bg(1)+bg(4) ! NEW BUDGET = BUDGET FROM FILE + CHANGES
01190 rewrite #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1',key=g1$&cd$: g1$,mat bg,gd$,ex$,cd$ nokey L1220
01200 goto L1100
01210 L1210: bg(1)=sum(bm) : bg(2)=cb : bg(3)=0 !:
      bg(5)=bg(1)+bg(4) : gd$=gd$ : cd$="B"
01220 L1220: write #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1': g1$,mat bg,gd$,ex$,cd$
01230 goto L1100
01240 ! ______________________________________________________________________
01250 END1: ! end on g/l
01260 cd$="X": mat bg=(0): gd$=ex$="" ! write one blank line at end of file
01270 g1$="999999999999"
01280 ! Write #2,Using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1': G1$,MAT BG,GD$,EX$,CD$    why was it doing this  ??? kj
01290 close #2: 
01300 execute "Index "&env$('Q')&"\GLmstr\Budget"&str$(bud)&".H"&env$('cno')&","&env$('Q')&"\GLmstr\BGINDX"&str$(bud)&".H"&env$('cno')&",1/149,12/1,Replace,DupKeys -n"
01310 open #2: "Name="&env$('Q')&"\GLmstr\Budget"&str$(bud)&".H"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\BgIndx"&str$(bud)&".H"&env$('cno')&",Shr",internal,outin,keyed 
01320 ! holding files in gl___________________________________________________
01330 execute "DIR "&env$('Q')&"\GLmstr\GL*.H"&env$('cno')&" >"&env$('temp')&"\Work."&session$
01340 open #3: "Name="&env$('temp')&"\Work."&session$,display,input 
01350 L1350: linput #3: ln$ eof L1530
01360 ln$=uprc$(ln$)
01370 if ln$(1:2)><"GL" then goto L1350
01380 d1=val(ln$(3:8)) conv L1350
01390 if fndate_mmddyy_to_ccyymmdd(d1)<fd1 or fndate_mmddyy_to_ccyymmdd(d1)>fd2 then goto L1350
01400 open #4: "Name="&env$('Q')&"\GLmstr\"&ln$(1:8)&".H"&env$('cno'),internal,input 
01410 L1410: read #4,using L1420: g1$,d1,amt,tcde eof L1500
01420 L1420: form pos 1,c 12,n 6,pd 6.2,n 2
01430 if g1$(3:3)=" " then g1$(3:3)="0"
01440 if g1$(12:12)=" " then g1$(12:12)="0"
01450 read #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1',key=g1$&"B": g1$,mat bg,gd$,ex$,cd$ nokey L1410
01460 bg(2)=bg(2)+amt
01470 rewrite #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1',key=g1$&"B": g1$,mat bg,gd$,ex$,cd$
01480 goto L1410
01490 ! ______________________________________________________________________
01500 L1500: close #4: 
01510 goto L1350
01520 ! from unpaid invoice file _____________________________________________
01530 L1530: close #3: ioerr L1540
01540 L1540: close #4: ioerr L1550
01550 L1550: open #3: "Name="&env$('Q')&"\CLmstr\PAYTRANS.H"&env$('cno')&",Shr",internal,input,relative ioerr L1740
01560 open #4: "Name="&env$('Q')&"\CLmstr\UnPdAloc.H"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\Uaidx2.H"&env$('cno')&",Shr",internal,outin,keyed 
01570 for j=1 to lrec(3)
01580   read #3,using L1590,rec=j: vn$,iv$,d1,gde norec L1720
01590 L1590: form pos 1,c 8,c 12,pos 27,n 6,pos 96,n 1
01600   if gde>0 then goto L1720 ! ALREADY POSTED TO GL AS A/P
01610   if fndate_mmddyy_to_ccyymmdd(d1)<fd1 or fndate_mmddyy_to_ccyymmdd(d1)>fd2 then goto L1720
01620 ! Read #4,Using 1600,Rec=AA: G1$,AMT,NAA
01630   restore #4,key>=lpad$(rtrm$(vn$),8)&lpad$(rtrm$(iv$),12): nokey L1720
01640 L1640: read #4,using 'Form POS 1,C 8,C 12,c 12,PD 5.2,C 30': hvn$,hiv$,g1$,amt,gd$ eof L1720
01650   if vn$=hvn$ and iv$<>hiv$ then goto L1640
01660   if vn$<>hvn$ or iv$<>hiv$ then goto L1720
01670   form pos 21,c 12,pd 5.2,x 30,pd 3
01680   read #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1',key=g1$&"B": g1$,mat bg,gd$,ex$,cd$ nokey L1710
01690   bg(3)=bg(3)+amt
01700   rewrite #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1',key=g1$&"B": g1$,mat bg,gd$,ex$,cd$
01710 L1710: goto L1640
01720 L1720: next j
01730 ! from check history (checks not posted)____________________________
01740 L1740: close #3: ioerr L1750
01750 L1750: close #4: ioerr L1760
01760 L1760: open #3: "Name="&env$('Q')&"\CLmstr\TRMSTR.H"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\TRIDX1.H"&env$('cno')&",Shr",internal,input,keyed ioerr L1930
01770 open #4: "Name="&env$('Q')&"\CLmstr\TRALLOC.H"&env$('cno')&",Shr",internal,input,relative 
01780 L1780: read #3,using L1790: bcde,tcde,iv$,d1,pcde,scd eof L1930
01790 L1790: form pos 1,n 2,n 1,c 8,pos 12,g 6,pos 71,n 1,x 6,n 1
01800 if pcde=1 or pcde=3 then goto L1780
01810 if d1<fd1 or d1>fd2 then goto L1780
01820 key$=lpad$(str$(bcde),2)&str$(tcde)&rpad$(iv$,8) !:
      restore #4,key>=key$: nokey L1780
01830 L1830: read #4,using 'Form Pos 1,C 11,c 12,pd 5.2,pos 65,pd 3,pos 80,n 1': newkey$,g1$,amt,naa,gde
01840 if newkey$<>key$ then goto L1780
01850 read #4,using L1860,rec=aa: g1$,amt,naa,gde norec L1780
01860 L1860: form pos 12,c 12,pd 5.2,pos 65,pd 3,pos 80,n 1
01870 if gde>0 then goto L1910
01880 read #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1',key=g1$&"B": g1$,mat bg,gd$,ex$,cd$ nokey L1910
01890 if tcde=2 or tcde=3 then bg(3)=bg(3)-amt else bg(3)=bg(3)+amt
01900 rewrite #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1',key=g1$&"B": g1$,mat bg,gd$,ex$,cd$
01910 L1910: goto L1830
01920 ! from purchase order file_________________________________________
01930 L1930: close #3: ioerr L1940
01940 L1940: close #4: ioerr L1950
01950 L1950: open #3: "Name="&env$('Q')&"\POmstr\POmstr.H"&env$('cno')&",KFName="&env$('Q')&"\POmstr\POMSIDX.H"&env$('cno')&",Shr",internal,input,keyed ioerr L2080
01960 open #4: "Name="&env$('Q')&"\POmstr\POTRANS.H"&env$('cno')&",Shr",internal,input,relative 
01970 L1970: read #3,using L1980: d1,mat aa eof L2080
01980 L1980: form pos 10,n 6,pos 161,2*pd 3
01990 if fndate_mmddyy_to_ccyymmdd(d1)<fd1 or fndate_mmddyy_to_ccyymmdd(d1)>fd2 then goto L1970
02000 aa=aa(1)
02010 L2010: if aa=0 then goto L1970
02020 read #4,using L2030,rec=aa: pt1,pt2,pt3,g1$,naa
02030 L2030: form pos 52,pd 3,pd 5.2,pd 3,pos 91,c 12,pos 103,pd 3
02040 read #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1',key=g1$&"B": g1$,mat bg,gd$,ex$,cd$ nokey L2070
02050 bg(3)=bg(3)+(pt1-pt3)*pt2
02060 rewrite #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1',key=g1$&"B": g1$,mat bg,gd$,ex$,cd$
02070 L2070: aa=naa : goto L2010
02080 L2080: close #3: ioerr L2090
02090 L2090: close #4: ioerr L2100
02100 L2100: close #2: ioerr L2110
02110 L2110: open #2: "Name="&env$('Q')&"\GLmstr\Budget"&str$(bud)&".H"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\BgIndx"&str$(bud)&".H"&env$('cno')&",Shr",internal,outin,keyed 
02120 L2120: read #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1': g1$,mat bg,gd$,ex$,cd$ eof READD_SUB_TOTALS
02130 bg(5)=bg(1)+bg(4) ! update new balance for any changes
02140 rewrite #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1': g1$,mat bg,gd$,ex$,cd$
02150 goto L2120
02160 READD_SUB_TOTALS: ! 
02170 mat t=(0): mat s=(0)
02180 restore #2: 
02190 L2190: read #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1': g1$,mat bg,gd$,ex$,cd$ eof L2240
02200 if cd$="B" then mat t=t+bg: mat s=s+bg
02210 if cd$="S" then mat bg=s: mat s=(0) !:
        rewrite #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1': g1$,mat bg,gd$,ex$,cd$
02220 if cd$="T" then mat bg=t: mat t=(0) !:
        rewrite #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1': g1$,mat bg,gd$,ex$,cd$
02230 goto L2190
02240 L2240: restore #2: 
02250 goto DISPLAY_GRID ! (end of updating balances)
02260 ! ______________________________________________________________________
02270 ASK_BEGINNING_ITEM: ! 
02280 goto DISPLAY_GRID ! skip this screen
02290 fntos(sn$="bgmaint3") !:
      respc=0
02300 fnlbl(1,1,"Beginning General Ledger #:",30,1)
02310 fnqgl(1,33) !:
      resp$(1)=fnrgl$(gl$(1))
02320 fncmdkey("&Next",1,1,0,"Display budget file starting with this account. ") !:
      fncmdkey("&Back",2,0,0,"Takes you back one screen.") !:
      fncmdkey("E&xit",5,0,1,"Returns to main menu")
02330 fnacs(sn$,0,mat resp$,ckey) ! ask general ledger #
02340 if ckey=5 then goto MENU1
02350 g1$=startgl$=fnagl$(resp$(1))
02360 restore #2,key>=g1$&"B": nokey L2370
02370 L2370: if ckey=2 then goto MENU1
02380 bk=0
02390 DISPLAY_GRID: ! 
02400 restore #2: 
02410 x=13
02420 if trim$(startgl$)<>"" then restore #2,key>=startgl$&"B": nokey L2430
02430 L2430: fntos(sn$="Bgmaint4") !:
      respc=0
02440 fnlbl(1,1,bud$,50,right) ! move cmdkeys down
02450 frame=0
02460 fnflexinit1('bgmaintgrid',2,1,20,95,mat chdr$,mat cmask$,1,0)
02470 READ_BUDGET_GRID: ! read budget items
02480 read #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1': g1$,mat bg,gd$,ex$,cd$ eof EO_BUDGET_GRID norec L2650
02490 if needactual=0 and needbudget=0 then goto L2610
02500 read #1,using "form pos 13,c 30,pos 237,pd 6.2",key=g1$: name$,prioryear nokey L2610
02510 restore #12,search>=g1$&"  ": nokey L2610 eof L2610
02520 L2520: read #12,using "form pos 1,c 12,c 2,2*pd 6.2": acno$,oldyr$,cb,oldbud eof L2610
02530 if acno$<>g1$ then goto L2610
02540 for j=1 to 5
02550   if val(oldyr$)=year(j) and needactual=1 and needbudget=1 then item$(x+j*2-1)=str$(cb) ! balance side of showing both
02560   if val(oldyr$)=year(j) and needbudget=1 and needactual=1 then item$(x+j*2)=str$(oldbud) ! budget side of showing both
02570   if val(oldyr$)=year(j) and needbudget=1 and needactual=0 then item$(x+j)=str$(oldbud) ! only budget
02580   if val(oldyr$)=year(j) and needbudget=0 and needactual=1 then item$(x+j)=str$(oldbud) ! only actual
02590 next j
02600 goto L2520
02610 L2610: if cd$="A" or cd$="X" then mat cmask$=(""): cmask$(1)="30": : mat item$=(""): item$(1)=str$(rec(2)): item$(2)=gd$: item$(10)=g1$: goto L2640
02620 mat cmask$=holdcmask$
02630 item$(1)=str$(rec(2)) !:
      item$(10)=g1$ !:
      item$(3)=str$(bg(1)): item$(4)=str$(bg(2)) !:
      item$(5)=str$(bg(3)) : item$(6)=str$(bg(4)) : item$(7)=str$(bg(1)-bg(2)-bg(3)-bg(4)) !:
      item$(8)=str$(bg(5)) !:
      item$(9)=str$(bg(6)) : item$(2)=gd$(1:30) : item$(11)=ex$ !:
      item$(12)=cd$ !:
      item$(13)=str$(prioryear)
02640 L2640: fnflexadd1(mat item$)
02650 L2650: goto READ_BUDGET_GRID
02660 EO_BUDGET_GRID: ! 
02670 fnlbl(22,40," ",mylen,right) ! move cmdkeys down
02680 fncmdkey("&Add",1,0,0,"Allows you to add new lines to the budget file.")
02690 fncmdkey("&Edit",2,1,0,"Highlight any record and press Enter or click Edit to change any existing budget record.")
02700 fncmdkey("&Calculate New Balance",6,0,0,"Will re-calculate new balances if any changes have been made to general ledger or checkbook.")
02710 fncmdkey("&Pull More History",9,0,0,"Pull expenditures and budgets from prior years.")
02720 fncmdkey("&Listing",3,0,0,"Prints listings of the budget")
02730 fncmdkey("&Delete",7,0,0,"Deletes this line item")
02740 fncmdkey("&Back",8,0,0,"Reselect starting budget item to display.")
02750 fncmdkey("E&xit",5,0,1,"Exits back to main budget screen.")
02760 fnacs(sn$,0,mat resp$,ck) ! GRID
02770 add=edit=0
02780 if ck=5 then goto MENU1
02790 if ck=2 then edit=1
02800 if ck=3 then goto PRNT1 ! pr listings of unpaid invoice file
02810 if ck=1 then add=1: insrt=1: jb1=1 !:
        g1$=k$(max(1,curfld-1)): mat bg=(0): gd$=ex$=cd$="" !:
        mat resp$=(""): goto MAINTAIN_NONB_RECORDS
02820 if ck=2 then !:
        rec2=val(resp$(1)) : read #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1',rec=rec2,release: g1$,mat bg,gd$,ex$,cd$ !:
        mat resp$=("") !:
        holdkey$=g1$&cd$ else !:
        holdkey$=''
02830 if ck=2 then resp$(1)=g1$: resp$(2)=str$(bg(1)): resp$(3)=str$(bg(2)) !:
        resp$(4)=str$(bg(3)): resp$(5)=str$(bg(4)) : resp$(6)=str$(bg(5)) !:
        resp$(7)=gd$: resp$(8)=ex$ !:
        resp$(9)=cd$ !:
        goto MAINTAIN_LINE_ITEM
02840 if ck=6 then goto INCLUDE_CHANGES
02850 if ck=7 then goto L2860 else goto L2890
02860 L2860: mat ml$(2) !:
      ml$(1)="You have chosen to delete the highlighted budget item." !:
      ml$(2)="Take YES to continue, else NO to retain the record." !:
      fnmsgbox(mat ml$,resp$,cap$,52)
02870 if resp$="Yes" then goto L2880 else goto DISPLAY_GRID
02880 L2880: delete #2,rec=val(resp$(1)): !:
      restore #2: !:
      goto DISPLAY_GRID
02890 L2890: if ck=8 then goto MENU1
02900 if ck=9 then goto ASK_ABOUT_HISTORY
02910 goto DISPLAY_GRID
02920 MAINTAIN_NONB_RECORDS: ! 
02930 if add=1 then g1$=gd$=cd$=""
02940 fntos(sn$="Add_line_item") !:
      mylen=23: mypos=mylen+3 : right=1: rc=0
02950 if use_dept =1 then let fnlbl(1,26,"Fund #",6,2)
02960 if use_sub =1 then let fnlbl(1,40,"Sub #",6,2)
02970 fnlbl(2,1,"General Ledger Number:",mylen,right)
02980 if use_dept=1 then let fntxt(2,26,3,0,right,"30",0,"Enter the fund portion of the general ledger number.",0 ) !:
        resp$(rc+=1)=g1$(1:3)
02990 fntxt(2,31,6,0,right,"30",0,"Enter the main part of the general ledger number.",0 ) !:
      resp$(rc+=1)=g1$(4:9)
03000 if use_sub=1 then let fntxt(2,40,3,0,right,"30",0,"Enter the sub portion of the general ledger number.",0 ) !:
        resp$(rc+=1)=g1$(10:12)
03010 fnlbl(3,1,"Description:",mylen,right)
03020 fntxt(3,mypos,50,0,left,"",0,"Enter the account description.",0 ) !:
      resp$(rc+=1)=gd$
03030 fnlbl(4,1,"Type of Entry:",mylen,right)
03040 option2$(1)="A "&type$(1) !:
      option2$(2)="B "&type$(2) !:
      option2$(3)="S "&type$(3) : option2$(4)="T "&type$(4): !:
      option2$(5)="X "&type$(5) : option2$(6)="Z "&type$(6)
03050 fncomboa("Types",4,mypos,mat option2$,"Select the type of entry.",20,container)
03060 resp$(rc+=1)=cd$
03070 fncmdset(2)
03080 fnacs(sn$,0,mat resp$,ckey)
03090 if ckey=5 then goto DISPLAY_GRID
03100 dno=ano=sno=0
03110 if use_dept=1 then dno=val(resp$(1)) : ano=val(resp$(2))
03120 if use_dept=0 then ano=val(resp$(1))
03130 if use_dept=1 and use_sub=1 then sno=val(resp$(3))
03140 if use_dept=0 and use_sub=1 then sno=val(resp$(2))
03150 if use_dept=1 and use_sub=1 then gd$=resp$(4): cd$=resp$(5)(1:1)
03160 if use_dept=0 and use_sub=1 then gd$=resp$(3): cd$=resp$(4)(1:1)
03170 if use_dept=0 and use_sub=0 then gd$=resp$(2): cd$=resp$(3)(1:1)
03180 if use_dept=1 and use_sub=0 then gd$=resp$(3): cd$=resp$(4)(1:1)
03190 key$=cnvrt$("N 3",dno)&cnvrt$("N 6",ano)&cnvrt$("N 3",sno)&"B"
03200 read #2,using 'Form POS 1,c 13',key=key$: oldkey$ nokey L3240
03210 MSGBOX1: ! 
03220 mat ml$(3) !:
      ml$(1)="General ledger account # "&key$&" already " !:
      ml$(2)="exists. Take OK to add a different account." !:
      ml$(3)="Take Cancel to return to main screen." !:
      fnmsgbox(mat ml$,resp$,cap$,49)
03230 if resp$="OK" then goto MAINTAIN_NONB_RECORDS else goto MENU1
03240 L3240: if add=1 then goto WRITE_NEW_RECORD else goto REWRITE_EXISTING_RECORD
03250 WRITE_NEW_RECORD: ! 
03260 mat bg=(0) : ex$="": edit=1 !:
      g1$=key$(1:12)
03270 write_new_record
03280 write #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1': g1$,mat bg,gd$,ex$,cd$
03290 if cd$="B" then goto MAINTAIN_LINE_ITEM else goto DISPLAY_GRID
03300 REWRITE_EXISTING_RECORD: ! 
03310 rewrite #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1',rec=rec2: g1$,mat bg,gd$,ex$,cd$
03320 goto DISPLAY_GRID
03330 MAINTAIN_LINE_ITEM: ! 
03340 if cd$="B" then goto L3350 else goto MAINTAIN_NONB_RECORDS
03350 L3350: holdg1$=g1$: holdcd$=cd$
03360 fntos(sn$="Budget_Edit") !:
      respc=0: mylen=20: mypos=mylen+3
03370 fnlbl(1,1,"Account #:",mylen,right)
03380 fnqgl(1,mypos) !:
      resp$(respc+=1)=fnrgl$(g1$)
03390 fnlbl(2,1,"Budget:",mylen,right)
03400 fntxt(2,mypos,12,0,0,"10",0,"Approved budget at the beginning of the year") !:
      resp$(respc+=1)=str$(bg(1))
03410 fnlbl(3,1,"Actual Amount:",mylen,right)
03420 fntxt(3,mypos,12,0,0,"10",0,"Actual expenditures or receiptsfor the year.") !:
      resp$(respc+=1)=str$(bg(2))
03430 fnlbl(4,1,"Unpaid Expenses:",mylen,right)
03440 fntxt(4,mypos,12,0,0,"10",0,"Accounts payable, etc.") !:
      resp$(respc+=1)=str$(bg(3))
03450 fnlbl(5,1,"Changes:",mylen,right)
03460 fntxt(5,mypos,12,0,0,"10",0,"Changes for the year.") !:
      resp$(respc+=1)=str$(bg(4))
03470 ! If TI3=2 Then rEMAINING= BG(1)-BG(2)-BG(3) Else rEMAINING=BG(1)-BG(2)-BG(3)+BG(4)
03480 ! fnLBL(6,1,"Remaining:",MYLEN,RIGHT)
03490 ! fnTXT(6,MYPOS,12,0,0,"10",0,"Balance remaining on the budget.") !:
      ! rESP$(RESPC+=1)=STR$(REMAINING)
03500 fnlbl(6,1,"New Budget:",mylen,right)
03510 fntxt(6,mypos,12,0,0,"10",0,"New Budget for this year.") !:
      resp$(respc+=1)=str$(bg(5))
03520 fnlbl(8,1,"Description:",mylen,right)
03530 fntxt(8,mypos,50,0,0,"",0,"Description of Line Item.") !:
      resp$(respc+=1)=gd$
03540 fnlbl(10,1,"Reason for Change:",mylen,right)
03550 fntxt(10,mypos,50,0,0,"",0,"Brief reason for change in budget.") !:
      resp$(respc+=1)=ex$
03560 fnlbl(12,1,"Type of Entry:",mylen,right)
03570 if cd$="A" or trim$(cd$)="" then option2$(1)="A "&type$(1) !:
        option2$(2)="B "&type$(2) !:
        option2$(3)="S "&type$(3) : option2$(4)="T "&type$(4): !:
        option2$(5)="X "&type$(5) : option2$(6)="Z "&type$(6)
03580 if cd$="B" then option2$(1)="B "&type$(2) !:
        option2$(2)="A "&type$(1) !:
        option2$(3)="S "&type$(3) : option2$(4)="T "&type$(4): !:
        option2$(5)="X "&type$(5) : option2$(6)="Z "&type$(6)
03590 if cd$="S" then option2$(1)="S "&type$(3) !:
        option2$(2)="A "&type$(1) !:
        option2$(3)="B "&type$(2) : option2$(4)="T "&type$(4): !:
        option2$(5)="X "&type$(5) : option2$(6)="z "&type$(6)
03600 if cd$="T" then option2$(4)="T "&type$(4) !:
        option2$(2)="A "&type$(1) !:
        option2$(3)="B "&type$(2) : option2$(4)="S "&type$(3): !:
        option2$(5)="X "&type$(5) : option2$(6)="Z "&type$(6)
03610 if cd$="X" then option2$(1)="X "&type$(5) !:
        option2$(2)="A "&type$(1) !:
        option2$(3)="B "&type$(2) : option2$(4)="S "&type$(3): !:
        option2$(5)="T "&type$(4) : option2$(6)="Z "&type$(6)
03620 if cd$="Z" then option2$(1)="Z "&type$(6) : option2$(2)="A "&type$(1) !:
        option2$(3)="B "&type$(2) : option2$(4)="S "&type$(3): !:
        option2$(5)="T "&type$(4) : option2$(6)="X "&type$(5)
03630 resp$(respc+=1)=option2$(1)
03640 fncomboa("Types",12,mypos,mat option2$,"Select the type of entry.",20,container)
03650 fncmdkey("&Save",1,1,0,"Save any changes." ) !:
      fncmdkey("E&xit",5,0,1,"Exit without saving any changes.")
03660 fnacs(sn$,0,mat resp$,ck) ! EDIT SCREEN
03670 if ckey=5 then goto MENU1
03680 g1$=fnagl$(resp$(1))
03690 bg(1)=val(resp$(2))
03700 bg(2)=val(resp$(3))
03710 bg(3)=val(resp$(4))
03720 bg(4)=val(resp$(5))
03730 bg(5)=val(resp$(6))
03740 gd$=resp$(7)
03750 ex$=resp$(8)
03760 for j=1 to 6
03770   if resp$(9)(1:1)=option2$(j)(1:1) then cd$=option2$(j)(1:1)
03780 next j
03790 remaining=bg(1)-bg(2)-bg(3)-bg(4) ! remaining balance
03800 bg(5)=bg(1)+bg(4) ! make new budget equal the old budget plus changes
03810 if holdg1$<>g1$ then read #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1',key=g1$&cd$: oldg1$ nokey L3850
03820 goto L3850
03830 mat ml$(2) !:
      ml$(1)="You are attempting to change the record number to  an existing" !:
      ml$(2)="record number.  Take OK to change to a different number. " !:
      fnmsgbox(mat ml$,resp$,cap$,0)
03840 goto MAINTAIN_LINE_ITEM
03850 L3850: if add=1 then goto L3880
03860 rewrite #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1',rec=rec2: g1$,mat bg,gd$,ex$,cd$
03870 goto L3900
03880 L3880: write #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1': g1$,mat bg,gd$,ex$,cd$
03890 add=0
03900 L3900: restore #2: 
03910 goto DISPLAY_GRID
03920 ! ______________________________________________________________________
03930 PRNT1: ! John's pr Routine ... yeah, i made this mess
03940 header$="" : opr=255 : ps$="############" !:
      screen=cp=page=bob1=bob2=bob3=bob4=lyne=0
03950 uline2$=rpt$("_",244) : dline2$=rpt$("=",244)
03960 fntos(sn$="bgprint") !:
      respc=0 : right=1 : mylen=25 : mypos=mylen+3
03970 fnlbl(lyne+=1,1,"Check the columns your want prirted",38,1)
03980 fnchk(lyne+=2,mypos,"GL Description:",1) !:
      resp$(respc+=1)="False"
03990 fntxt(lyne,mypos+5,2,0,0,"30",0,"Maximum column width can not exceed the defaults that are displayed.") !:
      resp$(respc+=1)=str$(an2(lyne-2))
04000 fnchk(lyne+=1,mypos,"Budget Amount:",1) !:
      resp$(respc+=1)="False"
04010 fntxt(lyne,mypos+5,2,0,0,"30",0,"Maximum column width can not exceed the defaults that are displayed.") !:
      resp$(respc+=1)=str$(an2(lyne-2))
04020 fnchk(lyne+=1,mypos,"Paid / Received:",1) !:
      resp$(respc+=1)="False"
04030 fntxt(lyne,mypos+5,2,0,0,"30",0,"Maximum column width can not exceed the defaults that are displayed.") !:
      resp$(respc+=1)=str$(an2(lyne-2))
04040 fnchk(lyne+=1,mypos,"Unpaid Expenses:",1) !:
      resp$(respc+=1)="False"
04050 fntxt(lyne,mypos+5,2,0,0,"30",0,"Maximum column width can not exceed the defaults that are displayed.") !:
      resp$(respc+=1)=str$(an2(lyne-2))
04060 fnchk(lyne+=1,mypos,"Changed Amount:",1) !:
      resp$(respc+=1)="False"
04070 fntxt(lyne,mypos+5,2,0,0,"30",0,"Maximum column width can not exceed the defaults that are displayed.") !:
      resp$(respc+=1)=str$(an2(lyne-2))
04080 fnchk(lyne+=1,mypos,"New Budget:",1) !:
      resp$(respc+=1)="False"
04090 fntxt(lyne,mypos+5,2,0,0,"30",0,"Maximum column width can not exceed the defaults that are displayed.") !:
      resp$(respc+=1)=str$(an2(lyne-2))
04100 fnchk(lyne+=1,mypos,"Next Years Budget:",1) !:
      resp$(respc+=1)="False"
04110 fntxt(lyne,mypos+5,2,0,0,"30",0,"Maximum column width can not exceed the defaults that are displayed.") !:
      resp$(respc+=1)=str$(an2(lyne-2))
04120 fnchk(lyne+=1,mypos,"Reason for Change:",1) !:
      resp$(respc+=1)="False"
04130 fntxt(lyne,mypos+5,2,0,0,"30",0,"Maximum column width can not exceed the defaults that are displayed.") !:
      resp$(respc+=1)=str$(an2(lyne-2))
04140 fnchk(lyne+=1,mypos,"Budget Remaining:",1) !:
      resp$(respc+=1)="False"
04150 fntxt(lyne,mypos+5,2,0,0,"30",0,"Maximum column width can not exceed the defaults that are displayed.") !:
      resp$(respc+=1)=str$(an2(lyne-2))
04160 fnchk(lyne+=1,mypos,"General Ledger Number:",1) !:
      resp$(respc+=1)="False"
04170 fntxt(lyne,mypos+5,2,0,0,"30",0,"Maximum column width can not exceed the defaults that are displayed.") !:
      resp$(respc+=1)=str$(an2(lyne-2))
04180 fnchk(lyne+=1,mypos,"% of Budget Used:",1) !:
      resp$(respc+=1)="False"
04190 fntxt(lyne,mypos+5,2,0,0,"30",0,"Maximum column width can not exceed the defaults that are displayed.") !:
      resp$(respc+=1)=str$(an2(lyne-2))
04200 fnlbl(lyne+=1,1,"Report Heading Date:",mylen,1)
04210 fntxt(lyne,mypos,30,0,0,"",0,"Date you want printed in heading of the report.") !:
      resp$(respc+=1)=dat$
04220 fncmdkey("&Next",1,1,0,"Display ") !:
      fncmdkey("E&xit",5,0,1,"Returns to main menu")
04230 fnacs(sn$,0,mat resp$,ck) ! pr setup
04240 if ck=5 then goto DISPLAY_GRID
04250 if resp$(1)="True" then an1$(1)="Y" !:
        an2(1)=val(resp$(2))
04260 if resp$(3)="True" then an1$(2)="Y" !:
        an2(2)=val(resp$(4))
04262 if resp$(5)="True" then an1$(3)="Y" !:
        an2(3)=val(resp$(6))
04264 if resp$(7)="True" then an1$(4)="Y" !:
        an2(4)=val(resp$(8))
04270 if resp$(9)="True" then an1$(5)="Y" !:
        an2(5)=val(resp$(10))
04280 if resp$(11)="True" then an1$(6)="Y" !:
        an2(6)=val(resp$(12))
04290 if resp$(13)="True" then an1$(7)="Y" !:
        an2(7)=val(resp$(14))
04300 if resp$(15)="True" then an1$(8)="Y" !:
        an2(8)=val(resp$(16))
04310 if resp$(17)="True" then an1$(9)="Y" !:
        an2(9)=val(resp$(18))
04320 if resp$(19)="True" then an1$(10)="Y" !:
        an2(10)=val(resp$(20))
04330 if resp$(21)="True" then an1$(11)="Y" !:
        an2(11)=val(resp$(22))
04340 rdate$=resp$(23)
04350 for j=1 to 11
04360   if an2(1)>maxan2(j) then goto L4380 else goto L4390
04370 next j
04380 L4380: mat ml$(2) !:
      ml$(1)="You have selected a column width longer than the maximum allowed." !:
      ml$(2)="Select a smaller width." !:
      fnmsgbox(mat ml$,resp$,cap$,0) !:
      goto PRNT1
04390 L4390: fnopenprn
04400 restore #2,search>="": nokey MENU1 ioerr L4420
04410 goto L4430
04420 L4420: open #2: "Name="&env$('Q')&"\GLmstr\BUDGET"&str$(bud)&".H"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\BGINDX"&str$(bud)&".H"&env$('cno')&",Shr",internal,outin,keyed ioerr MENU1
04430 L4430: if an1$(1)<>"Y" then goto L4470
04440 header$="GL Description                            "(1:an2(1))&"  "
04450 uline$="                                          "(1:an2(1)+2)
04460 dline$="                                          "(1:an2(1)+2)
04470 L4470: if an1$(2)="Y" then header$=header$&"  Budget Amt  "(1:an2(2))&"  "
04480 if an1$(3)="Y" then header$=header$&" Expenses Pd  "(1:an2(3))&"  "
04490 if an1$(4)="Y" then header$=header$&" Exp. Unpaid  "(1:an2(4))&"  "
04500 if an1$(5)="Y" then header$=header$&" Changed Amt  "(1:an2(5))&"  "
04510 if an1$(6)="Y" then header$=header$&"  New Budget"(1:an2(6))&"  "
04520 if an1$(7)="Y" then header$=header$&"Nxt Yrs Bdgt"(1:an2(7))&"  "
04530 if an1$(8)="Y" then header$=header$&"Reason for Change                       "(1:an2(8))&"  "
04540 if an1$(9)="Y" then header$=header$&" Bdgt Remain  "(1:an2(9))&"  "
04550 if an1$(10)="Y" then header$=header$&"   GL Number  "(1:an2(10))&"  "
04560 if an1$(11)="Y" then header$=header$&" % of Budget "(1:an2(11))&"  "
04570 for j=2 to 11
04580   if j=8 and an1$(8)="Y" then dline$=dline$&rpt$(" ",42)(1:an2(8)+2) : uline$=uline$&rpt$(" ",42)(1:an2(8)+2) : goto L4620
04590   if j=10 and an1$(10)="Y" then dline$=dline$&"            "(1:an2(10)+2) : uline$=uline$&"            "(1:an2(10)+2) : goto L4620
04600   if an1$(j)="Y" then dline$=dline$&dline2$(1:an2(j))&"  "
04610   if an1$(j)="Y" then uline$=uline$&uline2$(1:an2(j))&"  "
04620 L4620: next j
04630 lhdr=len(header$)
04640 bob1=lhdr-11
04650 bob2=int((lhdr-len(rtrm$(env$('cnam'))))/2)
04660 bob3=int((lhdr-len(rtrm$(rdate$)))/2)
04670 bob4=int((lhdr-len(rtrm$(bud$)))/2)
04680 restore #2,search>="": nokey MENU1 ioerr L4420
04690 gosub HDR
04700 L4700: read #2,using 'Form POS 1,C 12,6*PD 6.2,2*C 50,C 1': g1$,mat bg,gd$,ex$,cd$ eof L4970
04710 L4710: form pos 1,c lhdr,skip 1,pos 1,c lhdr,skip 1
04720 line$=""
04730 if an1$(1)="Y" then line$=line$&gd$(1:an2(1))&"  "
04740 for j=2 to 7
04750   if an1$(j)="Y" and len(ltrm$(cnvrt$("N 12.2",(bg(j-1)))))>an2(j) then line$=line$&ps$(13-an2(j):12)&"  ": goto L4770
04760   if an1$(j)="Y" then line$=line$&cnvrt$("N 12.2",bg(j-1))(13-an2(j):12)&"  "
04770 L4770: next j
04780 if an1$(8)="Y" then line$=line$&ex$(1:an2(8))&"  "
04790 if ti3=2 then goto L4830
04800 if an1$(9)="Y" and len(ltrm$(cnvrt$("N 12.2",bg(1)-bg(2)-bg(3)+bg(4))))>an2(9) then line$=line$&ps$(13-an2(9):12)&"  ": goto L4850
04810 if an1$(9)="Y" then line$=line$&cnvrt$("N 12.2",bg(1)-bg(2)-bg(3)+bg(4))(13-an2(9):12)&"  "
04820 goto L4850
04830 L4830: if an1$(9)="Y" and len(ltrm$(cnvrt$("N 12.2",bg(1)-bg(2)-bg(3))))>an2(9) then line$=line$&ps$(13-an2(9):12)&"  ": goto L4850
04840 if an1$(9)="Y" then line$=line$&cnvrt$("N 12.2",bg(1)-bg(2)-bg(3))(13-an2(9):12)&"  "
04850 L4850: if an1$(10)="Y" and cd$<>"T" and cd$<>"H" then line$=line$&g1$(1:an2(10)) ! THIS WONT WORK IF YOU ADD ANYTHING AFTER IT!!!
04860 if an1$(10)="Y" and cd$="T" then line$=line$&"                                        "(1:an2(10))
04870 if bg(1)<>0 then pused=(bg(2)+bg(3))/bg(1) else pused=0
04880 if an1$(11)="Y" then line$=line$&cnvrt$("N 12",pused*100)(13-an2(11):12)&"  "
04890 bobtom=len(line$)
04900 if cd$="A" and an1$(1)="Y" then pr #255,using L5120: gd$ pageoflow NWPGE
04910 if cd$="B" then pr #255,using L5130: line$(1:len(line$)) pageoflow NWPGE
04920 if cd$="S" then pr #255,using L5140: uline$(1:len(line$)),line$,sline$(1:len(line$)) pageoflow NWPGE
04930 if cd$="T" then pr #255,using L5150: uline$(1:len(line$)),line$,dline$(1:len(line$)) pageoflow NWPGE
04940 if cd$="X" then pr #255,using L5120: "" pageoflow NWPGE
04950 if cd$="Z" then pr #255: newpage : gosub HDR
04960 goto L4700
04970 L4970: fncloseprn
04980 goto DISPLAY_GRID
04990 ! ______________________________________________________________________
05000 NWPGE: ! 
05010 pr #255: newpage
05020 gosub HDR
05030 continue 
05040 ! ______________________________________________________________________
05050 HDR: ! 
05060 page=page+1
05070 pr #255,using L5080: "Page: ",page,env$('cnam'),rdate$,bud$
05080 L5080: form pos bob1,c 6,n 4,skip 1,pos bob2,c 40,skip 1,pos bob3,c 40,skip 1,pos bob4,c 50,skip 2
05090 pr #255,using L4710: header$,uline2$(1:lhdr-2)
05100 return 
05110 ! ______________________________________________________________________
05120 L5120: form pos 1,c 80,skip 1
05130 L5130: form pos 1,c bobtom,skip 1
05140 L5140: form pos 1,c bobtom,skip 1,pos 1,c bobtom,skip 1,pos 1,c bobtom,skip 1
05150 L5150: form pos 1,c bobtom,skip 1,pos 1,c bobtom,skip 1,pos 1,c bobtom,skip 2
05160 ! ______________________________________________________________________
05170 MAINTAIN_RANGE_FILE: ! 
05180 if ad1=1 then bud=0: bud$="": mat gl$=("")
05190 fntos(sn$="bgmaintrange") !:
      respc=0 : mylen=20 : mypos=20: mypos2=60: lyne=3
05200 fnlbl(1,1,"Budget #:",mylen,right)
05210 fntxt(1,mylen+3,2,0,0,"30",0,"") !:
      resp$(respc+=1)=str$(bud)
05220 fnlbl(2,1,"Description:",14,right)
05230 fntxt(2,mylen+3,50,0,0,"",0,"Enter your budget file name. Enter the range of general ledger numbers below to be included in this budget.") !:
      resp$(respc+=1)=bud$
05240 fnlbl(3,30,"Range From:",mylen,0)
05250 fnlbl(3,70,"Range To:",mylen,0)
05260 for j=1 to 40 step 2
05270   fnqgl(lyne+=1,mypos,0,2) !:
        resp$(respc+=1)=fnrgl$(gl$(j))
05280   fnqgl(lyne,mypos2,0,2) !:
        resp$(respc+=1)=fnrgl$(gl$(j+1))
05290 next j
05300 fncmdkey("&Complete",1,1,0,"Saves the changes and and builds the new file from the general ledger.") !:
      !:
      fncmdkey("&Delete",6,0,1,"Deletes this complete budget file.") !:
      fncmdkey("&Cancel",5,0,1,"Returns to main budget file menu.")
05310 fnacs(sn$,0,mat resp$,ck) ! gl breakdown screen
05320 if ck=5 then goto MENU1
05330 bud=val(resp$(1))
05340 bud$=resp$(2)
05350 k$=lpad$(str$(bud),2)
05360 for j=1 to 40
05370   gl$(j)=fnagl$(resp$(j+2))
05380 next j
05390 if add=1 then goto L5470
05400 if ck=6 then goto L5410 else goto L5450
05410 L5410: mat ml$(2) !:
      ml$(1)="You have chosen to delete the budget file. Click Yes " !:
      ml$(2)="to continue, else No to retain the file." !:
      fnmsgbox(mat ml$,resp$,cap$,52)
05420 if resp$="Yes" then goto L5430 else goto L5510
05430 L5430: delete #5,key=lpad$(str$(bud),2): 
05440 goto L5510
05450 L5450: rewrite #5,using L5490,key=k$: bud,bud$,mat gl$ nokey L5470
05460 goto L5490
05470 L5470: gosub CREATE_NEW_FILE
05480 write #5,using L5490: bud,bud$,mat gl$
05490 L5490: form pos 1,n 2,c 50,40*c 12
05500 ! mat GL$=("  0     0  0")
05510 L5510: goto READD_TOTALS
05520 ! ______________________________________________________________________
05530 XIT: fnxit
05540 ! ______________________________________________________________________
05550 INCLUDE_CHANGES: ! 
05560 fntos(sn$="Include_Changes") !:
      respc=0: mylen=40: mypos=mylen+3 : lyne=0
05570 fnchk(lyne+=1,mypos,"Include Changes in Remaining Balance:",1) !:
      resp$(respc+=1)="True"
05580 fncmdkey("&Next",1,1,0,"Continue with re-calculations. ") !:
      fncmdkey("E&xit",5,0,1,"Returns to main menu")
05590 fnacs(sn$,0,mat resp$,ck) ! include changes if remaining balance
05600 if ck=5 then goto MENU1
05610 if resp$(1)="True" then ti3=1 else ti3=2
05620 chg=1
05630 goto READD_TOTALS
05640 ! ______________________________________________________________________
05650 ASK_ABOUT_HISTORY: ! 
05660 fntos(sn$="Ask_about") !:
      respc=0: mylen=40: mypos=mylen+3 : lyne=0
05670 fnchk(lyne+=1,mypos,"Include Actual Receitps and Expenditures:",1) !:
      resp$(respc+=1)="True"
05680 fnchk(lyne+=1,mypos,"Include Budget Amounts:",1) !:
      resp$(respc+=1)="True"
05690 fnlbl(lyne+=1,1,"Years to Review:",mylen,1)
05700 fntxt(lyne,mypos,2,0,0,"30",0,"Year code in YY format.") !:
      resp$(respc+=1)=""
05710 for j=1 to 4
05720   fntxt(lyne+=1,mypos,2,0,0,"30",0,"Year code in YY format.") !:
        resp$(respc+=1)=""
05730 next j
05740 fncmdkey("&Next",1,1,0,"Display ") !:
      fncmdkey("E&xit",5,0,1,"Returns to main menu")
05750 fnacs(sn$,0,mat resp$,ck) ! ask prior years
05760 if ck=5 then goto MENU1
05770 if resp$(1)="True" then needactual=1 else needactual=0
05780 if resp$(2)="True" then needbudget=1 else needbudget=0
05790 totalextra=0
05800 for j=1 to 5
05810   year(j)=val(resp$(j+2))
05820   if year(j)>0 and needactual=1 then totalextra+=1
05830   if year(j)>0 and needbudget=1 then totalextra+=1
05840 next j
05850 ! Gosub GRID_SPECS
05860 goto DISPLAY_GRID
05870 GRID_SPECS: ! 
05880 mat chdr$(13+totalextra) : mat cmask$(13+totalextra) : mat item$(13+totalextra): mat holdcmask$(13+totalextra) !:
      chdr$(1)="Rec" : chdr$(2)="Description" : chdr$(3)="Budget" !:
      chdr$(4)="Actual" : chdr$(5)="Unpaid" !:
      chdr$(6)="Changes" !:
      chdr$(7)="Remaining" : chdr$(8)="New Budget" !:
      chdr$(9)="Next Years Budget" !:
      chdr$(10)="GL #"
05890 chdr$(11)="Reason" !:
      chdr$(12)="TOE" !:
      chdr$(13)="Prior Amount"
05900 cmask$(1)='' : cmask$(2)='10' : cmask$(3)="10" !:
      cmask$(4)='10' !:
      cmask$(5)='10' : cmask$(6)='10': cmask$(7)='10' !:
      cmask$(8)='10': cmask$(9)='10' : cmask$(10)='' !:
      cmask$(11)='': cmask$(12)='' : cmask$(13)='10'
05910 x=13 ! add extra columns for old history
05920 for j=1 to 5
05930   if needactual=1 and year(j)>0 then chdr$(x+=1)="Amount-"&cnvrt$("pic(##)",year(j)) !:
          cmask$(x)="10"
05940   if needbudget=1 and year(j)>0 then chdr$(x+=1)="Budget-"&cnvrt$("pic(##)",year(j)) !:
          cmask$(x)="10"
05950 next j
05960 x=0
05970 mat holdcmask$=cmask$
05980 return 
05990 ! <Updateable Region: ERTN>
06000 ERTN: fnerror(program$,err,line,act$,"xit")
06010 if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
06020 execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
06030 pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
06040 ERTN_EXEC_ACT: execute act$ : goto ERTN
06050 ! /region
06060 ! ______________________________________________________________________
