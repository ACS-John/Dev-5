00020 ! ______________________________________________________________________
00030   library 'S:\Core\Library': fnopenprn,fncloseprn,fntop,fncno,fnxit,fncreg_read,fncreg_write
00040   library "S:\acsTM\Print_Invoice": fnprint_invoice
00060 ! ______________________________________________________________________
00070 ! 
00080   on error goto ERTN
00090   fntop(program$,cap$="Print Invoice")
00100   fncno(cno,cnam$)
00102   dim fl1$(8),io1$(60),scrid$(4)*80,inp(3),iv$*12,a1$*30
00104   dim pt(4),fl2$(8),scr2$(4),ot2$(4),cnam$*40,nam$*25,bk$(20)*30
00106   dim cde$(30)*6,ct(30),sc(30),id$(30)*55,da(30),gl$(30)*12,gl(3)
00108   dim a$(3)*30,cdk$*6,des$*60,bc$(3)*18
00110   bc$(1)="PARTIAL BILL"
00112   bc$(2)="FINAL BILL"
00114   bc$(3)="WRITE OFF"
00120 ! 
00220   fncreg_read('Last Invoice Number',tmp$) : iv1=val(tmp$)
00360 ! 
00410   fl1$(5)="1,10,c 60,h,n"
00420   fl1$(6)="2,10,c 60,h,n"
00430   fl1$(7)="9,1,c 80,h,n"
00440   fl1$(8)="24,2,c 60,h,n"
00450   fl2$(5)="2,10,c 60,h,n"
00460   fl2$(6)="14,10,c 60,h,n"
00470   fl2$(7)="15,10,c 60,h,n"
00480   fl2$(8)="24,2,c 60,h,n"
00490   let io1$(1)="4,25,N 5,UE,N"
00500   let io1$(2)="5,25,N 1,UE,N"
00510   let io1$(3)="6,25,N 6,UE,N"
00520   let io1$(4)="7,25,C 12,UE,N"
00530   for j=1 to 10
00540     let io1$(4+j)=str$(j+9)&",1,C 6,UE,N"
00550     let io1$(14+j)=str$(j+9)&",8,C 55,UE,N"
00560     let io1$(24+j)=str$(j+9)&",64,N 10.2,UE,N"
00570     let io1$(34+j)=str$(j+9)&",75,N 2,UE,N"
00580     let io1$(44+j)=str$(j+9)&",78,N 2,UE,N"
00590   next j
00600   for j=1 to 4
00610     fl1$(j)=str$(j+3)&",8,c 20"
00620     ot2$(j)=str$(j+3)&",25,n 10.2,ut,n"
00630     fl2$(j)=fl1$(j)
00640   next j
00642   dim scr1$(4)
00650   scr1$(1)="CLIENT #"
00660   scr1$(2)="BILLING CODE"
00670   scr1$(3)="DATE"
00680   scr1$(4)="INVOICE #"
00740   open #1: "Name="&env$('Q')&"\TMmstr\CLmstr.h"&str$(cno)&",KFName="&env$('Q')&"\TMmstr\CLIndex.h"&str$(cno)&",Shr",internal,input,keyed ioerr ERTN
00750   open #32: "Name="&env$('Q')&"\TMmstr\CLmstr.h"&str$(cno)&",KFName="&env$('Q')&"\TMmstr\CLIndx2.h"&str$(cno)&",Shr",internal,input,keyed 
00770   pr newpage ! r: select entry type (regular or repringing previously entered invoices)
00780   pr f "8,30,c 16,r,n": " pr Invoices"
00790   pr f "10,5,c 70": "Enter 1 for regular or 2 if Reprinting previously entered invoices:"
00800   pr f "12,32,Cc 16,B,5": "Cancel (F5)"
00810   if f1=0 then let f1=1
00820 L820: rinput fields "10,73,N 1,UE,N": f1 conv L820
00830   if cmdkey=5 then goto XIT
00840   on f1 goto REGULAR_ENTRY,L880 none L820
00842 ! /r
00850 REGULAR_ENTRY: ! 
00860 ! close #h_tmwk1,free:
00870   open #h_tmwk1:=2: "Name="&env$('Q')&"\TMmstr\TMWk1.h"&str$(cno)&",RecL=2484,Replace",internal,outin,relative 
00880 L880: ! 
00882   open #3: "Name="&env$('Q')&"\TMmstr\IVDesc.h"&str$(cno)&",KFName="&env$('Q')&"\TMmstr\IVDIndex.h"&str$(cno)&",Shr",internal,input,keyed ioerr ERTN
00890   fnopenprn(cp,58,220,process)
00900   if f1=2 then gosub REPR_PREV_INV
00910 L910: ! 
00912   scrid$(1)="Time Management Input Of Invoices"
00920   scrid$(2)="Enter CLIENT # as 0 when completed."
00930   scrid$(3)="-Code- ---------Invoice Descriptions-------------------------  --Amount-- CT SC"
00940   scrid$(4)="  Press F1 when completed with this screen"
00950 L950: ! 
00952   let inp3=inp(3)
00960   mat inp=(0)
00970   let inp(3)=inp3
00990   let iv$=str$(iv1+1)
01000 ! on conv goto L1020
01010   let iv$=str$(iv1+1) conv ignore
01020 ! L1020: !
01022 ! on conv system
01030   mat id$=(" ")
01040   mat da=(0)
01050   mat cde$=("")
01060   mat ct=(0)
01070   mat sc=(0)
01080 L1080: ! 
01082   pr newpage
01090   pr f mat fl1$: mat scr1$,mat scrid$
01100   pr f "24,2,C 70,R,N": "  Press F1 to Continue; F5 to Stop; F6 for Search"
01110   pr f mat io1$: mat inp,iv$,mat cde$(1:10),mat id$(1:10),mat da(1:10),mat ct(1:10),mat sc(1:10)
01120   pr f "1,72,C 8,R,N": date$
01130   pr f "2,72,C 8,R,N": time$
01140   pr f io1$(2): 2
01150 L1150: ! 
01152   input fields mat io1$,attr "R": mat inp,iv$,mat cde$(1:10),mat id$(1:10),mat da(1:10),mat ct(1:10),mat sc(1:10) conv CONV1
01160   cp1=currow
01170   if ce>0 then let io1$(ce)(ce1:ce2)="U": ce=0
01180   if cmdkey>0 then goto L1260 else ce=curfld
01190   goto L1260
01200 L1200: ! 
01202   ce=ce+1
01204   if ce>udim(io1$) then ce=1
01210 CT1: ! 
01212   let io1$(ce)=rtrm$(uprc$(io1$(ce)))
01214   ce1=pos(io1$(ce),"U",9)
01216   if ce1=0 then goto L1200
01220   ce2=ce1+1
01222   let io1$(ce)(ce1:ce1)="UC"
01224   goto L1150
01230 CONV1: ! 
01232   if ce>0 then let io1$(ce)(ce1:ce2)="U"
01240   ce=cnt+1
01250 ERR1: ! 
01252   pr f "24,78,C 1": bell
01254   goto CT1
01260 L1260: ! 
01262   let de=cp1-9
01270   if de<1 or de>10 then let de=1
01280   if cmdkey=6 then goto HELP1
01290   if chg=2 and inp(1)=0 then goto L1720
01300   if cmdkey=1 or inp(1)=0 then goto L1710
01310   if ce><1 then goto L1370
01320   let k$=lpad$(str$(inp(1)),5)
01330   read #1,using 'form pos 6,c 30',key=k$: a1$ nokey ERR1
01350   pr f "4,35,C 40,H,N": a1$
01360   goto L1200
01370 L1370: ! 
01372   if ce=2 and inp(2)<1 or inp(2)>2 then goto ERR1
01380   if ce=2 then pr f "5,35,C 20,H,N": bc$(inp(2))
01390   if ce=3 and inp(3)<10100 or inp(3)>123199 then goto ERR1
01400   if ce<5 then goto L1200
01410   if ce>4 and ce<15 then goto L1610
01420   if ce<15 or ce>24 then goto L1450
01430   if rtrm$(id$(de))="" then goto ERR1
01440   ce=ce+10 : goto CT1
01450 L1450: ! 
01452   if ce<25 or ce>34 then goto L1480
01460   if da(de)=0 then goto ERR1
01470   ce=ce+10 : goto CT1
01480 L1480: ! 
01482   if ce<35 or ce>44 then goto L1510
01490   if ct(de)<1 or ct(de)>30 then goto ERR1
01500   ce=ce+10 : goto CT1
01510 L1510: ! 
01512   if ce<45 or ce>55 then goto L1540
01520   if sc(de)<0 or sc(de)>25 then goto ERR1
01530   ce=ce-39
01540 L1540: ! 
01542   let gl(1)=val(gl$(de)(1:3))
01544   let gl(2)=val(gl$(de)(4:9))
01546   let gl(3)=val(gl$(de)(10:12))
01550   pr f "22,40,C 20,N": "General Ledger #"
01551   dim fli4$(3)
01552   fli4$(1)="22,58,N 3,ut,N"
01554   fli4$(2)="22,62,N 6,ut,N"
01556   fli4$(3)="22,69,N 3,ut,N"
01570   rinput fields mat fli4$,attr "R": mat gl
01580   let gl$(de)=lpad$(str$(gl(1)),3)&lpad$(str$(gl(2)),6)&lpad$(str$(gl(3)),3)
01590   pr f "22,40,C 40": ""
01600   goto CT1
01610 L1610: ! 
01612   if rtrm$(cde$(de))="" then goto L1700
01620   cde$(de)=uprc$(cde$(de))
01630   cdk$=lpad$(rtrm$(cde$(de)),6)
01640   read #3,using L1650,key=cdk$: cdk$,des$,da,gl$(de) nokey ERR1
01650 L1650: form pos 1,c 6,c 55,pd 5.2,c 12
01660   pr f io1$(ce+10): des$
01670   pr f io1$(ce+20): da
01680   if cmdkey <> 6 then ce=ce+20
01690   goto CT1
01700 L1700: ! 
01702   ce=ce+10 : goto CT1
01710 L1710: ! 
01712   if inp(1)=0 and chg><2 then goto SCR_FINAL
01720 L1720: ! 
01722   if inp(1)=0 then mat inp=(0)
01730   if inp(1)=0 then goto L1900
01740   let pt(1)=pt(1)+inp(1)
01750   for j=1 to 10
01760     let pt(2)=pt(2)+da(j)
01770     let pt(3)=pt(3)+ct(j)
01780     let pt(4)=pt(4)+sc(j)
01790   next j
01800   if chg=2 then goto L1900
01810   let rw=lrec(2)+1
01820   write #h_tmwk1,using F_TMWK1: mat inp,iv$,mat cde$,mat id$,mat da,mat ct,mat sc,mat gl$
01830   let iv1=val(iv$) conv L1850
01840   fncreg_write('Last Invoice Number',str$(iv1))
01850 L1850: ! 
01852   if x9=0 then goto L950
01860   let inp(3)=0
01870   let inp(5)=0
01880   let inp(6)=0
01890   goto L1080
01900 L1900: ! 
01902   rewrite #h_tmwk1,using F_TMWK1,rec=rr: mat inp,iv$,mat cde$,mat id$,mat da,mat ct,mat sc,mat gl$
01910 SCR_ADDEDIT: pr newpage
01920   pr f "10,10,c 60": "Enter ref # to correct; enter 0 when completed"
01930 L1930: input fields "10,60,N 5,UE,N": rr conv L1930
01940   if rr=0 then goto SCR_FINAL
01950   read #h_tmwk1,using F_TMWK1,rec=rr: mat inp,iv$,mat cde$,mat id$,mat da,mat ct,mat sc,mat gl$ norec SCR_ADDEDIT ioerr ERTN
01960   let pt(1)=pt(1)-inp(1)
01970   for j=1 to 10
01980     let pt(2)=pt(2)-da(j)
01990     let pt(3)=pt(3)-ct(j)
02000     let pt(4)=pt(4)-sc(j)
02010   next j
02020   goto L1080
02030 ! ______________________________________________________________________
02040 SCR_FINAL: ! r:
02042   pr newpage
02050   scrid$(1)="TIME MANAGEMENT INPUT PROOF TOTALS"
02060   scrid$(2)="1 for listing, 2 for corrections, 3 for additional entries,"
02070   scrid$(3)=" 4 to pr invoices entered, or 5 to merge."
02080   scrid$(4)=""
02090   pr f mat fl2$: mat scr2$,mat scrid$
02100   pr f mat ot2$: mat pt
02110 L2110: input fields "16,30,N 1,UE,N": chg conv L2110
02120   on chg goto PR_PROOF,SCR_CORRECTION,L910,SCR_PRINT_INVOICES,GO_MERGE none L2110
02122 ! /r
02130 PR_PROOF: ! r:
02132   pr newpage
02140   pr f "13,30,Cc 20,B,5": "Cancel (F5)"
02150   pr f "10,10,c 60,h,n": "TIME MANAGEMENT CORRECTION LISTING IN PROCESS"
02160   gosub PR_PROOF_HEAD
02232   for j=1 to lrec(2)
02240     read #h_tmwk1,using F_TMWK1,rec=j: mat inp,iv$,mat cde$,mat id$,mat da,mat ct,mat sc,mat gl$ ioerr ERTN
02250     if inp(1)<>0 then 
02260       pr #255: "REF #  CLIENT #  BILLING-CODE     DATE      INVOICE #"
02270       pr #255,using L2280: j,mat inp,iv$ pageoflow PR_PROOF_PGOF
02280 L2280: form pos 1,n 4,n 8,n 10,n 12,x 2,c 12,skip 2
02332       pr #255: "-CODE-  -------DESCRIPTION-------------------------------------  --AMOUNT--      CAT     SUB  -GL--NUMBER-"
02340       for j1=1 to 30
02350         if rtrm$(id$(j1))<>"" then 
02360           pr #255,using L2370: cde$(j1),id$(j1),da(j1),ct(j1),sc(j1),gl$(j1) pageoflow PR_PROOF_PGOF
02370 L2370:    form pos 1,c 8,c 56,n 11.2,2*n 8,x 3,c 12
02410         end if 
02412       next j1
02420       pr #255: "----------------------------------------------------------------------------------------------------------"
02430       pr #255: 
02440     end if 
02442   next j
02452   fncloseprn
02460   goto SCR_FINAL ! /r
02461 PR_PROOF_HEAD: ! r:
02462   pr #255,using L2200: date$,cnam$,time$,"Time Management pr Invoices Proof Listing"
02463 L2200: form skip 1,pos 1,c 8,pos 44,cc 44,skip 1,pos 1,c 8,pos 44,c 44,skip 2
02464   return  ! /r
02465 PR_PROOF_PGOF: ! r:
02466   pr #255: newpage
02467   gosub PR_PROOF_HEAD
02468   continue  ! /r
02470 SCR_CORRECTION: ! r:
02472   scrid$(1)="TIme Management Input Correction Screen"
02480   scrid$(2)="Enter client # as 0 to delete this entry"
02490   scrid$(3)="  Desc/Code   Invoice Descriptions"
02500   scrid$(4)="  Press F1 when completed with this screen"
02510   goto SCR_ADDEDIT ! /r
02520 GO_MERGE: ! r:
02522   close #1: 
02530   close #h_tmwk1: 
02540   chain "S:\acsTM\TMMRGINV"
02550 ! /r
02560 SCR_PRINT_INVOICES: ! r:
02562   pr newpage
02570   pr f "10,20,c 30,h,n": "position invoices in printer"
02580   pr f "11,20,c 38,n": "Enter 1 to pr selected invoices,"
02582   pr f "12,20,c 38,n": "   Or 0 to pr all invoices entered."
02590 L2590: input fields "13,26,N 1,UE,N": select_invoices_to_print conv L2590
02600   if select_invoices_to_print=1 then goto SCR_SELECT_INVOICE
02610   if select_invoices_to_print><0 then goto L2590
02620   pr newpage
02630   pr f "10,10,c 60": "Print invoices in process"
02640   fnopenprn
02670   align=0
02672   restore #h_tmwk1: 
02680   do  ! for j=1 to lrec(h_tmwk1)
02690 PR_SELECTED_INVOICE: ! 
02692     read #h_tmwk1,using F_TMWK1: mat inp,iv$,mat cde$,mat id$,mat da,mat ct,mat sc,mat gl$ eof PRI_EOF norec L2870 ioerr ERTN
02700     if inp(1)=0 then goto L2840
02710     let k$=lpad$(str$(inp(1)),5)
02720     read #1,using L2730,key=k$: mat a$ ioerr ERTN
02730 L2730: form pos 6,3*c 30
02750     fnprint_invoice(align, k$, mat a$, iv$, inp(3),mat id$, mat da,0)
02840 L2840: ! 
02842     if select_invoices_to_print=1 then goto SCR_SELECT_INVOICE
02850   loop  ! next j
02852 PRI_EOF: ! 
02860   fncloseprn
02870 L2870: ! 
02872   goto SCR_FINAL ! /r
02880 SCR_SELECT_INVOICE: ! r:
02882   pr newpage
02890   pr f "10,10,c 60": "Ref # of invoice to print, 0 when finished"
02900 L2900: input fields "10,70,N 5,UE,N": j conv L2900
02910   if j=0 then goto SCR_FINAL
02920   if j<1 or j>rw then goto SCR_SELECT_INVOICE
02930   goto PR_SELECTED_INVOICE ! /r
02940 REPR_PREV_INV: ! 
02942   mat pt=(0)
02950   for rw=1 to lrec(2)
02960     read #h_tmwk1,using F_TMWK1,rec=rw: mat inp,iv$,mat cde$,mat id$,mat da,mat ct,mat sc norec L3050 ioerr ERTN
02970 F_TMWK1: form pos 1,n 5,n 1,n 6,c 12,30*c 6,30*c 55,30*pd 5.2,30*n 2,30*n 2,30*c 12
02980     if inp(1)=0 then goto L2840
02990     let pt(1)=pt(1)+inp(1)
03000     for j=1 to 10
03010       let pt(2)=pt(2)+da(j)
03020       let pt(3)=pt(3)+ct(j)
03030       let pt(4)=pt(4)+sc(j)
03040     next j
03050 L3050: next rw
03060   return 
03070 XIT: let fnxit
03080 ERTN: if err=61 then pr f "23,3,C 75,N": "THIS PROGRAM IS TRYING TO ACCESS A RECORD THAT IS IN Use!" else goto L3100
03090   goto L3140
03100 L3100: pr newpage
03110   if err=4148 then pr f "23,3,C 78,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN Use AND CANNOT BE SHARED!" else goto L3130
03120   goto L3140
03130 L3130: pr f "23,3,C 75,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
03140 L3140: pr f "24,3,C 70,N": "PRESS ENTER TO RETRY; ELSE ENTER  Q  TO QUIT"
03150   input fields "24,60,C 1,N": quitcode$
03160   if rtrm$(uprc$(quitcode$))="Q" then goto L3200
03170   pr f "23,3,C 78,N": ""
03180   pr f "24,3,C 78,N": ""
03190   retry 
03200 L3200: goto XIT
03210 SRCH1: s1=1 ! NAME SEARCH
03220   open #127: "SROW=1,SCOL=1,EROW=24,ECOL=80",display,outin  ! SAVE SCREEN
03230 L3230: pr #127: newpage
03240   close #101: ioerr ignore
03250   open #101: "SROW=6,SCOL=3,EROW=08,ECOL=78,BORDER=DR,CAPTION=BUsiness Name Search",display,outin 
03260   let prtall=0
03270   pr f "7,4,C 55,H,N": "Enter beginning search info. or blank for all:"
03280   pr f "9,32,C 16,R,N": "Press F5 to stop"
03290 L3290: input fields "7,50,C 25,UE,N": nam$
03300   if cmdkey=5 then goto SRCHEND
03310   let nam$=rtrm$(nam$)
03320   l1=len(nam$)
03330   restore #32,search>=nam$: nokey L3290
03340   close #101: ioerr ignore
03350 L3350: pr newpage
03360   pr f "1,10,C 5,R,N": "Acct#"
03370   pr f "1,17,C 30,R,N": "Company Name"
03380   cde=0
03390   for j=1 to 20
03400     read #32,using L3410,release: k$,a1$ eof L3510
03410 L3410: form pos 1,c 5,c 30
03420     if a1$(1:l1)=nam$ or prtall=1 then goto L3430 else goto L3510
03430 L3430: cde=1
03440     pr f str$(j+1)&",10,C 5,ut,N": k$
03450     pr f str$(j+1)&",17,C 30,ut,N": a1$
03460     if j>1 then goto L3500
03470     bk=bk+1
03480     if bk>20 then bk=1
03490     bk$(bk)=a1$
03500 L3500: next j
03510 L3510: if j>1 then let j=j-1
03520   mat in2$(j)
03530   pr f "24,08,C 60,R,N": "Enter to continue; F5 to stop or enter ACCOUNT #:"
03540 L3540: input fields "24,58,C 14,RE,N": k$
03550   alp=0
03560   if cmdkey=5 then goto SRCHEND
03570   if rtrm$(k$)><"" then let inp(1)=val(k$) conv L3540 : goto SRCHEND
03580   if cmdkey><2 then goto L3630
03590   bk=bk-1
03600   if bk<1 then goto L3650
03610   restore #32,key>=bk$(bk): nokey L3650
03620   bk=bk-1
03630 L3630: selclp=1
03640   goto L3350
03650 L3650: selclp=0
03660   goto L3230
03670 SRCHEND: ! r:
03672   close #101: ioerr ignore
03680   close #127: ioerr L3720
03690   if rtrm$(k$)="" then goto L3720
03700   if s1=1 then pr f io1$(1): inp(1)
03710   if s1=2 then pr f io1$(ce): k$
03720 L3720: ! 
03722   return  ! /r
03730 !  SCRNSAV: ! r:
03732 !    sav1=1
03740 !    close #99: ioerr ignore
03750 !    open #99: "SROW=1,SCOL=1,EROW=24,ECOL=80",display,outin
03760 !    pr #99: newpage
03770 !  L3770: !
03772 !    let j1=int(rnd*22+1)
03780 !    let j2=int(rnd*54+1)
03790 !    for n=1 to 24 : pr f str$(n)&",1,C 80,B,N": "" : next n
03800 !    pr f str$(j1)&","&str$(j2)&",C 25,R,N": "   A.C.S. Screen Saver"
03810 !    pr f str$(j1+2)&","&str$(j2)&",C 25,R,N": "PRESS ANY KEY TO CONTINUE"
03820 !    pr f str$(j1+1)&","&str$(j2)&",C 25,R,N": "   "&date$&"   "&time$
03830 !    let t1=val(time$(7:8))
03840 !  L3840: !
03842 !    let t2=val(time$(7:8))
03850 !    let x$=kstat$: if x$><"" then goto OVER
03860 !    if t1=t2 then goto L3840
03870 !    pr f str$(j1)&","&str$(j2)&",C 25,B,N": ""
03880 !    pr f str$(j1+1)&","&str$(j2)&",C 25,B,N": ""
03890 !    pr f str$(j1+2)&","&str$(j2)&",C 25,B,N": ""
03900 !    goto L3770
03910 !  OVER: !
03912 !    close #99:
03920 !    return ! /r
03930 HELP1: ce=curfld
03940   if ce=1 then gosub SRCH1 : goto CT1
03950   if ce>4 and ce<16 then gosub SRCH2 : goto CT1
03960   goto CT1
03970 SRCH2: ! r:
03972   s1=2 ! CODE SEARCH
03980   open #127: "SROW=1,SCOL=1,EROW=24,ECOL=80",display,outin 
03990 L3990: ! 
03992   pr #127: newpage
04000   close #101: ioerr ignore
04010   open #101: "SROW=6,SCOL=3,EROW=08,ECOL=78,BORDER=DR,CAPTION=CODE SEARCH",display,outin 
04020   let prtall=0
04030   pr f "7,4,C 55,H,N": "Enter beginning search info. or blank for all:"
04040   pr f "9,32,C 16,R,N": "Press F5 to stop"
04050 L4050: ! 
04052   input fields "7,50,C 6,UE,N": nam$
04060   if cmdkey=5 then goto SRCHEND
04070   let nam$=lpad$(rtrm$(nam$),6)
04080   restore #3,search>=nam$: nokey L4050
04090   close #101: ioerr ignore
04100 L4100: ! 
04102   pr newpage
04110   pr f "1,2,C 6,ut,N": " Code"
04120   pr f "1,9,C 46,ut,N": "Description"
04130   pr f "1,56,C 10,ut,N": "  Amount"
04140   pr f "1,67,C 12,ut,N": "  GL Number"
04150   cde=0
04160   for j=1 to 20
04170     read #3,using L1650,release: cdk$,des$,da,gl$ eof L4290
04190     cde=1
04200     pr f str$(j+1)&",2,C 6,ut,N": cdk$
04210     pr f str$(j+1)&",9,C 46,ut,N": des$(1:46)
04220     pr f str$(j+1)&",56,N 10.2,ut,N": da
04230     pr f str$(j+1)&",67,C 12,ut,N": gl$
04240     if j>1 then goto L4280
04250     bk=bk+1
04260     if bk>20 then bk=1
04270     bk$(bk)=cdk$
04280 L4280: next j
04290 L4290: if j>1 then let j=j-1
04300   mat in2$(j)
04310   pr f "24,08,C 60,R,N": "Enter to continue; F5 to stop or enter ACCOUNT #:"
04320 L4320: input fields "24,58,C 6,RE,N": k$
04330   alp=0
04340   if cmdkey=5 then goto SRCHEND
04350   if rtrm$(k$)><"" then let inp(1)=val(k$) conv L4320 : goto SRCHEND
04360   if cmdkey><2 then goto L4410
04370   bk=bk-1
04380   if bk<1 then goto L4430
04390   restore #32,key>=bk$(bk): nokey L4430
04400   bk=bk-1
04410 L4410: selclp=1
04420   goto L4100
04430 L4430: selclp=0
04440   goto L3990 ! /r
12000 IGNORE: continue 
