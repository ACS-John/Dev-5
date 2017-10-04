00010 ! 
00020 ! 
00030 ! 
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnerror,fndat,fncno,fntop,fnxit,fnacs,fntos,fnlbl,fntxt,fncomboa,fnchk,fncmdset,fnwait,fndate_mmddyy_to_ccyymmdd
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim dat$*20,cnam$*40,vnam$*30,de$*50,fd$*30,ft(3),aa(2),gl(3),ade$*50
00080   dim ft2(1000),cap$*128,ti1$*1,io4$*2,wrd4$(2)*13,io5$(2),wrd5$(2)*31
00090   dim tr$(5)*35,tr(2),pp1yn$*1,item2$(2)*35,item1$(2)*15
00100   dim tmp$*40,dp(3),dp$*30
00110 ! ______________________________________________________________________
00120   fntop(program$,cap$="Claims or Purchases Report")
00130   cancel=99
00140   fncno(cno,cnam$)
00160   fndat(dat$)
00170   gosub ASK_TI1
00180   goto ASK_PP1
00190 ! ______________________________________________________________________
00200 ASK_SORT: ! 
00210   if fund=1 then let ty1$="Vendor Sequence" else let ty1$="Fund Sequence"
00220   if coded=1 then let ty2$="All Invoices" else let ty2$="Selected Invoices"
00230   open #paytrans=4: "Name="&env$('Q')&"\CLmstr\PayTrans.H"&str$(cno)&",Shr",internal,input,relative 
00240   execute "Index "&env$('Q')&"\CLmstr\unpdaloc.H"&str$(cno)&' '&env$('Q')&"\CLmstr\Uaidx2.H"&str$(cno)&" 1 20 Replace DupKeys -n" ! index in vendor, reference order
00250   open #unpdaloc=8: "Name="&env$('Q')&"\CLmstr\UnPdAloc.H"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\Uaidx2.H"&str$(cno)&",Shr",internal,input,keyed 
00260 READ_PAYTRANS: ! 
00270   read #paytrans,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,g 10.2,G 1': vn$,iv$,ivd,dd,po$,de$,upa,cde eof L420
00280   if coded=2 and cde=0 then goto READ_PAYTRANS
00290   if fndate_mmddyy_to_ccyymmdd(dd)>fndate_mmddyy_to_ccyymmdd(d2) then goto READ_PAYTRANS
00300   if ti1$<>"C" and (fndate_mmddyy_to_ccyymmdd(ivd)<ld1 or fndate_mmddyy_to_ccyymmdd(ivd)>hd1) then !:
          goto READ_PAYTRANS ! Purchases Only
00310   let ivnum+=1 ! Unique Number for each Invoice
00320   restore #unpdaloc,key>=vn$&"            ": 
00330 READ_UNPDALOC: ! 
00340   read #unpdaloc,using 'Form pos 1,c 8,c 12,N 3,N 6,N 3,PD 5.2,C 30': alvn$,aliv$,mat gl,amt,ade$ norec READ_PAYTRANS eof READ_PAYTRANS
00350   if alvn$<>vn$ then goto READ_PAYTRANS
00360   if trim$(aliv$)<>trim$(iv$) then goto READ_UNPDALOC
00370   if fund=2 then let de$=ade$(1:18)
00380   if sum(gl)=0 and amt=0 then goto L410 ! don't write zero records
00390   write #clwork,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,pd 10.2,G 1,N 3,N 6,N 3,N 8': vn$,iv$,ivd,dd,po$,de$(1:18),amt,cde,mat gl,ivnum
00410 L410: goto READ_UNPDALOC
00420 L420: close #paytrans: : close #unpdaloc: : close #clwork: 
00430   let upa=0 ! this sort is ok. it sorts a temporary work file. leave in
00440   open #tmp=9: "Name="&env$('temp')&"\Control,Size=0,RecL=128,Replace",internal,output 
00450   write #tmp,using 'Form POS 1,C 128': "File CLWork"&wsid$&".H"&str$(cno)&","&env$('Q')&"\CLmstr,,"&env$('temp')&"\Addr,,,,,A,N"
00460   if fund=2 then !:
          write #tmp,using 'Form POS 1,C 128': "Mask 74,12,N,A" ! "Mask 74,3,N,A,1,20,C,A,86,4,N,A"
00470   if fund<>2 then !:
          write #tmp,using 'Form POS 1,C 128': "Mask 1,20,C,A,86,4,N,A"
00480   close #tmp: 
00490   execute "Free "&env$('temp')&"\Addr -n" ioerr ignore
00500   execute "Sort "&env$('temp')&"\Control -n"
00510   open #addr:=9: "Name="&env$('temp')&"\Addr",internal,input 
00520   open #paymstr:=13: "Name="&env$('Q')&"\CLmstr\PayMstr.H"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\PayIdx1.H"&str$(cno)&",Shr",internal,outin,keyed 
00530   open #rpmstr:=23: "Name="&env$('Q')&"\PRmstr\rpMstr.H"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\rpIndex.H"&str$(cno)&",Shr",internal,input,keyed ioerr L550
00540   let prcode=1
00550 L550: open #clwork:=10: "Name="&env$('Q')&"\CLmstr\CLWork"&wsid$&".H"&str$(cno)&",Shr",internal,input,relative 
00560   open #glmstr:=5: "Name="&env$('Q')&"\CLmstr\GLmstr.H"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\GLIndex.H"&str$(cno)&",Shr",internal,outin,keyed 
00570   open #work:=6: "Name="&env$('temp')&"\Work,Size=0,RecL=22,Replace",internal,output 
00580   close #work: 
00590   execute "Free "&env$('temp')&"\Indx -n" ioerr ignore
00600   execute "Index "&env$('temp')&"\Work,"&env$('temp')&"\Indx,1,12,Replace,DupKeys -n"
00610   open #work=6: "Name="&env$('temp')&"\Work,KFName="&env$('temp')&"\Indx",internal,outin,keyed 
00620   open #fundmstr=7: "Name="&env$('Q')&"\CLmstr\FundMstr.H"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\FundIdx1.H"&str$(cno)&",Shr",internal,input,keyed 
00630   let notused=1: open #11: "Name="&env$('Q')&"\CLmstr\dptmSTR.H"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\dptidx1.H"&str$(cno),internal,input,keyed ioerr L640 : let notused=0
00640 L640: let fnopenprn
00650   let vn$="": let iv$=""
00660 L660: read #addr,using 'Form POS 1,PD 3': r4 eof END1
00670   if r4=0 then goto L660
00680   read #clwork,using 'Form POS 86,N 4',rec=r4: ivnum
00690   if hivnum=0 then let upa=0 : goto L700 else goto L760
00700 L700: read #clwork,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,pd 10.2,G 1,N 3,N 6,N 3,N 8',rec=r4: vn$,iv$,ivd,dd,po$,ade$,amt,cde,mat gl,ivnum
00710   let upa=upa+amt
00720   let hivnum=ivnum
00730   let gl$=cnvrt$("PIC(ZZ#)",gl(1))&cnvrt$("PIC(ZZZZZZ)",gl(2))&cnvrt$("PIC(ZZ#)",gl(3))
00740   gosub L1050 ! ACCUMULATE G/L TOTALS
00750   goto L660
00760 L760: if vn$=hvn$ then goto L810
00770   if fund=2 then goto L810 ! no vendor totals in fund sequence
00780   if vc<2 then goto L800
00790   pr #255: tab(87);"__________"
00800   pr #255,using L3070: "Vendor Total",v3
00805 L800: let vc=v1=v2=v3=0
00810 L810: ! pr #255: PAGEOFLOW NEWPGE
00820   let vc=vc+1
00830   let hvn$=vn$
00840   if nofx=0 then gosub HDR
00850 ! If GL$(3:3)<>"0" Then Pause
00860   if pos1=0 and pos2=0 then goto L880
00870   if notused =0 and dpt$><gl$(1:3)&cnvrt$("pic(zz)",val(gl$(pos1:pos2))) then gosub TOTAL_DEPARTMENT
00880 L880: if fund<>2 then goto L930
00890   if fund$><gl$(1:3) then gosub TOTAL_FUND : pr #255: newpage : gosub HDR
00900   let f2=(val(gl$(1:3)))
00910   if f2=0 then let f2=1000
00920   let ft2(f2)=ft2(f2)+upa
00930 L930: if cde=1 then let p1=97: let v1=v1+upa: let t1=t1+upa : let ft(1)=ft(1)+upa: let dp(1)=dp(1)+upa else let p1=109: let v2=v2+upa : let t2=t2+upa : let ft(2)=ft(2)+upa : let dp(2)=dp(2)+upa
00940   let v3=v3+upa
00950   let t3=t3+upa : if rtrm$(ltrm$(iv$))="Received" or rtrm$(ltrm$(iv$))="Adjustment" then goto L970 else let ft(3)=ft(3)+upa : let dp(3)=dp(3)+upa
00970 L970: let vnam$=""
00980   read #paymstr,using 'Form POS 9,C 30',key=vn$,release: vnam$ nokey L1000
00990   goto L1010
01000 L1000: if prcode=1 then !:
          read #rpmstr,using 'Form POS 9,C 30',key=vn$: vnam$ nokey ignore
01001 L1010: ! 
01002   if trim$(vnam$)='CRIS PERRY' then pr 'vn$='&vn$ : pause 
01010   pr #255,using 'Form POS 1,C 32,C 12,2*PIC(ZZZZ/ZZ/ZZ),X 2,C 18,POS 87,N 10.2,POS 99,C 4,C 7,C 3': vnam$,iv$,ivd,dd,ade$(1:18),upa,gl$(1:3),gl$(4:9),gl$(10:12) pageoflow NEWPGE
01020   let upa=0
01030   if endcode=1 then goto L1360
01040   goto L700
01050 L1050: if gl$(3:3)=" " then let gl$(3:3)="0"
01060   if gl$(12:12)=" " then let gl$(12:12)="0"
01070   read #work,using 'Form POS 13,pd 10.2',key=gl$: gla nokey L1120
01080   if amt<-20202020 then amt=0
01090   let gla=gla+amt
01100   rewrite #work,using 'Form POS 13,pd 10.2': gla
01110   goto L1150
01120 L1120: ! 
01122   if amt<-20202020 then amt=0
01130   if amt<>0 then 
01140     write #work,using 'Form POS 1,C 12,pd 10.2': gl$,amt
01142   end if 
01150 L1150: ! 
01152   return 
01160 NEWPGE: pr #255: newpage: gosub HDR : continue 
01330 IGNORE: continue 
01340 END1: ! 
01350   if r4=0 then goto XIT
01360   endcode=1
01370   goto L760
01375 L1360: if vc<2 then goto L1390
01380 ! pr #255: TAB(87);"__________"
01385 ! pr #255,Using 1800: "Vendor Total",V3 Pageoflow NEWPGE
01390 L1390: gosub TOTAL_FUND
01400   pr #255: tab(87);"__________"
01410   pr #255,using 'Form POS 67,C 18,N 12.2': "Final Total",t3 pageoflow NEWPGE
01420   pr #255: tab(87);"=========="
01430   restore #work,key>="            ": nokey L1620
01440 L1440: read #work,using 'Form POS 1,C 12,pd 10.2': gl$,gla eof L1620
01450   if hf$="" or hf$=gl$(1:3) then goto L1470
01460   gosub TOTNOFX
01470 L1470: let hf$=gl$(1:3)
01480   let de$=""
01490   read #glmstr,using 'Form POS 13,C 50',key=gl$: de$ nokey L1500
01500 L1500: pr #255,using 'Form POS 12,C 14,C 50,N 12.2': gl$,de$,gla pageoflow NEWPGE
01510   let tnofx+=gla
01520   goto L1440
01530 ! ______________________________________________________________________
01540 TOTNOFX: pr #255: tab(78);"__________"
01550 ! If FUND<>2 Then Goto 1550
01560   if val(hf$)>0 then let fd$="Total for Fund #: "&ltrm$(hf$) else let fd$="Total"
01570   pr #255,using 'Form POS 12,C 14,C 50,N 12.2': "",fd$,tnofx pageoflow NEWPGE
01580   pr #255: pageoflow NEWPGE
01590   let tnofx=0
01600   return 
01610 ! ______________________________________________________________________
01620 L1620: gosub TOTNOFX
01630   if fund=2 then gosub FT2
01640   fncloseprn
01650   close #work,free: ioerr XIT
01660 XIT: let fnxit
01670 ! ______________________________________________________________________
01680 TOTAL_FUND: ! r:
01690   pr #255: tab(87);"__________"
01700   pr #255,using 'Form POS 67,C 18,N 12.2': "Fund   Total",ft(3) pageoflow NEWPGE
01710   mat ft=(0)
01720   return  ! /r
01730 ! ______________________________________________________________________
01740 ASK_PP1: ! 
01750   open #clwork=10: "Name="&env$('Q')&"\CLmstr\CLWork"&wsid$&".H"&str$(cno)&", Size=0, RecL=93, Replace", internal,outin 
01760   open #trmstr=1: "Name="&env$('Q')&"\CLmstr\TrMstr.H"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\TrIdx1.H"&str$(cno)&",Shr",internal,input,keyed 
01770   open #tralloc=2: "Name="&env$('Q')&"\CLmstr\TrAlloc.H"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\TrAlloc-Idx.H"&str$(cno)&",Shr",internal,input,keyed 
01880   if pp1yn$="N" then goto END8
02070   ld1=fndate_mmddyy_to_ccyymmdd(ld1) : let hd1=fndate_mmddyy_to_ccyymmdd(hd1)
02080 READ_TRMSTR: ! 
02090   read #trmstr,using 'Form POS 1,N 2,N 1,C 8,G 6,PD 10.2,C 8,C 35,pos 78,n 1': bank_code,tcde,tr$(1),tr$(2),tr3,tr$(4),tr$(5),scd eof END8 !:
        let tr$(3)=str$(tr3)
02100   if tcde=2 then goto READ_TRMSTR ! skip receipts
02110   if pr$="N" and scd=4 then goto READ_TRMSTR ! skip payroll checks
02120   let pd1=fndate_mmddyy_to_ccyymmdd(val(tr$(2))) conv READ_TRMSTR
02130   if ld1=0 and hd1=0 then goto L2150
02140   if pd1<ld1 or pd1>hd1 then goto READ_TRMSTR
02150 L2150: ck1=val(tr$(1)) conv NEXT_IVNUM
02160   goto RESTORE_TRALLOC
02170 ! ______________________________________________________________________
02180 NEXT_IVNUM: ! 
02190   let ivnum+=1 : ck1=ivnum
02200 RESTORE_TRALLOC: ! 
02210   let key$=cnvrt$("Pic(zz)",bank_code)&str$(tcde)&tr$(1) !:
        restore #tralloc,key=key$: nokey READ_TRMSTR
02220 READ_TRALLOC: ! 
02230   read #tralloc,using 'Form Pos 1,C 11,N 3,N 6,N 3,PD 5.2,C 30,C 6,X 3,C 12,N 1': newkey$,mat gl,amt,de$,ivd$,po$,pc eof READ_TRMSTR 
02232   ivd=val(ivd$) conv ignore
02234   if newkey$<>key$ then goto READ_TRMSTR
02240   if ivd=0 then goto L2270
02250   if pc=0 or pc=1 then goto WRITE_CLWORK ! Never been in AP
02260   if fndate_mmddyy_to_ccyymmdd(ivd)<ld1 or fndate_mmddyy_to_ccyymmdd(ivd)>hd1 then goto EO_TRALLOC_LOOP
02270 L2270: if tcde=2 then let paid$="Received" else !:
          if tcde=3 then let paid$="Adjustment" !:
          else let paid$="Paid"
02280 WRITE_CLWORK: ! 
02290   write #clwork,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,pd 10.2,G 1,N 3,N 6,N 3,N 8': tr$(4),paid$,ivd,0,po$,de$(1:18),amt,0,mat gl,ck1
02300 EO_TRALLOC_LOOP: goto READ_TRALLOC
02310 ! ______________________________________________________________________
02320 END8: ! 
02330   close #trmstr: 
02340   close #tralloc: 
02350   goto ASK_SORT
02360 ! ______________________________________________________________________
02370 FT2: let fd$="Total for all Funds"
02380   pr #255: newpage : gosub HDR
02390   pr #255: "Fund   Amount" !:
        pr #255: "____  __________"
02400   if ft2(1000)<>0 then !:
          pr #255,using 'Form POS 1,N 4,N 12.2': 0,ft2(1000) pageoflow NEWPGE
02410   for j=1 to 999
02420     if ft2(j)<>0 then !:
            pr #255,using 'Form POS 1,N 4,N 12.2': j,ft2(j) pageoflow NEWPGE
02430   next j
02440   return 
02450 ! ______________________________________________________________________
02460 ASK_TI1: ! r:
02470   fntos(sn$="claims") !:
        let respc=0
02480   fnlbl(1,1,"Cutoff Date:",38,1)
02490   fntxt(1,40,8,0,1,"1",0,"No invoices past this date will be listed!") !:
        let resp$(respc+=1)=""
02500   fnlbl(2,1,"Type of Report:",38,1)
02510   let item1$(1)="Claims" !:
        let item1$(2)="Purchases"
02520   fncomboa("claims-srt",2,40,mat item1$,tt$) !:
        let resp$(respc+=1)=item1$(1)
02530   fnchk(3,41,"Include previously paid Invoices:",1) !:
        let resp$(respc+=1)="False"
02540   fnlbl(5,1,"Starting Date:",38,1)
02550   fntxt(5,40,8,0,1,"1",0,"Only applicable if including previously paid invoices!") !:
        let resp$(respc+=1)=""
02560   fnlbl(6,1,"Ending Date:",38,1)
02570   fntxt(6,40,8,0,1,"1",0,"Only applicable if including previously paid invoices!") !:
        let resp$(respc+=1)=""
02580   fnlbl(8,1,"Sort by:",38,1)
02590   let item2$(1)="Fund Number" !:
        let item2$(2)="Vendor Number"
02600   fncomboa("claims-act",8,40,mat item2$) !:
        let resp$(respc+=1)=item2$(1)
02610   fnlbl(10,1,"Show Invoices:",38,1)
02620   let item2$(1)="All Invoices" !:
        let item2$(2)="Coded for Payment"
02630   fncomboa("claims-3",10,40,mat item2$,"You have a choice of listing all unpaid invoices on the report, or just those that have been selected for payment") !:
        let resp$(respc+=1)=item2$(1)
02640   fnchk(11,41,"Include payroll checks:",1) !:
        let resp$(respc+=1)="False"
02650   fnlbl(13,1,"Beginning Position of Department Number:",43,1)
02660   fntxt(13,46,2,0,1,"30",0,"If you have departmental breakdowns within a fund, you must identify the first digit of the department # within the general ledger number") !:
        let resp$(respc+=1)=" "
02670   fnlbl(14,1,"Ending Position of Department Number:",43,1)
02680   fntxt(14,46,2,0,1,"30",0,"Last digit representing department #. Example: GL # '001001600000' The beginning position would be 6 and the ending 7 if department number was the 16.") !:
        let resp$(respc+=1)=" "
02690   fncmdset(2)
02692   fnacs(sn$,0,mat resp$,ck)
02700   if ck=5 then goto XIT
02710   let d2=val(resp$(1)) ! cutoff date
02720   let ti1$=resp$(2)(1:1)
02730   if resp$(3)(1:1)="T" then let pp1yn$="Y" else let pp1yn$="N"
02740   ld1=val(resp$(4))
02750   let hd1=val(resp$(5))
02760   if resp$(6)(1:1)="V" then let fund=1 else let fund=2 ! vendor # or fund #
02770   if resp$(7)(1:1)="A" then coded=1 else coded=2 ! all invoices or only coded for payment
02780   if resp$(8)(1:1)="T" then let pr$="Y" else let pr$="N"
02790   let pos1=val(resp$(9))
02800   let pos2=val(resp$(10))
02810   return 
02850 ! /r
02860 ! <Updateable Region: ERTN>
02870 ERTN: let fnerror(program$,err,line,act$,"xit")
02880   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
02890   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
02900   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
02910 ERTN_EXEC_ACT: execute act$ : goto ERTN
02920 ! /region
02930 ! ______________________________________________________________________
02940 TOTAL_DEPARTMENT: ! r: DPT TOTAL   ! special modifications for monticello and other Ed clients - Must have the departmental file set up under files for this to run
02950 ! If GL$(3:3)<>"0" Then Pause
02960   if fund<>2 then goto TD_XIT
02970   if dp(3)<>0 then 
02980     let dp$=""
02990     if pos1=0 and pos2=0 then goto TD_XIT
03000     let dpt$=gl$(1:3)&cnvrt$("pic(zz)",val(gl$(pos1:pos2)))
03010     read #11,using 'form pos 1,n 3,n 2,c 30',key=dpt$: fund1,dept1,dp$ nokey TD_XIT
03040     if dept1=0 then goto TD_XIT ! don't do department totals if department file only has fund numbers.
03050     pr #255: tab(87);"----------"
03060     pr #255,using L3070: "DEPT   TOTAL",dp(3) pageoflow NEWPGE
03070 L3070: form pos 67,c 18,n 12.2,skip 1
03072   end if 
03080   mat dp=(0)
03090   if gl$(1:3)><fund$ then goto TD_XIT
03100   let hp3=51-int(len(rtrm$(dp$))/2)
03110   pr #255,using L3120: dp$ pageoflow NEWPGE
03120 L3120: form pos hp3,c 30,skip 1
03130 TD_XIT: ! 
03132   return  ! /r
42000 HDR: ! 
42010   let fd$=""
42020   let fund$=gl$(1:3)
42040   read #fundmstr,using 'Form POS 4,C 25',key=fund$: fd$ nokey ignore ! changed from "nokey ignore" in an attempt to fix error 201
42060   let nofx=1 : let pg+=1
42080   pr #255,using 'Form POS 1,C 8,CC 86': date$,cnam$
42100   if ti1$="C" then let tmp$="Claims" else let tmp$="Purchases"
42120   pr #255,using 'Form POS 1,C 8,POS 30,C 50': time$,tmp$&" Report-"&rtrm$(ty1$)&"-"&rtrm$(ty2$)
42140   pr #255,using 'Form POS 1,C 4,N 4,CC 86': "Page",pg,dat$
42160   if fund<>2 then let fd$=""
42180   pr #255,using 'Form POS 1,Cc 102': fd$
42200   pr #255: ""
42220   pr #255: "                                              Invoice     Due                           Total          GL    "
42240   pr #255: "Payee Name                      Invoice Numb    Date      Date    Description            Due          Number"
42260   pr #255: "______________________________  ____________  ________  ________  __________________  __________  ______________"
42280   let nofx=1
42300 !  a nokey on the read #fundmstr line above can cause and err 201 (return without gosub)  the nokey is the last error and the continue goes to the line after the unsuccessful read   Build the general ledger control file and put the fund names in it!!!!!!!!
42320   return  ! /r
