00010 ! Replace R:\acsCL\BankReconciliation
00020 ! Bank reconciliation routines
00030 ! ______________________________________________________________________
00040   library 'R:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fncno,fnerror,fndat,fndate_mmddyy_to_ccyymmdd,fntos,fnlbl,fncomboa,fncmdset,fnacs,fnchain,fntxt,fnbutton,fnfra,fnopt,fnflexinit1,fnflexadd1,fncmdkey,fnchk,fncombof,fnmsgbox,fncd
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cnam$*40,dat$*20,adr(2)
00080   dim ck$(22),cap$*128,aa(2)
00090   dim tr$(5)*35,de$*30,bn$*40,sn$*50
00100   dim aa(2),line$*132,resp$(10)*35,ml$(4)*60
00110   dim item1$(6)*40,item3$(9)*40,chdr$(5)*15,cmask$(5)*10,flxitm$(5)*15
00120   dim w(4),hdw$(4)*40
00130   dim io7$(9),dpt(3),io8$(25),dpd(5,5)
00140 ! ______________________________________________________________________
00150   let fntop(program$, cap$="Bank Reconciliation")
00160   let cancel=99 : let right=1
00170   let fncno(cno,cnam$)
00172   let fndat(dat$)
00190 ! let cd1=date("mmddyy")
00200   open #20: "Name=Q:\CLmstr\Company.h"&str$(cno)&",Shr",internal,input,relative 
00202   read #20,using 'Form POS 150,X 2,N 2',rec=1,release: wbc
00204   close #20: 
00210   open #bankmstr:=12: "Name=Q:\CLmstr\BankMstr.H"&str$(cno)&",KFName=Q:\CLmstr\BankIdx1.H"&str$(cno)&",Shr",internal,outin,keyed 
00220   open #paymstr1:=13: "Name=Q:\CLmstr\PayMstr.H"&str$(cno)&",KFName=Q:\CLmstr\PayIdx1.H"&str$(cno)&",Shr",internal,outin,keyed 
00230   open #paymstr2:=14: "Name=Q:\CLmstr\PayMstr.H"&str$(cno)&",KFName=Q:\CLmstr\PayIdx2.H"&str$(cno)&",Shr",internal,outin,keyed 
00240   open #trmstr1:=1: "Name=Q:\CLmstr\TrMstr.H"&str$(cno)&",KFName=Q:\CLmstr\TrIdx1.H"&str$(cno)&",Shr",internal,outin,keyed 
00250   open #trmstr2:=2: "Name=Q:\CLmstr\TrMstr.H"&str$(cno)&",KFName=Q:\CLmstr\TrIdx2.H"&str$(cno)&",Shr",internal,outin,keyed 
00260   open #tralloc:=3: "Name=Q:\CLmstr\TrAlloc.H"&str$(cno)&",Shr",internal,outin,relative 
00270   read #bankmstr,using 'Form POS 3,C 40',key=cnvrt$("N 2",wbc),release: bn$
00280   restore #bankmstr: 
00290 MENU1: ! 
00300   close #81: ioerr L310
00310 L310: open #81: "Name=Q:\CLmstr\Bank"&str$(wbc)&".H"&str$(cno)&",Use,RecL=32",internal,outin,relative 
00315   if lrec(81)>0 then !:
          read #81,using "Form POS 1,N 6,2*PD 5.2,N 6",rec=1: stmtdt,bgbal,stmtbal,codt conv L320 : goto BANK_STMT_INFO
00320 L320: if lrec(81)>0 then !:
          read #81,using "Form POS 1,N 6,2*PD 7.2,N 6",rec=1: stmtdt,bgbal,stmtbal,codt !:
        else !:
          write #81,using "Form POS 1,N 6,2*PD 7.2,N 6": stmtdt,bgbal,stmtbal,codt
00330 BANK_STMT_INFO: ! 
00340   let fntos(sn$="bankrec-1") !:
        let respc=0 !:
        let mylen=30 : let mypos=mylen+2
00350   let fnlbl(1,1,"Bank Code:",mylen,right)
00360   let fncombof("Bankmstr",1,mypos,35,"Q:\CLmstr\bankmstr.h"&str$(cno),1,2,3,30,"Q:\CLmstr\BankIdx1.h"&str$(cno),0,0, "",0) !:
        let resp$(respc+=1)=str$(wbc)
00370   let fnlbl(3,1,"Statement Date:",mylen,right)
00380   let fntxt(3,mypos,10,0,1,"3",0,"We suggest you always use the month end date.") !:
        let resp$(respc+=1)=str$(stmtdt)
00390   let fnlbl(4,1,"Previous Statement Balance:",mylen,right)
00400   let fntxt(4,mypos,14,0,1,"10",0,"Pull this information directly from your current bank statement. ") !:
        let resp$(respc+=1)=str$(bgbal)
00410   let fnlbl(5,1,"Current Statement Balance:",mylen,right)
00420   let fntxt(5,mypos,14,0,1,"10",0," ") !:
        let resp$(respc+=1)=str$(stmtbal)
00430   let fnlbl(6,1,"Reconciliation Cutoff Date:",mylen,right)
00440   let fntxt(6,mypos,10,0,1,"3",0,"The cutoff date would normally be the last day of the month.") !:
        let resp$(respc+=1)=str$(codt)
00450   let fnlbl(8,1,"Reconciliation Options:",28,right)
00460   let item1$(1)="Enter Cleared Checks" !:
        let item1$(2)="Enter Cleared Deposits" !:
        let item1$(3)="Clear All Adjustments" !:
        let item1$(4)="Calculate Bank Totals" !:
        let item1$(5)="Print Reconciliation Listing" !:
        let item1$(6)="Make Corrections"
00470   let fncomboa("bankrec-1",8,30,mat item1$,"Select the funtion you would like to perform.  Normally you would clear the checks, deposits, adjustments and then calculate the bank totals") !:
        let resp$(respc+=1)=item1$(1)
00480   let fncmdkey("&Display Balances",3,0,0,"Displays previous balances for this bank account.") !:
        let fncmdkey("&Next",1,1,0,"Proceed to next options") !:
        let fncmdkey("&Cancel",5,0,1,"Returns to main menu")
00490   let fnacs(sn$,0,mat resp$,ck)
00500   if ck=5 then goto XIT
00510   let wbc=val(resp$(1)(1:2)) ! working bank code
00520   if ck=3 then goto MENU1 ! redisplay balances
00530   read #bankmstr,using 'Form POS 3,C 40',key=cnvrt$("N 2",wbc),release: bn$
00540   let stmtdt=val(resp$(2)(5:6))*10000+val(resp$(2)(7:8))*100+val(resp$(2)(3:4)) ! convert date back to mmddyy format
00550   let bgbal=val(resp$(3))
00560   let stmtbal=val(resp$(4))
00570   let codt=val(resp$(5)(5:6))*10000+val(resp$(5)(7:8))*100+val(resp$(5)(3:4)) ! convert date back to mmddyy format
00580   for j=1 to 6
00590     if resp$(6)=item1$(j) then let ti3=j: goto L610
00600   next j
00610 L610: if ti3=6 then let fnchain("R:\acsCL\Transaction")
00620   close #81: ioerr L630
00630 L630: open #81: "Name=Q:\CLmstr\Bank"&str$(wbc)&".H"&str$(cno)&",Use,RecL=32",internal,outin,relative 
00640   if lrec(81)=0 then write #81,using "Form POS 1,N 6,2*PD 7.2,N 6",rec=1: stmtdt,bgbal,stmtbal,codt : goto L660
00650   rewrite #81,using "Form POS 1,N 6,2*PD 7.2,N 6",rec=1: stmtdt,bgbal,stmtbal,codt
00660 L660: close #81: ioerr L670
00670 L670: if ti3<3 then let wtt=ti3
00680   on ti3 goto CLEARING_OPTIONS,CLEARING_OPTIONS,CLEARING_ADJUSTMENTS,CALCULATE_TOTALS,PRINT_LISTINGS none MENU1
00690 ! ______________________________________________________________________
00700 CLEARING_ADJUSTMENTS: ! 
00710   let fntos(sn$="bankrec-11") !:
        let respc=0
00720   let fnlbl(1,60,"",1,0)
00730   let fnlbl(1,1,"If the adjustments have been previously cleared",50,0)
00740   let fnlbl(2,1,"with a wrong date, then enter the previously",50,0)
00750   let fnlbl(3,1,"cleared date (mmddyy) (otherwise leave blank):",50,0)
00760   let fntxt(3,48,8,0,1,"1",0,"") !:
        let resp$(respc+=1)=""
00770   let fncmdset(2): let fnacs(sn$,0,mat resp$,ck)
00780   if ck=5 or ck=cancel then goto MENU1
00790   let pcd1=val(resp$(1)) ! clear old adjustments
00800   restore #trmstr1,key>=cnvrt$("N 2",wbc)&"3        ": nokey MENU1
00810 L810: read #trmstr1,using 'Form POS 1,N 2,N 1,POS 12,N 6,POS 72,N 6': bank_code,tcde,d1,clr eof MENU1
00820   if tcde><3 then goto MENU1
00830   if bank_code><wbc then goto MENU1
00840   if fndate_mmddyy_to_ccyymmdd(d1)>fndate_mmddyy_to_ccyymmdd(stmtdt) then goto L810
00850   if clr><pcd1 then goto L810
00860   rewrite #trmstr1,using 'Form POS 1,N 2,N 1,POS 12,N 6,POS 72,N 6': bank_code,tcde,d1,stmtdt eof MENU1
00870   goto L810
00880 ! ______________________________________________________________________
00890 CLEARING_OPTIONS: ! 
00900   let fntos(sn$="bankrec-2") !:
        let respc=0
00910   let fnlbl(1,1,"Reference Number to Clear:",28,1)
00920   let fntxt(1,31,8,0,1,"",0,"If you wish to clear one transaction at a time, enter the reference # here.") !:
        let resp$(respc+=1)=""
00930   let fnlbl(2,1,"Cleared Amount:",28,1)
00940   let fntxt(2,31,12,0,1,"10",0,"You do not have to enter the amount.  If you do, it will compare the amount you enter to your check history and warn you of any differences. ") !:
        let resp$(respc+=1)=""
00950   if ti3=1 then let fnbutton(1,50,"Clear &Range of Checks",60,"This option allows you to clear a range of checks without having to enter each check number",0,20)
00960   if ti3=2 then let fnbutton(1,50,"Clear Deposits by &Amount",61,"This option allows you to clear deposits by entering the amount without having to know the reference number.",0,30)
00970   if ti3=2 then let fnbutton(3,50,"Clear Deposits by Date &Range",62,"This option allows you to clear deposits by entering a date range.",0,30)
00980   if ti3=2 then let fnbutton(5,50,"Clear Deposits from &List",63,"This option allows you to clear deposits from a listing of all outstanding deposits.",0,30)
00990   if ti3=1 then let fnbutton(3,50,"Clear Checks from &List",64,"This option allows you to clear checks from a listing of all outstanding checks.",0,20)
01000   let fncmdset(2): let fnacs(sn$,0,mat resp$,ck)
01010   if ck=5 then goto MENU1
01020   let k$=resp$(1) ! check # to clear
01030   if ck=61 then !:
          let ti=2: let tcde=wtt=ti3: let ck$=k$ !:
          let tr3=pcde=0 !:
          goto CLEAR_DEPOSITS_BY_AMOUNT
01040   if ti3=1 and ck=60 then goto L2790 ! clear range of checks
01050 ! If TI3=2 AND CK=3 Then Goto 4440 ! clear deposits by amount
01060   if ti3=2 and ck=62 then goto L2790 ! deposits by date range
01070   if ti3=2 and ck=63 then goto CLEAR_TRANSACTIONS_FROM_LIST ! deposits from listing of all outstanding deposits
01080   if ti3=1 and ck=64 then goto CLEAR_TRANSACTIONS_FROM_LIST ! clear checks from listing
01090   if ck=5 or ck=cancel then goto MENU1
01100   if rtrm$(k$)="" then goto CLEARING_OPTIONS
01110   let ck$=lpad$(rtrm$(k$),8)
01120   let k$=cnvrt$("N 2",wbc)&str$(ti3)&ck$
01130   read #trmstr1,using 'Form POS 1,N 2,N 1,C 8,G 6,pd 10.2,C 8,C 35,N 1,N 6,N 1',key=k$,release: bank_code,tcde,tr$(1),tr$(2),tx3,tr$(4),tr$(5),pcde,clr,scd nokey L1190 !:
        let tr$(3)=str$(tx3)
01140   if val(tr$(3))=val(resp$(2)) or val(resp$(2))=0 then goto L1210
01150   mat ml$(3) !:
        let ml$(1)="Cleared amount does not agree with your check history!" !:
        let ml$(2)="Any corrections to your data must be done through" !:
        let ml$(3)="the check history option. OK to clear; Cancel to skip" !:
        let fnmsgbox(mat ml$,resp$,cap$,49)
01170   if resp$="OK" then goto L1210
01180   if resp$="Cancel" then goto CLEARING_OPTIONS
01190 L1190: mat ml$(1) !:
        let ml$(1)="Reference # "&k$(4:11)&" could not be found.  Retry!" !:
        let fnmsgbox(mat ml$,resp$,cap$,48)
01200   goto CLEARING_OPTIONS
01210 L1210: if clr>0 then goto ALREADY_CLEARED
01220   rewrite #trmstr1,using 'Form POS 72,N 6',key=k$: stmtdt
01230   goto CLEARING_OPTIONS
01240 ALREADY_CLEARED: ! 
01250   let fntos(sn$="bankrec-8") !:
        let respc=0
01260   let fnlbl(1,1,"Check # "&trim$(k$(4:11))&" was previously cleared on "&cnvrt$("PIC(ZZ/ZZ/ZZ)",clr)&"!",60,1)
01270   let fnlbl(2,1,"Correct cleared date:",30,1)
01280   let fntxt(2,33,10,0,1,"1",0,"Enter the new date if cleared in error on another date.  Use the old cleared date if correct.  Leave blank to unclear. ") !:
        let resp$(respc+=1)=str$(stmtdt)
01290   let fncmdset(2): let fnacs(sn$,0,mat resp$,ck)
01300   if ck=5 or ck=cancel then goto CLEARING_OPTIONS
01310   let cdte=val(resp$(1)) ! statement date cleared
01320   if cdte=0 then goto L1330
01330 L1330: rewrite #trmstr1,using 'Form POS 72,N 6',key=k$: cdte
01340   goto CLEARING_OPTIONS
01350 CALCULATE_TOTALS: ! 
01360   restore #trmstr1,key>=cnvrt$("N 2",wbc)&"         ": nokey MENU1
01370   mat t1=(0)
01380 L1380: read #trmstr1,using 'Form POS 1,N 2,N 1,C 8,G 6,pd 10.2,C 8,C 35,N 1,N 6,N 1',release: bank_code,tcde,tr$(1),tr$(2),tx3,tr$(4),tr$(5),pcde,clr,scd eof L1600 !:
        let tr$(3)=str$(tx3)
01390   if bank_code><wbc then goto L1600
01400   let amt=val(tr$(3)) conv L1380
01410   if amt=0 then goto L1380
01420   if codt=0 then goto L1450
01430   let x=val(tr$(2)) conv L1380
01440   if fndate_mmddyy_to_ccyymmdd(x)>fndate_mmddyy_to_ccyymmdd(codt) then goto L1380
01450 L1450: if stmtdt=clr then goto L1530
01460   if clr=0 then goto L1480
01470   if fndate_mmddyy_to_ccyymmdd(clr)<fndate_mmddyy_to_ccyymmdd(stmtdt) then goto L1380
01480 L1480: on tcde goto L1490,L1500,L1510,L1490 none L1580 ! NOT CLEARED
01490 L1490: let t1(8)=t1(8)+1: let t1(9)=t1(9)+amt : let w0=2 : goto L1580 ! NOT CLEARED CHECKS
01500 L1500: let t1(6)=t1(6)+1: let t1(7)=t1(7)+amt : let w0=1 : goto L1580 ! NOT CLEARED DEPOSITS
01510 L1510: if amt>0 then goto L1500
01520   let amt=-amt : goto L1490
01530 L1530: on tcde goto L1540,L1550,L1560,L1540 none L1580
01540 L1540: let t1(1)=t1(1)+1: let t1(2)=t1(2)+amt : let w0=4: goto L1580 ! CLEARED CHECKS
01550 L1550: let t1(3)=t1(3)+1: let t1(4)=t1(4)+amt : let w0=3: goto L1580 ! CLEARED DEPOSTIS
01560 L1560: if amt>0 then goto L1550
01570   let amt=-amt: goto L1540
01580 L1580: if ti3=5 then gosub L2550
01590   goto L1380
01600 L1600: if ti3><5 then goto BANKTOTALSCREEN
01610   let fnopenprn
01620   for j=1 to 4
01630     let wp=j+50
01640     if w(j)=0 then goto WPNEXTJ
01650     print #wp,using L1660: t2(j)
01660 L1660: form pos 19,"  ----------",skip 1,pos 19,n 12.2,skip 1
01670 ! If NW=1 Then Print #WP: NEWPAGE
01680     close #wp: 
01690     open #wp: "Name="&env$('temp')&"\RPT"&str$(j)&"."&wsid$,display,input 
01700 L1700: linput #wp: line$ eof EO_WP
01710     print #255: line$
01720     goto L1700
01730 EO_WP: ! 
01740     print #255: newpage
01750     close #wp: 
01760 WPNEXTJ: next j
01770   let fncloseprn
01780   goto MENU1
01790 BANKTOTALSCREEN: ! 
01800   let fntos(sn$="bankrec-7") !:
        let respc=0
01810   let fnlbl(1,1, rtrm$(bn$(1:30)),45,2)
01820   let fnlbl(3,1,"   Statement Date:      "&ltrm$(cnvrt$("PIC(##/##/##)",stmtdt)),45,1)
01830   let fnlbl(4,1,"Previous Statement Balance:"&cnvrt$("pic(----,---,---.##)",bgbal),45,1)
01840   let fnlbl(5,1,cnvrt$("N 5",t1(3))&" Deposit(s) Cleared:"&cnvrt$("pic(----,---,---.##)",t1(4)),45,1)
01850   let fnlbl(6,1, cnvrt$("N 5",t1(1))&" Withdrawal(s) Cleared:"&cnvrt$("pic(----,---,---.##)",t1(2)),45,1)
01860   let t1(5)=bgbal+t1(4)-t1(2)
01870   let fnlbl(7,1, "Calculated Statement Balance:"&cnvrt$("pic(----,---,---.##)",t1(5)),45,1)
01880   let fnlbl(8,1, "Bank Statement Balance:"& cnvrt$("pic(----,---,---.##)",stmtbal),45,1)
01890   let fnlbl(9,1, "Out of Balance Amount:"&cnvrt$("pic(----,---,---.##)",stmtbal-t1(5)),45,1)
01900   let fnlbl(10,1, "Uncleared Deposit(s):"&cnvrt$("pic(----,---,---.##)",t1(7)),45,1)
01910   let fnlbl(11,1, cnvrt$("N 5",t1(8))&" Uncleared Withdrawal(s):"&cnvrt$("pic(----,---,---.##)",t1(9)) ,45,1)
01920   let t1(10)=bgbal+t1(7)+t1(4)-t1(9)-t1(2)
01930   let fnlbl(12,1,"Calculated Book Balance:"&cnvrt$("pic(----,---,---.##)",t1(10)) ,45,1)
01940   let fncloseprn
01950   let cutoffbal=bal
01960   restore #trmstr1,key>=cnvrt$("N 2",wbc)&"         ": nokey EO_ADDING_BALANCE
01970 L1970: read #trmstr1,using 'Form POS 1,N 2,N 1,C 8,G 6,pd 10.2,C 8,C 35,N 1,N 6,N 1': bank_code,tcde,tr$(1),tr$(2),tx3,tr$(4),tr$(5),pcde,clr,scd eof EO_ADDING_BALANCE !:
        let tr$(3)=str$(tx3)
01980   if bank_code><wbc then goto L1970
01990   let x=val(tr$(2)) conv L1970
02000   if fndate_mmddyy_to_ccyymmdd(x)=<fndate_mmddyy_to_ccyymmdd(codt) then goto L1970
02010   if tcde=1 then let cutoffbal=cutoffbal+val(tr$(3)) ! add checks back
02020   if tcde=2 then let cutoffbal=cutoffbal-val(tr$(3)) ! subtract deposits
02030   if tcde=3 then let cutoffbal=cutoffbal-val(tr$(3)) ! adjustments
02040   goto L1970
02050 EO_ADDING_BALANCE: ! 
02060   let fnlbl(11,1,"Book Balance as of"&cnvrt$("PIC(ZZ/ZZ/ZZ)",codt)&":"&cnvrt$("pic(----,---,---.##)",cutoffbal),45,1)
02070   let fncmdset(3): let fnacs(sn$,0,mat resp$,ck)
02080   if ck=5 then goto MENU1
02090   if ck=1 then gosub L2120 : goto BANKTOTALSCREEN
02100   goto BANKTOTALSCREEN
02110 ! ______________________________________________________________________
02120 L2120: let fnopenprn
02125   print #255,using L2140: cnam$
02130   print #255,using L2140: bn$(1:30)
02140 L2140: form pos 20,cc 40
02150   print #255,using L2140: "Bank Reconciliation"
02160   print #255,using L2140: "Statement Date: "&cnvrt$("PIC(ZZ/ZZ/ZZ)",stmtdt)
02170   print #255: ""
02180   print #255: "Previous Statement Balance: "&cnvrt$("N 11.2",bgbal)
02190   print #255: cnvrt$("N 5",t1(3))&" Deposits Total:   "&cnvrt$("N 15.2",t1(4))
02200   print #255: cnvrt$("N 5",t1(1))&" Withdrawals Total: "&cnvrt$("N 14.2",t1(2))
02210   print #255: "Bank Statement Balance:"&cnvrt$("N 16.2",stmtbal)
02220   print #255: "Calculated Statement Bal:"&cnvrt$("N 14.2",t1(5))
02230   print #255: "Out of Balance Amount:"&cnvrt$("N 17.2",stmtbal-t1(5))
02240   print #255: ""
02250   print #255: cnvrt$("N 5",t1(6))&" Uncleared Deposits:"&cnvrt$("N 14.2",t1(7))
02260   print #255: cnvrt$("N 5",t1(8))&" Uncleared Withdrawals:"&cnvrt$("N 11.2",t1(9))
02270   print #255: "Actual Bank Balance:"&cnvrt$("N 19.2",t1(10))
02280   return 
02290 ! ______________________________________________________________________
02300 PRINT_LISTINGS: ! 
02310   let hdw$(1)="Deposits not cleared as of "&cnvrt$("PIC(ZZ/ZZ/ZZ)",stmtdt)
02320   let hdw$(2)="Withdrawals not cleared as of "&cnvrt$("PIC(ZZ/ZZ/ZZ)",stmtdt)
02330   let hdw$(3)="Deposits cleared on "&cnvrt$("PIC(ZZ/ZZ/ZZ)",stmtdt)
02340   let hdw$(4)="Withdrawals cleared on "&cnvrt$("PIC(ZZ/ZZ/ZZ)",stmtdt)
02350   let fntos(sn$="bankrec-12") !:
        let respc=0 : mat resp$=('')
02360 ! Let FNFRA(1,1,4,60,"Select Listings to Print","Choose any of the four listing",0)
02370   let fnchk(1,3,hdw$(1),0) !:
        let resp$(respc+=1)=""
02380   let fnchk(2,3,hdw$(2),0) !:
        let resp$(respc+=1)=""
02390   let fnchk(3,3,hdw$(3),0) !:
        let resp$(respc+=1)=""
02400   let fnchk(4,3,hdw$(4),0) !:
        let resp$(respc+=1)=""
02410   let fncmdset(2): let fnacs(sn$,0,mat resp$,ck)
02420   if ck=5 or ck=cancel then goto MENU1
02430   if resp$(1)(1:1)="T" then let w(1)=1 else let w(1)=0
02440   if resp$(2)(1:1)="T" then let w(2)=1 else let w(2)=0
02450   if resp$(3)(1:1)="T" then let w(3)=1 else let w(3)=0
02460   if resp$(4)(1:1)="T" then let w(4)=1 else let w(4)=0
02470   let fncloseprn
02480   for j=1 to 4
02490     if w(j)=0 then goto L2520
02500     let wp=j+50
02510     open #wp: "Name="&env$('temp')&"\RPT"&str$(j)&"."&wsid$&",Size=0,RecL=132,Replace",display,output 
02520 L2520: ! 
02522   next j
02530   mat t2=(0)
02540   goto CALCULATE_TOTALS
02550 L2550: if w(w0)=0 then goto L2610
02560   let wp=w0+50
02570   if w(w0)=1 then gosub WP_HEADER
02580   print #wp,using L2590: tr$(1),val(tr$(2)),amt,tr$(5),clr pageoflow WP_PGOF
02590 L2590: form pos 1,c 10,pic(zz/zz/zz),n 12.2,x 2,c 37,pic(zz/zz/zz),skip 1
02600   let t2(w0)=t2(w0)+amt
02610 L2610: ! 
02620   return 
02630 WP_PGOF: ! r:
02632   print #wp: newpage
02640   gosub WP_HEADER
02650   continue  ! /r
02670 WP_HEADER: ! r:
02680 ! 
02700   print #wp,using L2710: date$('mm/dd/yy'),rtrm$(cnam$)&" - Bank Account # "&str$(wbc),time$," Reconciliation Listing",hdw$(w0),"Page",w(w0),dat$
02710 L2710: form skip 3,pos 1,c 8,cc 70,skip 1,pos 1,c 8,cc 70,skip 1,pos 9,cc 70,skip 1,pos 1,c 4,n 4,cc 70,skip 2
02720   print #wp: "Check or                                                              Date"
02730   print #wp: "Ref Numb    Date       Amount   Name or Description                  Cleared"
02740   print #wp: "________  ________  __________  ___________________________________  ________"
02750   let w(w0)=w(w0)+1
02760   return  ! /r
02770 ! 
02780 ! 
02790 L2790: let r1=2
02800 SCR_CLEAR_BY_RANGE: ! clear by range of check numbers or by date
02810   let fntos(sn$="bankrec-9")
02812   let respc=0
02820   let clear_from_range=1 ! return here from grid
02830   let fnlbl(1,43,"",1,0)
02840   if ti3=2 then goto L2900
02850   let fnlbl(1,1,"Lowest Check Number to Clear:",30,1)
02860   let fntxt(1,32,8,0,1,"",0,"If your bank statement lists checks in the order cleared and indicate breaks in the check numbers, it is very easy to clear checks using this feature.")
02862   let resp$(respc+=1)=""
02870   let fnlbl(2,1,"Highest Check Number to Clear:",30,1)
02880   let fntxt(2,32,8,0,1,"",0,"Enter the last check number in the series.")
02882   let resp$(respc+=1)=""
02890   goto L2940
02900 L2900: ! 
02902   let fnlbl(1,1,"Lowest Date to Clear:",30,1)
02910   let fntxt(1,32,10,0,1,"1",0,"Normally you would use beginning of the month, but you could back the date up a few days to get any uncleared from the previous month.")
02912   let resp$(respc+=1)=""
02920   let fnlbl(2,1,"Highest Date to Clear:",30,1)
02930   let fntxt(2,32,10,0,1,"1",0,"Normally you would use the last day of the month, but if the bank's cutoff date is different, you may want to use it.")
02932   let resp$(respc+=1)=""
02940 L2940: ! 
02942   let fncmdset(2): let fnacs(sn$,0,mat resp$,ck)
02950   if ck=5 or ck=cancel then goto MENU1
02960   let l1=val(resp$(1)) ! either lowest date or lowest check #
02970   let h1=val(resp$(2)) ! either highest date or highest check #
02980   if ti3=2 then 
02990     let l1=fncd(l1)
03000     let h1=fncd(h1)
03010   end if 
03012   if ti3=1 then let k$=cnvrt$("N 2",wbc)&str$(ti3)&lpad$(trim$(resp$(1)),8)
03020   if ti3=2 then let k$=cnvrt$("N 2",wbc)&str$(ti3)&lpad$(str$(0),8)
03030   restore #trmstr1,key>=k$: nokey NOTHING_IN_RANGE
03040   goto L3060
03050 NOTHING_IN_RANGE: ! r:
03052   mat ml$(1)
03054   let ml$(1)="Nothing found in this range."
03056   let fnmsgbox(mat ml$,resp$,cap$,48)
03058   goto SCR_CLEAR_BY_RANGE ! /r
03060 L3060: ! Print Fields "1,2,C 8,R,N": " Check #"   ! do I want some kind of grid here???? kj
03070 ! Print Fields "1,11,C 10,R,N": "  Amount"
03080 DO_CLEAR_BY_RANGE: ! r:
03082   read #trmstr1,using 'Form POS 1,N 2,N 1,C 8,G 6,pd 10.2,C 8,C 35,N 1,N 6,N 1': bank_code,tcde,tr$(1),tr$(2),tx3,tr$(4),tr$(5),pcde,clr,scd eof FINIS_DCBR
03084   let tr$(3)=str$(tx3)
03090   if bank_code><wbc or tcde><ti3 then goto SCR_CLEAR_BY_RANGE
03100   if ti3=1 then goto L3150
03110   if clr>0 then goto DO_CLEAR_BY_RANGE
03120   let tr2=fncd(val(tr$(2))) conv DO_CLEAR_BY_RANGE
03130   if tr2<l1 or tr2>h1 then goto DO_CLEAR_BY_RANGE
03140   goto L3170
03150 L3150: ! 
03152   let h2=val(tr$(1)) conv DO_CLEAR_BY_RANGE
03160   if h2>h1 then let displaycleared=1 : goto CLEAR_TRANSACTIONS_FROM_LIST ! Goto 3710
03170 L3170: ! 
03180   rewrite #trmstr1,using 'Form POS 72,N 6': stmtdt
03260   goto DO_CLEAR_BY_RANGE
03264 FINIS_DCBR: ! 
03268   goto DISPLAY_CLEARED_SO_FAR ! /r
03270 DISPLAY_CLEARED_SO_FAR: ! r: show what has been cleared
03280   let displaycleared=1 : goto CLEAR_TRANSACTIONS_FROM_LIST
03290   if ti><2 then goto DCSF_FINIS ! don't know what the following logic is; probably can delete
03300   if rtrm$(tr$(4))="" then goto DCSF_FINIS
03310   read #paymstr1,using 'Form POS 9,C 30,POS P1,PD 3',key=tr$(4),release: sn$,ta1 nokey DCSF_FINIS
03320   if rtrm$(tr$(4))="" then let adr=ta1=0 else let adr=ta1
03330   mat aa=(0)
03340   let gl$=salgl$=""
03350 L3350: if adr=0 then goto DCSF_FINIS
03360   read #payeeglbreakdown,using L3370,rec=adr: gl$,pct,de$,nta norec DCSF_FINIS
03370 L3370: form pos 9,c 12,pd 3.2,c 30,pd 3
03380   let amt=round(val(tr$(3))*pct*.01,2)
03390   if rtrm$(de$)="" then let de$=tr$(5)(1:30)
03400   let lr5=lrec(tralloc)+1
03410   write #tralloc,using 'Form POS 1,N 2,N 1,C 8,C 12,PD 5.2,C 30,N 6,X 3,C 12',rec=lr5: bank_code,tcde,tr$(1),gl$,amt,de$,0,""
03420   let tac=tac+amt
03430   if aa(1)=0 then let aa(1)=lr5
03440 ! If AA(2)>0 Then !:
        ! Rewrite #TRALLOC,Using 'Form POS 65,X 3',Rec=AA(2): LR5
03450   let aa(2)=lr5
03460 ! Mat TR=AA
03470 ! rewrite #TRMSTR1,Using 'Form POS 79,2*PD 3',Rec=CR1: MAT TR
03490   let ti=3
03500   print fields "7,46,C 18,R,N": cnvrt$("N 16.2",tac)
03510   let adr=nta
03520   goto L3350
03530 DCSF_FINIS: ! 
03532   return  ! /r
03540 FREE_DPAMNTS: ! r:
03550   execute "Free Q:\CLmstr\DpAmnts.H"&str$(cno)&" -n"
03560   continue  ! /r
03570 ! ______________________________________________________________________
03580 CLEAR_DEPOSITS_BY_AMOUNT: ! clear deposits by amounts
03590   if cda=1 then goto DPAMENU
03600 L3600: open #dpamnts=91: "Name=Q:\CLmstr\DpAmnts.H"&str$(cno)&",Use,RecL=20",internal,outin,relative ioerr FREE_DPAMNTS
03610   open #92: "Name=Q:\CLmstr\DpDates.H"&str$(cno)&",Use,RecL=150",internal,outin,relative 
03620   read #92,using L3630,rec=1: mat dpd norec L3650
03630 L3630: form pos 1,25*n 6
03640   goto L3660
03650 L3650: ! 
03652   write #92,using L3630,rec=1: mat dpd
03660 L3660: ! 
03662   open #93: "Name=Q:\CLmstr\DPTypes.H"&str$(cno)&",Use,RecL=3",internal,outin,relative 
03670   read #93,using F_DPTYPES,rec=1: mat dpt norec L3700
03680 F_DPTYPES: form pos 1,3*n 1
03690   goto L3710
03700 L3700: write #93,using F_DPTYPES,rec=1: mat dpt
03710 L3710: let cda=1
03720 ! __________________________________________
03730 DPAMENU: ! 
03740   let fntos(sn$="bankrec-5")
03742   let respc=0
03750   let fnfra(1,1,9,60,"Reconciliation Options","Used to clear deposita by amounts and not by reference number",0)
03752   let frame=1
03760   let fnopt(1,3,"Erase Previous Amounts & Dates",0,frame) !:
        if sel_code=1 or sel_code=0 then let resp$(respc+=1)="True" else !:
          let resp$(respc+=1)="False"
03770   let fnopt(2,3,"Enter Deposit Amounts",0,frame) !:
        if sel_code=2 then let resp$(respc+=1)="True" else !:
          let resp$(respc+=1)="False"
03780   let fnopt(3,3,"Enter Types to Add Together",0,frame) !:
        if sel_code=3 then let resp$(respc+=1)="True" else !:
          let resp$(respc+=1)="False"
03790   let fnopt(4,3,"Enter Dates to Add Together",0,frame) !:
        if sel_code=4 then let resp$(respc+=1)="True" else !:
          let resp$(respc+=1)="False"
03800   let fnopt(5,3,"Clear Matches",0,frame) !:
        if sel_code=5 then let resp$(respc+=1)="True" else !:
          let resp$(respc+=1)="False"
03810   let fnopt(6,3,"Print listing of Un-Matched",0,frame) !:
        if sel_code=6 then let resp$(respc+=1)="True" else !:
          let resp$(respc+=1)="False"
03820   let fnopt(7,3,"Print Listing of All Entries",0,frame) !:
        if sel_code=7 then let resp$(respc+=1)="True" else !:
          let resp$(respc+=1)="False"
03830   let fnopt(8,3,"Make Corrections",0,frame) !:
        if sel_code=8 then let resp$(respc+=1)="True" else !:
          let resp$(respc+=1)="False"
03840 ! Let FNCOMBOA("bankrec-3",3,30,MAT ITEM3$,"Select the funtion you would like to perform.  Normally you would Erase Previous, Enter Amounts, Clear Matches and check your totals")!!:
        let resp$(respc+=1)=item3$(1)
03850   let resp$(2)="True"
03860   if t1 =0 and t2=0 then goto L3890
03870   let fnlbl(12,1,"Deposits Entered: "&cnvrt$("pic(ZZ,ZZZ,ZZZ.##)",t1),40,0)
03880   let fnlbl(13,1,"Deposits Cleared: "&cnvrt$("pic(ZZ,ZZZ,ZZZ.##)",t2),40,0)
03890 L3890: let fncmdset(2): let fnacs(sn$,0,mat resp$,ck)
03900   if ck=5 or ck=cancel then goto MENU1
03910   for j=1 to 8
03920     if resp$(j)(1:1)="T" then let sel_code=ti1=j: goto L3940
03930   next j
03940 L3940: if ti1=3 or ti1=4 then goto DPAMENU ! these two options not finished  kj
03950   on ti1 goto L3960,L4000,DPTYPES,DPDATES,L5260,PRINT_EDITS,PRINT_EDITS,COR1 none DPAMENU
03960 L3960: close #dpamnts,free: ioerr L3970
03970 L3970: close #92,free: ioerr L3980
03980 L3980: close #93: ioerr L3990
03990 L3990: goto L3600
04000 L4000: if lrec(91)=0 then goto ENTER_DEPOSITS_CLEARED
04010   read #dpamnts,using L4120,rec=lrec(91): tr$,am1,ti$
04020 ENTER_DEPOSITS_CLEARED: ! 
04030   let fntos(sn$="bankrec-4") !:
        let respc=0
04040   let fnlbl(1,1,"Amount to Clear:",25,1)
04050   let fntxt(1,28,12,0,1,"10",0,"Enter each deposit amount that shows as cleared on the bank statement.") !:
        let resp$(respc+=1)=""
04060   if am1>0 then let fnlbl(2,1,"Last Amount Entered:"&cnvrt$("N 13.2",am1),38,1)
04070   let fncmdset(2): let fnacs(sn$,0,mat resp$,ck)
04080   if ck=5 then goto PRINT_EDITS
04090   let am1=val(resp$(1)) ! deposit amount entered
04100   if am1=0 then goto PRINT_EDITS
04110   write #dpamnts,using L4120: "",am1,""
04120 L4120: form pos 1,c 8,pd 10.2,c 2
04130   goto ENTER_DEPOSITS_CLEARED
04140 PRINT_EDITS: let t1=t2=0
04150   let fnopenprn
04160   if ti1=6 then print #255,using "form pos 10,cc 60": "Uncleared Entries"
04170   if ti1=7 then print #255,using "form pos 10,cc 60": "Listing of All Entries Entered For Clearing"
04180   for j=1 to lrec(91)
04190     read #dpamnts,using L4120,rec=j: tr$,am1,ti$
04200     if am1=0 then goto L4270
04210     if ti1<6 then goto L4250
04220     if rtrm$(tr$)><"" and ti1=6 then goto L4270
04230     print #255,using L4240: j,tr$,am1,ti$
04240 L4240: form pos 1,n 4,x 2,c 10,n 10.2,x 2,c 2,skip 1
04250 L4250: let t1=t1+am1
04260     if rtrm$(tr$)><"" then let t2=t2+am1
04270 L4270: next j
04280   if ti1<6 then goto DPAMENU
04290   print #255,using L4300: "__________",t1,"=========="
04300 L4300: form pos 17,c 10,skip 1,pos 17,n 10.2,skip 1,pos 17,c 10,skip 1
04310   let fncloseprn
04320   goto DPAMENU
04330 DPTYPES: ! ENTER TYPES TO TOTAL   ! fix  KJ
04340   for j=1 to 3
04350     let io7$(j)=str$(j*2+11)&",40,N 1,UT,N"
04360   next j
04370   close #101: ioerr L4380
04380 L4380: open #101: "SROW=04,SCOL=10,EROW=19,ECOL=73,Border=Sr,CAPTION=<Deposit Types to Total",display,outin 
04390   print fields "5,11,C 62": "This is designed to take care of two or more types of Credit"
04400   print fields "6,11,C 62": "Cards that are totaled together by the card company before"
04410   print fields "7,11,C 62": "they are transmitted to your bank for deposit."
04420   print fields "09,11,c 62": "credit card deposits begin with a prefix of c1,c2, or c3."
04430   print fields "10,11,c 62": "place a 1 by the types to be added together for 1 bank deposit"
04440   for j=1 to 3
04450     print fields str$(j*2+11)&",37,C 3": "C"&str$(j)
04460   next j
04470   print fields "20,30,C 09,B,1": "Next (F1)"
04480 L4480: rinput fields mat io7$: mat dpt conv CONV7
04490   if ce>0 then let io7$(ce)(ce1:ce2)="U": let ce=0
04500   if ck>0 then goto L4570 else let ce=curfld
04510 L4510: let ce=ce+1: if ce>udim(io7$) then let ce=1
04520 L4520: let io7$(ce)=rtrm$(uprc$(io7$(ce))) : let ce1=pos(io7$(ce),"U",10) : if ce1=0 then goto L4510
04530   let ce2=ce1+1 : let io7$(ce)(ce1:ce1)="UC" : goto L4480
04540 CONV7: if ce>0 then let io7$(ce)(ce1:ce2)="U"
04550   let ce=cnt+1
04560   print fields "24,78,C 1": bell : goto L4520
04570 L4570: rewrite #93,using F_DPTYPES,rec=1: mat dpt
04580   goto DPAMENU
04590 ! ______________________________________________________________________
04600 DPDATES: ! r: ENTER DATES TO TOTAL
04610   close #108: ioerr L4620
04620 L4620: open #108: "SROW=03,SCOL=10,EROW=23,ECOL=73,Border=Sr,CAPTION=<Credit Card Dates to be Totaled",display,outin 
04630   print #108: newpage
04640   for j=1 to 5
04650     let io8$(j)="14,"&str$(j*9+15)&",N 6,UT,N"
04660     let io8$(j+5)="16,"&str$(j*9+15)&",N 6,UT,N"
04670     let io8$(j+10)="18,"&str$(j*9+15)&",N 6,UT,N"
04680     let io8$(j+15)="20,"&str$(j*9+15)&",N 6,UT,N"
04690     let io8$(j+20)="22,"&str$(j*9+15)&",N 6,UT,N"
04700   next j
04710   print fields "4,11,c 62": "This is designed to take care of two or more dates on Credit"
04720   print fields "5,11,c 62": "Card batched totaled together by the credit card company"
04730   print fields "6,11,c 62": "before they are transmitted to your bank for deposit."
04740   print fields "08,11,c 62": "this usually occurs on weekends or when a batch is transmitted"
04750   print fields "09,11,c 62": "after 8pm (Eastern time) one day and before 8pm the next."
04760   print fields "11,11,C 62": "You may have 5 different entries with up to 5 Dates per entry"
04770   for j=1 to 5
04780     print fields str$(j*2+12)&",16,C 50": "Entry "&str$(j)&"        +        +        +        +"
04790   next j
04800   print fields "13,24,C 42,N": "         (use only mmddyy format)"
04810   print fields "24,35,C 09,B,1": "Next (F1)"
04820 L4820: rinput fields mat io8$: mat dpd conv ERR8
04830   if ce>0 then let io8$(ce)(ce1:ce2)="U": let ce=0
04840   if ck>0 then goto L4910 else let ce=curfld
04850 L4850: let ce=ce+1: if ce>udim(io8$) then let ce=1
04860 L4860: let io8$(ce)=rtrm$(uprc$(io8$(ce))) : let ce1=pos(io8$(ce),"U",10) : if ce1=0 then goto L4850
04870   let ce2=ce1+1 : let io8$(ce)(ce1:ce1)="UC" : goto L4820
04880   if ce>0 then let io8$(ce)(ce1:ce2)="U"
04890   let ce=cnt+1
04900 ERR8: print fields "24,78,C 1": bell : goto L4860
04910 L4910: let dpd$=cnvrt$("PIC(######)",stmtdt)
04920   let mo2=val(dpd$(1:2))
04930 ! let da2=val(dpd$(3:4))
04940   let yr2=val(dpd$(5:6))
04950   for ce=1 to 25
04960     let j1=int((ce-1)/5)+1
04970     let j2=ce-(j1-1)*5
04980     if dpd(j1,j2)=0 then goto L5090
04990     let dpd$=cnvrt$("PIC(######)",dpd(j1,j2))
05000     let mo1=val(dpd$(1:2))
05010     let da1=val(dpd$(3:4))
05020     let yr1=val(dpd$(5:6))
05030     if mo1<01 or mo1>12 then goto ERR8
05040     if mo1=mo2 and yr1=yr2 then goto L5080 ! SAME MONTH SAME YEAR
05050     if yr1=yr2 and mo1+1=mo2 then goto L5080 ! PREVIOUS MONTH SAME YEAR
05060     if yr1+1><yr2 then goto ERR8
05070     if mo2=1 and mo1=12 then goto L5080 else goto ERR8
05080 L5080: if da1<01 or da1>31 then goto ERR8
05090 L5090: next ce
05100   let ce=0
05110   rewrite #92,using L3630,rec=1: mat dpd
05120   goto DPAMENU ! /r
05130 ! ______________________________________________________________________
05140 COR1: print newpage
05150   print fields "10,10,c 50": "Item Number to correct:"
05160   print fields "12,35,C 10,B,99": "Stop (Esc)"
05170 L5170: input fields "10,50,Nz 4,UT,N": cr1 conv L5170
05180   if cr1=0 or ck=5 then goto DPAMENU
05190   if cr1<1 or cr1>lrec(91) then goto L5170
05200   read #dpamnts,using L4120,rec=cr1: tr$,am1,ti$
05210   print fields "10,10,C 60": "Correct Amount (0 to Delete):"
05220   rinput fields "10,50,N 10.2,UT,N": am1
05230   rewrite #dpamnts,using L4120,rec=cr1: tr$,am1,ti$
05240   goto COR1
05250 ! ______________________________________________________________________
05260 L5260: restore #trmstr1,key>=lpad$(str$(wbc),2)&"2        ": nokey DPAMENU
05270 L5270: read #trmstr1,using 'Form POS 1,C 11,G 6,pd 10.2,POS 72,N 6': k$,tr2,tr3,clr eof L5980
05280   if val(k$(1:2))><wbc then goto L5980
05290   if val(k$(3:3))><2 then goto L5980
05300   if clr><0 then goto L5270
05310   if fndate_mmddyy_to_ccyymmdd(tr2)>fndate_mmddyy_to_ccyymmdd(stmtdt) then goto L5270
05320   for r1=1 to lrec(91)
05330     read #dpamnts,using L4120,rec=r1: tr$,am1,ti$
05340     if am1=0 then goto L5960
05350     if rtrm$(tr$)><"" then goto L5960
05360     if k$(4:5)<"C1" or k$(4:5)>"C3" then goto L5910
05370     let ct1=val(k$(5:5))
05380 ! CCADD: !
05390     let ckd=am3=0
05400     let ckd1=val(k$(6:11)) conv L5910
05410     for j1=1 to 5
05420       for j2=1 to 5
05430         if dpd(j1,j2)=0 then goto L5450
05440         if dpd(j1,j2)=ckd1 then let ckd=j1: goto L5470
05450 L5450: next j2
05460     next j1
05470 L5470: if dpt(ct1)=0 and ckd=0 then goto L5910
05480     if dpt(ct1)=1 then goto L5520
05490     let am3=tr3
05500     let k2$=k$
05510     goto L5570
05520 L5520: for j=1 to 3
05530       if dpt(j)=0 then goto L5650
05540       let k2$=k$(1:4)&str$(j)&k$(6:11)
05550       read #trmstr1,using 'Form POS 1,C 11,G 6,pd 10.2,POS 72,N 6',key=k2$: k2$,tr2,am2,clr nokey L5570
05560       if clr=0 then let am3=am3+am2
05570 L5570: if ckd=0 then goto L5650
05580       for j1=1 to 5
05590         if dpd(ckd,j1)=0 then goto L5640
05600         if j1=j2 then goto L5640
05610         let k2$=k2$(1:5)&cnvrt$("PIC(######)",dpd(ckd,j1))
05620         read #trmstr1,using 'Form POS 1,C 11,G 6,pd 10.2,POS 72,N 6',key=k2$: k2$,tr2,am2,clr nokey L5640
05630         if clr=0 then let am3=am3+am2
05640 L5640: next j1
05650 L5650: if dpt(ct1)=0 then goto L5670
05660     next j
05670 L5670: if am3><am1 then goto L5880
05680     if dpt(ct1)=1 then goto L5720
05690     rewrite #trmstr1,using 'Form POS 72,N 6',key=k$: stmtdt
05700     let k2$=k$
05710     goto L5770
05720 L5720: for j=1 to 3
05730       if dpt(j)=0 then goto L5850
05740       let k2$=k$(1:4)&str$(j)&k$(6:11)
05750       read #trmstr1,using 'Form POS 1,C 11,G 6,pd 10.2,POS 72,N 6',key=k2$: k2$,tr2,am2,clr nokey L5850
05760       if clr=0 then !:
              rewrite #trmstr1,using 'Form POS 72,N 6',key=k2$: stmtdt
05770 L5770: if ckd=0 then goto L5850
05780       for j1=1 to 5
05790         if dpd(ckd,j1)=0 then goto L5840
05800         if j1=j2 then goto L5840
05810         let k2$=k2$(1:5)&cnvrt$("PIC(######)",dpd(ckd,j1))
05820         read #trmstr1,using 'Form POS 1,C 11,G 6,pd 10.2,POS 72,N 6',key=k2$: k2$,tr2,am2,clr nokey L5840
05830         if clr=0 then !:
                rewrite #trmstr1,using 'Form POS 72,N 6',key=k2$: stmtdt
05840 L5840: next j1
05850 L5850: if dpt(ct1)=0 then goto L5870
05860     next j
05870 L5870: rewrite #dpamnts,using L4120,rec=r1: k$(4:11)
05880 L5880: read #trmstr1,using 'Form POS 1,C 11',key=k$: k$
05890     if am3=am1 then goto L5270
05900     goto L5960
05910 L5910: if tr3><am1 then goto L5960
05920     rewrite #dpamnts,using L4120,rec=r1: k$(4:11)
05930     rewrite #trmstr1,using 'Form POS 72,N 6',key=k$: stmtdt
05950     goto L5270
05960 L5960: next r1
05970   goto L5270
05980 L5980: goto DPAMENU
05990 ! ______________________________________________________________________
06000 ! <Updateable Region: ERTN>
06010 ERTN: let fnerror(cap$,err,line,act$,"xit")
06020   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
06030   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
06040   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
06050 ERTN_EXEC_ACT: execute act$ : goto ERTN
06060 ! /region
06070 IGNORE: continue 
44000 XIT: let fnxit
52000 CLEAR_TRANSACTIONS_FROM_LIST: ! r:
52020   let lastrec=nextrec=total=0
52040   let displayattop$="True"
52060   if ti3=1 then let type$="Checks" else let type$="Deposits"
52080   close #clearing: ioerr ignore
52100   open #clearing=89: "Name=Q:\CLmstr\clearing.H"&wsid$&",replace,RecL=33",internal,outin,relative 
52120   if ti3=2 then 
52140     restore #trmstr1,key>=lpad$(str$(wbc),2)&"2        ": nokey DISPLAY_GRID
52160   else 
52180     restore #trmstr1,key>=lpad$(str$(wbc),2)&"1        ": nokey DISPLAY_GRID
52200   end if 
52220 L6140: ! 
52240   read #trmstr1,using 'Form POS 1,C 11,G 6,pd 10.2,POS 72,N 6': k$,tr2,tr3,clr eof DISPLAY_GRID
52260   if tr3=0 then goto L6140 ! don't display zero amounts
52280   if val(k$(1:2))><wbc then goto L6140
52300   if ti3=1 and val(k$(3:3))><1 then goto L6140 ! only disbursments
52320   if displaycleared=1 and clr=stmtdt then goto L6200 ! display only cleared on this date
52340   if displaycleared=1 and clr<>stmtdt then goto L6140 ! if only cleared this clearing date, don't allow others to list
52360   if clr><0 then goto L6140
52380 L6200: ! 
52400   write #clearing,using 'Form POS 1,C 11,G 6,pd 10.2,N 6': k$,tr2,tr3,clr
52420   goto L6140
52440 DISPLAY_GRID: ! 
52460   mat chdr$(5)
52480   mat cmask$(5)
52500   mat flxitm$(5)
52520   let chdr$(1)="Rec"
52540   let chdr$(2)="Reference #"
52560   let chdr$(3)="Date"
52580   let chdr$(4)="Amount"
52600   let chdr$(5)="Cleared"
52620   let cmask$(1)='30'
52640   let cmask$(2)=''
52660   let cmask$(3)='1'
52680   let cmask$(4)='10'
52700   let cmask$(5)='1'
52720 L6240: ! 
52722   let fntos(sn$="bankrec-11")
52740   let respc=0 : mat resp$=('')
52760   let fnlbl(1,1,trim$(bn$(1:30))&"-"&type$,65,2)
52780   let fnchk(3,49,"Display at Top:",1)
52800   let resp$(respc+=1)=displayattop$ : let respc_display_top=respc : print 'set respc_display_top to ';respc_display_top
52820 ! 
52840   let fnflexinit1('Deposit-1',5,3,15,55,mat chdr$,mat cmask$,1) : print '__________init___________ displayattop$='&displayattop$&', nextrec='&str$(nextrec)
52860   restore #clearing: 
53000   if nextrec>0 and displayattop$="True" then 
53020     let fn_ctfl_add_grid_items(1)
53040     let fn_ctfl_add_grid_items(2)
53440   else ! display them all in indexed order
53460     let fn_ctfl_add_grid_items
53640   end if 
54000 ! 
54020   let respc_grid=2
54040 ! 
54060   let fnlbl(2,30,"Total Cleared:",16,1)
54080   let fnlbl(2,49,cnvrt$('pic($$$,$$$,$$$,$$#.##)',total),1)
54100 ! 
54120   let fncmdkey("&Display Cleared",3,0,0,"Displays all transactions cleared on this clearing date")
54140   let fncmdkey("Display &Uncleared",2,0,0,"Displays all remaining uncleared transactions")
54160   if clear_from_range=1 then 
54180     let fncmdkey("&Clear by &Range",6,1,0,"Enter another range of reference numbers")
54200   else if clear_from_range=0 then 
54220     let fncmdkey("C&lear",1,1,0,"Clear the highlited transaction")
54240   end if 
54280   let fncmdkey("&Complete",5,0,1,"Return to Bank Reconciliation menu")
54300   let fnacs(sn$,0,mat resp$,ck)
54320   let displaycleared=total= clear_from_range=0
54340   if ck=5 or ck=cancel then goto BANK_STMT_INFO
54360   let displayattop$=resp$(respc_display_top) ! do you want next uncleared check at the top of the screen
54380   if ck=2 then ! redisplay on uncleared
54400     goto CLEAR_TRANSACTIONS_FROM_LIST
54420   else if ck=3 then ! displays only cleared on this date
54440     let displaycleared=1
54460     goto CLEAR_TRANSACTIONS_FROM_LIST
54480   else if ck=6 then ! return to clearing by range of reference numbers
54500     goto SCR_CLEAR_BY_RANGE
54520   end if 
54540   read #clearing,using 'Form POS 1,C 11,G 6,pd 10.2,N 6',rec=val(resp$(respc_grid)): k$,tr2,tr3,clr
54560   if clr=0 then let newclr=stmtdt else let newclr=0 ! if no previous clearing date, use new one; if it has a date, unclear it
54580   rewrite #clearing,using 'Form POS 28,n 6',rec=val(resp$(respc_grid)): newclr
54600   rewrite #trmstr1,using 'Form POS 72,N 6',key=k$: newclr ! update the transaction history
54620   let lastrec=val(resp$(respc_grid))
54640   if lastrec+1 <= lrec(clearing) then let nextrec=lastrec+1 else let nextrec=1
54660   if lrec(clearing)=1 then let nextrec=0
54680   goto L6240
54700 ! /r CLEAR_TRANSACTIONS_FROM_LIST
56000   def fn_ctfl_add_grid_items(; cagi_type)
56020 ! cagi_type 1 = uncleared only
56040 ! cagi_type 2 = cleared only
56060 ! cagi_type 0 = (default) all
56080     restore #clearing: 
56100     do 
56120       read #clearing,using 'Form POS 1,C 11,G 6,pd 10.2,c 6': flxitm$(2),flxitm$(3),amount,flxitm$(5) eof EO_CAGI_CLEARING
56140       if cagi_type=1 and trim$(flxitm$(5))<>'0' then goto CAGI_NEXT
56160       if cagi_type=2 and trim$(flxitm$(5))='0' then goto CAGI_NEXT
56180       let flxitm$(1)=str$(rec(clearing))
56200       let flxitm$(4)=str$(amount)
56220       if val(flxitm$(5))=stmtdt then let total+=amount
56240       let fnflexadd1(mat flxitm$) ! pr 'CAGI TYPE '&str$(cagi_type)&'  '&flxitm$(1)&'-'&flxitm$(4)&'-'&flxitm$(5)
56260 CAGI_NEXT: ! 
56280     loop 
56300 EO_CAGI_CLEARING: ! 
56320   fnend 
