00010 ! Replace S:\acsGL\BankRec
00020 ! Bank reconciliation routines for general ledger
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fncno,fnerror,fndat,fndate_mmddyy_to_ccyymmdd,fntos,fnlbl,fncomboa,fncmdset,fnacs,fnchain,fntxt,fnbutton,fnfra,fnopt,fnflexinit1,fnflexadd1,fncmdkey,fnchk,fncombof,fnmsgbox,fnrgl$,fnqgl,fnagl$,fnconsole
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cnam$*40,dat$*20,adr(2),gl(3),sf1$*28,pr$(4)*30,whgl$(5)*12
00080   dim sendto$*80,ck$(22),cap$*128,aa(2),prtr(99),k$*21
00090   dim tr$(5)*35,tr(2),de$*30,bn$*40,sn$*50
00100   dim aa(2),whgl(5,3),line$*132,resp$(10)*35,ml$(4)*60,desc$(6)*14
00110   dim item1$(6)*40,item3$(9)*40,chdr$(5)*15,cmask$(5)*10,flxitm$(5)*21
00120   dim w(4),hdw$(4)*40,miscgl$(10)*12,misc$(10)*20
00130   dim io7$(9),dpt(3),io8$(25),dpd(5,5)
00140 ! ______________________________________________________________________
00150   let fntop(program$, cap$="Reconcile Bank")
00160   cancel=99 : let right=1 : center=2 : let left=0
00170   let fncno(cno,cnam$) !:
        let fndat(dat$)
00180   let fnconsole(on=1)
00190   def fncd(x)=(x-int(x*.01)*100)*10000+int(x*.01)
00200   cd1=date("mmddyy")
00210   open #bankrec1:=1: "Name="&env$('Q')&"\GLmstr\bankrec.H"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\Bankrec-idx.H"&str$(cno)&",Shr",internal,outin,keyed 
00220 ! Open #BANKREC2:=2: "Name="&env$('Q')&"\GLmstr\bankrec.H"&STR$(CNO)&",KFName="&env$('Q')&"\GLmstr\bankx2.H"&STR$(CNO)&",Shr",Internal,Outin,Keyed  ! ????? ken
00230   close #82: ioerr L240
00240 L240: open #82: "Name="&env$('Q')&"\GLmstr\Bank2"&wsid$&".H"&str$(cno)&",Use,RecL=35",internal,outin,relative 
00250   read #82,using "Form pos 1,c 12",rec=1: wbc$ norec L270
00260   goto MENU1
00270 L270: write #82,using "Form pos 1,c 12",rec=1: wbc$
00280 MENU1: ! 
00290   close #81: ioerr L300
00300 L300: open #81: "Name="&env$('Q')&"\GLmstr\Bank"&wbc$&".H"&str$(cno)&",Use,RecL=32",internal,outin,relative 
00310   if lrec(81)>0 then !:
          read #81,using "Form POS 1,N 6,2*PD 5.2,N 6",rec=1: stmtdt,bgbal,stmtbal,codt !:
        else !:
          write #81,using "Form POS 1,N 6,2*PD 5.2,N 6": stmtdt,bgbal,stmtbal,codt
00320 BANK_STMT_INFO: ! 
00330   let fntos(sn$="bankrec-1") !:
        let respc=0 !:
        let mylen=30 : let mypos=mylen+2
00340   let fnlbl(1,1,"Bank Account:",mylen,right)
00350   let fnqgl(1,mypos,0,2,pas) !:
        let resp$(respc+=1)=fnrgl$(wbc$)
00360   let fnlbl(3,1,"Statement Date:",mylen,right)
00370   let fntxt(3,mypos,10,0,1,"3",0,"We suggest you always use the month end date.") !:
        let resp$(respc+=1)=str$(stmtdt)
00380   let fnlbl(4,1,"Previous Statement Balance:",mylen,right)
00390   let fntxt(4,mypos,12,0,1,"10",0,"Pull this information directly from your current bank statement. ") !:
        let resp$(respc+=1)=str$(bgbal)
00400   let fnlbl(5,1,"Current Statement Balance:",mylen,right)
00410   let fntxt(5,mypos,12,0,1,"10",0," ") !:
        let resp$(respc+=1)=str$(stmtbal)
00420   let fnlbl(6,1,"Reconciliation Cutoff Date:",mylen,right)
00430   let fntxt(6,mypos,10,0,1,"3",0,"The cutoff date would normally be the last day of the month.") !:
        let resp$(respc+=1)=str$(codt)
00440   let fnlbl(8,1,"Reconciliation Options:",28,right)
00450   let item1$(1)="Enter Cleared Checks" !:
        let item1$(2)="Enter Cleared Deposits" !:
        let item1$(3)="Clear All Adjustments" !:
        let item1$(4)="Calculate Bank Totals" !:
        let item1$(5)="Print Reconciliation Listing" !:
        let item1$(6)="Make Corrections"
00460   let fncomboa("bankrec-1",8,30,mat item1$,"Select the funtion you would like to perform.  Normally you would clear the checks, deposits, adjustments and then calculate the bank totals")
00470   if selection=0 then let selection=1: !:
          let resp$(respc+=1)=item1$(selection)
00480   let fncmdkey("&Display Balances",3,0,0,"Displays previous balances for this bank account.") !:
        let fncmdkey("&Next",1,1,0,"Proceed to next options") !:
        let fncmdkey("&Cancel",5,0,1,"Returns to main menu")
00490   let fnacs(sn$,0,mat resp$,ck)
00500   if ck=5 then goto XIT
00510   let wbc$=fnagl$(resp$(1)) ! working bank account
00520   rewrite #82,using "Form pos 1,c 12",rec=1: resp$(1)
00530   if ck=3 then goto MENU1 ! redisplay balances
00540   bn$=resp$(1)(13:30)
00550   let stmtdt=val(resp$(2)(5:6))*10000+val(resp$(2)(7:8))*100+val(resp$(2)(3:4)) ! convert date back to mmddyy format
00560   bgbal=val(resp$(3))
00570   let stmtbal=val(resp$(4))
00580   codt=val(resp$(5)(5:6))*10000+val(resp$(5)(7:8))*100+val(resp$(5)(3:4)) ! convert date back to mmddyy format
00590   for j=1 to 6
00600     if resp$(6)=item1$(j) then let ti3=j: let selection=j: goto L620
00610   next j
00620 L620: if ti3=6 then let fnchain("S:\acsGL\Bankreconciliation")
00630   close #81: ioerr L640
00640 L640: open #81: "Name="&env$('Q')&"\GLmstr\Bank"&wbc$&".H"&str$(cno)&",Use,RecL=32",internal,outin,relative 
00650   if lrec(81)=0 then write #81,using "Form POS 1,N 6,2*PD 5.2,N 6",rec=1: stmtdt,bgbal,stmtbal,codt : goto L670
00660   rewrite #81,using "Form POS 1,N 6,2*PD 5.2,N 6",rec=1: stmtdt,bgbal,stmtbal,codt
00670 L670: close #81: ioerr L680
00680 L680: if ti3<3 then let wtt=ti3
00690   on ti3 goto CLEARING_OPTIONS,CLEARING_OPTIONS,CLEARING_ADJUSTMENTS,CALCULATE_TOTALS,PRINT_LISTINGS none MENU1
00700 ! ______________________________________________________________________
00710 CLEARING_ADJUSTMENTS: ! 
00720   let fntos(sn$="bankrec-11") !:
        let respc=0
00730   let fnlbl(1,60,"",1,0)
00740   let fnlbl(1,1,"If the adjustments have been previously cleared",50,0)
00750   let fnlbl(2,1,"with a wrong date, then enter the previously",50,0)
00760   let fnlbl(3,1,"cleared date (mmddyy) (otherwise leave blank):",50,0)
00770   let fntxt(3,48,8,0,1,"1",0,"") !:
        let resp$(respc+=1)=""
00780   let fncmdset(2): let fnacs(sn$,0,mat resp$,ck)
00790   if ck=5 or ck=cancel then goto MENU1
00800   let pcd1=val(resp$(1)) ! clear old adjustments
00810   restore #bankrec1,key>=wbc$&"3        ": nokey MENU1
00820 L820: read #bankrec1,using 'Form POS 79,c 12,pos 3,N 2,N 1,POS 12,N 6,POS 72,N 6': bankgl$,tcde,d1,clr eof MENU1
00830   if tcde><3 then goto MENU1
00840   if bankgl$><wbc$ then goto MENU1
00850   if fndate_mmddyy_to_ccyymmdd(d1)>fndate_mmddyy_to_ccyymmdd(stmtdt) then goto L820
00860   if clr><pcd1 then goto L820
00870   rewrite #bankrec1,using 'Form POS 79,c 12,pos 3,N 1,POS 12,N 6,POS 72,N 6': bankgl$,tcde,d1,stmtdt eof MENU1
00880   goto L820
00890 ! ______________________________________________________________________
00900 CLEARING_OPTIONS: ! 
00910   let fntos(sn$="bankrec-2") !:
        let respc=0
00920   let fnlbl(1,1,"Reference Number to Clear:",28,1)
00930   let fntxt(1,31,8,0,1,"",0,"If you wish to clear one transaction at a time, enter the reference # here.") !:
        let resp$(respc+=1)=""
00940   let fnlbl(2,1,"Cleared Amount:",28,1)
00950   let fntxt(2,31,12,0,1,"10",0,"You do not have to enter the amount.  If you do, it will compare the amount you enter to your check history and warn you of any differences. ") !:
        let resp$(respc+=1)=""
00960   if ti3=1 then let fnbutton(1,50,"Clear Range of Checks",60,"This option allows you to clear a range of checks without having to enter each check number",0,20)
00970   if ti3=2 then let fnbutton(1,50,"Clear Deposits by Amount",61,"This option allows you to clear deposits by entering the amount without having to know the reference number.",0,30)
00980   if ti3=2 then let fnbutton(3,50,"Clear Deposits by Date Range",62,"This option allows you to clear deposits by entering a date range.",0,30)
00990   if ti3=2 then let fnbutton(5,50,"Clear Deposits from List",63,"This option allows you to clear deposits from a listing of all outstanding deposits.",0,30)
01000   if ti3=1 then let fnbutton(3,50,"Clear Checks from List",64,"This option allows you to clear checks from a listing of all outstanding checks.",0,20)
01010   let fncmdset(2): let fnacs(sn$,0,mat resp$,ck)
01020   if ck=5 then goto MENU1
01030   let k$=resp$(1) ! check # to clear
01040   if ck=61 then !:
          let ti=2: let tcde=wtt=ti3: ck$=k$ !:
          let hamt=tr3=pcde=0 !:
          goto CLEAR_DEPOSITS_BY_AMOUNT
01050   if ti3=1 and ck=60 then goto L2810 ! clear range of checks
01060 ! If TI3=2 AND CK=3 Then Goto 4440 ! clear deposits by amount
01070   if ti3=2 and ck=62 then goto L2810 ! deposits by date range
01080   if ti3=2 and ck=63 then goto CLEAR_TRANSACTIONS_FROM_LIST ! deposits from listing of all outstanding deposits
01090   if ti3=1 and ck=64 then goto CLEAR_TRANSACTIONS_FROM_LIST ! clear checks from listing
01100   if ck=5 or ck=cancel then goto MENU1
01110   if rtrm$(k$)="" then goto CLEARING_OPTIONS
01120   ck$=lpad$(rtrm$(k$),8)
01130   let k$=wbc$&str$(ti3)&ck$
01140   read #bankrec1,using 'Form POS 79,c 12,pos 3,N 1,C 8,G 6,pd 10.2,C 8,C 35,N 1,N 6,N 1',key=k$,release: bankgl$,tcde,tr$(1),tr$(2),tx3,tr$(4),tr$(5),pcde,clr,scd nokey L1190 !:
        let tr$(3)=str$(tx3)
01150   ckamt=val(resp$(2)) : if val(tr$(3))=ckamt or ckamt=0 then goto L1210
01160   mat ml$(3) !:
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
01220   rewrite #bankrec1,using 'Form POS 72,N 6',key=k$: stmtdt
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
01310   cdte=val(resp$(1)) ! statement date cleared
01320   if cdte=0 then goto L1330
01330 L1330: rewrite #bankrec1,using 'Form POS 72,N 6',key=k$: cdte
01340   goto CLEARING_OPTIONS
01350 CALCULATE_TOTALS: ! 
01360   restore #bankrec1,key>=wbc$&"         ": nokey MENU1
01370   mat t1=(0)
01380 L1380: read #bankrec1,using 'Form POS 79,c 12,pos 3,N 1,C 8,G 6,pd 10.2,C 8,C 35,N 1,N 6,N 1',release: bankgl$,tcde,tr$(1),tr$(2),tx3,tr$(4),tr$(5),pcde,clr,scd eof L1600 !:
        let tr$(3)=str$(tx3)
01390   if bankgl$><wbc$ then goto L1600
01400   amt=val(tr$(3)) conv L1380
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
01520   amt=-amt : goto L1490
01530 L1530: on tcde goto L1540,L1550,L1560,L1540 none L1580
01540 L1540: let t1(1)=t1(1)+1: let t1(2)=t1(2)+amt : let w0=4: goto L1580 ! CLEARED CHECKS
01550 L1550: let t1(3)=t1(3)+1: let t1(4)=t1(4)+amt : let w0=3: goto L1580 ! CLEARED DEPOSTIS
01560 L1560: if amt>0 then goto L1550
01570   amt=-amt: goto L1540
01580 L1580: if ti3=5 then gosub L2570
01590   goto L1380
01600 L1600: if ti3><5 then goto BANKTOTALSCREEN
01610   let fnopenprn
01620   for j=1 to 4
01630     let wp=j+50
01640     if w(j)=0 then goto WPNEXTJ
01650     pr #wp,using L1660: t2(j)
01660 L1660: form pos 19,"  ----------",skip 1,pos 19,n 12.2,skip 1
01670 ! If NW=1 Then pr #WP: NEWPAGE
01680     close #wp: 
01690     open #wp: "Name=RPT"&str$(j)&"."&wsid$,display,input 
01700 L1700: linput #wp: line$ eof EO_WP
01710     pr #255: line$
01720     goto L1700
01730 EO_WP: ! 
01740     pr #255: newpage
01750     close #wp: 
01760 ! If SENDTO$(1:3)<>"PRN" Then !:
          ! Execute "Sy Copy "&SENDTO$&"+RPT"&STR$(J)&"."&WSID$&" "&SENDTO$
01770 WPNEXTJ: next j
01780   let fncloseprn
01790   goto MENU1
01800 BANKTOTALSCREEN: ! 
01810   pr newpage
01820   let fntos(sn$="bankrec-7") !:
        let respc=0
01830   let fnlbl(1,1, rtrm$(bn$),45,2)
01840   let fnlbl(2,1,"   Statement Date:      "&ltrm$(cnvrt$("PIC(##/##/##)",stmtdt)),45,1)
01850   let fnlbl(3,1,"Previous Statement Balance:"&cnvrt$("pic(----,---,---.##)",bgbal),45,1)
01860   let fnlbl(4,1,cnvrt$("N 5",t1(3))&" Deposit(s) Cleared:"&cnvrt$("pic(----,---,---.##)",t1(4)),45,1)
01870   let fnlbl(5,1, cnvrt$("N 5",t1(1))&" Withdrawal(s) Cleared:"&cnvrt$("pic(----,---,---.##)",t1(2)),45,1)
01880   let t1(5)=bgbal+t1(4)-t1(2)
01890   let fnlbl(6,1, "Calculated Statement Balance:"&cnvrt$("pic(----,---,---.##)",t1(5)),45,1)
01900   let fnlbl(7,1, "Bank Statement Balance:"& cnvrt$("pic(----,---,---.##)",stmtbal),45,1)
01910   let fnlbl(8,1, "Out of Balance Amount:"&cnvrt$("pic(----,---,---.##)",stmtbal-t1(5)),45,1)
01920   let fnlbl(9,1, "Uncleared Deposit(s):"&cnvrt$("pic(----,---,---.##)",t1(7)),45,1)
01930   let fnlbl(10,1, cnvrt$("N 5",t1(8))&" Uncleared Withdrawal(s):"&cnvrt$("pic(----,---,---.##)",t1(9)) ,45,1)
01940   let fnlbl(10,1,"Calculated Book Balance:"&cnvrt$("pic(----,---,---.##)",t1(10)) ,45,1)
01950   let fncloseprn
01960   let t1(10)=bgbal+t1(7)+t1(4)-t1(9)-t1(2)
01970   cutoffbal=bal
01980   restore #bankrec1,key>=wbc$&"         ": nokey EO_ADDING_BALANCE
01990 L1990: read #bankrec1,using 'Form POS 79,c 12,pos 3,N 1,C 8,G 6,pd 10.2,C 8,C 35,N 1,N 6,N 1': bankgl$,tcde,tr$(1),tr$(2),tx3,tr$(4),tr$(5),pcde,clr,scd eof EO_ADDING_BALANCE !:
        let tr$(3)=str$(tx3)
02000   if bankgl$><wbc$ then goto L1990
02010   let x=val(tr$(2)) conv L1990
02020   if fndate_mmddyy_to_ccyymmdd(x)=<fndate_mmddyy_to_ccyymmdd(codt) then goto L1990
02030   if tcde=1 then cutoffbal=cutoffbal+val(tr$(3)) ! add checks back
02040   if tcde=2 then cutoffbal=cutoffbal-val(tr$(3)) ! subtract deposits
02050   if tcde=3 then cutoffbal=cutoffbal-val(tr$(3)) ! adjustments
02060   goto L1990
02070 EO_ADDING_BALANCE: ! 
02080   let fnlbl(11,1,"Book Balance as of"&cnvrt$("PIC(ZZ/ZZ/ZZ)",codt)&":"&cnvrt$("pic(----,---,---.##)",cutoffbal),45,1)
02090   let fncmdset(3): let fnacs(sn$,0,mat resp$,ck)
02100   if ck=5 then goto MENU1
02110   if ck=1 then gosub L2140 : goto BANKTOTALSCREEN
02120   goto BANKTOTALSCREEN
02130 ! ______________________________________________________________________
02140 L2140: let fnopenprn
02150   pr #255,using L2160: bn$(1:30)
02160 L2160: form pos 20,cc 40
02170   pr #255,using L2160: "Bank Reconciliation"
02180   pr #255,using L2160: "Statement Date: "&cnvrt$("PIC(ZZ/ZZ/ZZ)",stmtdt)
02190   pr #255: ""
02200   pr #255: "Previous Statement Balance: "&cnvrt$("N 11.2",bgbal)
02210   pr #255: cnvrt$("N 5",t1(3))&" Deposits Total:   "&cnvrt$("N 15.2",t1(4))
02220   pr #255: cnvrt$("N 5",t1(1))&" Withdrawals Total: "&cnvrt$("N 14.2",t1(2))
02230   pr #255: "Bank Statement Balance:"&cnvrt$("N 16.2",stmtbal)
02240   pr #255: "Calculated Statement Balance:"&cnvrt$("N 10.2",t1(5))
02250   pr #255: "Out of Balance Amount:"&cnvrt$("N 17.2",stmtbal-t1(5))
02260   pr #255: ""
02270   pr #255: cnvrt$("N 5",t1(6))&" Uncleared Deposits:"&cnvrt$("N 14.2",t1(7))
02280   pr #255: cnvrt$("N 5",t1(8))&" Uncleared Withdrawals:"&cnvrt$("N 11.2",t1(9))
02290   pr #255: "Actual Bank Balance:"&cnvrt$("N 19.2",t1(10))
02300   return 
02310 ! ______________________________________________________________________
02320 PRINT_LISTINGS: ! 
02330   let hdw$(1)="Deposits not cleared as of "&cnvrt$("PIC(ZZ/ZZ/ZZ)",stmtdt)
02340   let hdw$(2)="Withdrawals not cleared as of "&cnvrt$("PIC(ZZ/ZZ/ZZ)",stmtdt)
02350   let hdw$(3)="Deposits cleared on "&cnvrt$("PIC(ZZ/ZZ/ZZ)",stmtdt)
02360   let hdw$(4)="Withdrawals cleared on "&cnvrt$("PIC(ZZ/ZZ/ZZ)",stmtdt)
02370   let fntos(sn$="bankrec-12") !:
        let respc=0 : mat resp$=('')
02380 ! Let FNFRA(1,1,4,60,"Select Listings to Print","Choose any of the four listing",0)
02390   let fnchk(1,3,hdw$(1),0) !:
        let resp$(respc+=1)=""
02400   let fnchk(2,3,hdw$(2),0) !:
        let resp$(respc+=1)=""
02410   let fnchk(3,3,hdw$(3),0) !:
        let resp$(respc+=1)=""
02420   let fnchk(4,3,hdw$(4),0) !:
        let resp$(respc+=1)=""
02430   let fncmdset(2): let fnacs(sn$,0,mat resp$,ck)
02440   if ck=5 or ck=cancel then goto MENU1
02450   if resp$(1)(1:1)="T" then let w(1)=1 else let w(1)=0
02460   if resp$(2)(1:1)="T" then let w(2)=1 else let w(2)=0
02470   if resp$(3)(1:1)="T" then let w(3)=1 else let w(3)=0
02480   if resp$(4)(1:1)="T" then let w(4)=1 else let w(4)=0
02490   let fnopenprn
02500   for j=1 to 4
02510     if w(j)=0 then goto L2540
02520     let wp=j+50
02530     open #wp: "Name=Rpt"&str$(j)&"."&wsid$&",Size=0,RecL=132,Replace",display,output 
02540 L2540: next j
02550   mat t2=(0)
02560   goto CALCULATE_TOTALS
02570 L2570: if w(w0)=0 then goto L2630
02580   let wp=w0+50
02590   if w(w0)=1 then gosub L2690
02600   pr #wp,using L2610: tr$(1),val(tr$(2)),amt,tr$(5),clr pageoflow L2650
02610 L2610: form pos 1,c 10,pic(zz/zz/zz),n 12.2,x 2,c 37,pic(zz/zz/zz),skip 1
02620   let t2(w0)=t2(w0)+amt
02630 L2630: return 
02640 ! ______________________________________________________________________
02650 L2650: pr #wp: newpage
02660   gosub L2690
02670   continue 
02680 ! ______________________________________________________________________
02690 L2690: ! 
02700 ! 
02710   let hd2=46-int(len(rtrm$(hdw$(w0)))/2)
02720   pr #wp,using L2730: date$('mm/dd/yy'),rtrm$(cnam$)&" - Bank Account # "&str$(wbc),time$," Reconciliation Listing",hdw$(w0),"Page",w(w0),dat$
02730 L2730: form skip 3,pos 1,c 8,cc 56,skip 1,pos 1,c 8,cc 56,skip 1,pos 9,cc 56,skip 1,pos 1,c 4,n 4,cc 56,skip 2
02740   pr #wp: "Check or                                                              Date"
02750   pr #wp: "Ref Numb    Date       Amount   Name or Description                  Cleared"
02760   pr #wp: "________  ________  __________  ___________________________________  ________"
02770   let w(w0)=w(w0)+1
02780   return 
02790 ! ______________________________________________________________________
02800 ! ______________________________________________________________________
02810 L2810: c1=r1=2
02820 CLEAR_BY_RANGE: ! clear by range of check numbers or by date
02830   let fntos(sn$="bankrec-9") !:
        let respc=0
02840   clear_from_range=1 ! return here from grid
02850   let fnlbl(1,43,"",1,0)
02860   if ti3=2 then goto L2920
02870   let fnlbl(1,1,"Lowest Check Number to Clear:",30,1)
02880   let fntxt(1,32,8,0,1,"",0,"If your bank statement lists checks in the order cleared and indicate breaks in the check numbers, it is very easy to clear checks using this feature.") !:
        let resp$(respc+=1)=""
02890   let fnlbl(2,1,"Highest Check Number to Clear:",30,1)
02900   let fntxt(2,32,8,0,1,"",0,"Enter the last check number in the series.") !:
        let resp$(respc+=1)=""
02910   goto L2960
02920 L2920: let fnlbl(1,1,"Lowest Date to Clear:",30,1)
02930   let fntxt(1,32,10,0,1,"1",0,"Normally you would use beginning of the month, but you could back the date up a few days to get any uncleared from the previous month.") !:
        let resp$(respc+=1)=""
02940   let fnlbl(2,1,"Highest Date to Clear:",30,1)
02950   let fntxt(2,32,10,0,1,"1",0,"Normally you would use the last day of the month, but if the bank's cutoff date is different, you may want to use it.") !:
        let resp$(respc+=1)=""
02960 L2960: let fncmdset(2): let fnacs(sn$,0,mat resp$,ck)
02970   if ck=5 or ck=cancel then goto MENU1
02980   let l1=val(resp$(1)) ! either lowest date or lowest check #
02990   let h1=val(resp$(2)) ! either highest date or highest check #
03000   if ti3><2 then goto L3030
03010   let l1=fncd(l1)
03020   let h1=fncd(h1)
03030 L3030: if ti3=1 then let k$=wbc$&str$(ti3)&lpad$(str$(l1),8)
03040   if ti3=2 then let k$=wbc$&str$(ti3)&lpad$(str$(0),8)
03050   restore #bankrec1,key>=k$: nokey L3070
03060   goto L3080
03070 L3070: mat ml$(1) !:
        let ml$(1)="Nothing found in this range." !:
        let fnmsgbox(mat ml$,resp$,cap$,48) !:
        goto CLEAR_BY_RANGE
03080 L3080: ! pr Fields "1,2,C 8,R,N": " Check #"   ! do I want some kind of grid here???? kj
03090 ! pr Fields "1,11,C 10,R,N": "  Amount"
03100 L3100: read #bankrec1,using 'Form POS 79,c 12,pos 3,N 1,C 8,G 6,pd 10.2,C 8,C 35,N 1,N 6,N 1': bankgl$,tcde,tr$(1),tr$(2),tx3,tr$(4),tr$(5),pcde,clr,scd eof DISPLAYCLEAREDSOFAR !:
        let tr$(3)=str$(tx3)
03110   if bankgl$><wbc$ or tcde><ti3 then goto CLEAR_BY_RANGE
03120   if ti3=1 then goto L3170
03130   if clr>0 then goto L3100
03140   let tr2=fncd(val(tr$(2))) conv L3100
03150   if tr2<l1 or tr2>h1 then goto L3100
03160   goto L3190
03170 L3170: let h2=val(tr$(1)) conv L3100
03180   if h2>h1 then let displaycleared=1 : goto CLEAR_TRANSACTIONS_FROM_LIST ! Goto 3710
03190 L3190: rewrite #bankrec1,using 'Form POS 72,N 6': stmtdt
03200 ! pr Fields STR$(R1)&","&STR$(C1)&",C 8,N": TR$(1)  ! also some kind of grid here
03210 ! pr Fields STR$(R1)&","&STR$(C1+9)&",C 10,N": TR$(3)
03220 ! Let R1=R1+1
03230 ! If R1=8 Then Let R1=14
03240 ! If R1>23 Then Let R1=2: c1=C1+20 Else Goto 4230
03250 ! If C1>62 Then c1=2
03260 ! pr Fields "1,"&STR$(C1)&",C 8,R,N": " Check #"
03270 ! pr Fields "1,"&STR$(C1+9)&",C 10,R,N": "  Amount"
03280   goto L3100
03290 DISPLAYCLEAREDSOFAR: ! show what has been cleared
03300   let displaycleared=1 : goto CLEAR_TRANSACTIONS_FROM_LIST
03310 FREE_DPAMNTS: ! 
03320   execute "Free "&env$('Q')&"\GLmstr\DpAmnts.H"&str$(cno)&" -n"
03330   continue 
03340 ! ______________________________________________________________________
03350 CLEAR_DEPOSITS_BY_AMOUNT: ! clear deposits by amounts
03360   if cda=1 then goto DPAMENU
03370 L3370: open #dpamnts=91: "Name="&env$('Q')&"\GLmstr\DpAmnts.H"&str$(cno)&",Use,RecL=20",internal,outin,relative ioerr FREE_DPAMNTS
03380   open #92: "Name="&env$('Q')&"\GLmstr\DpDates.H"&str$(cno)&",Use,RecL=150",internal,outin,relative 
03390   read #92,using L3400,rec=1: mat dpd norec L3420
03400 L3400: form pos 1,25*n 6
03410   goto L3430
03420 L3420: write #92,using L3400,rec=1: mat dpd
03430 L3430: open #93: "Name="&env$('Q')&"\GLmstr\DPTypes.H"&str$(cno)&",Use,RecL=3",internal,outin,relative 
03440   read #93,using L3450,rec=1: mat dpt norec L3470
03450 L3450: form pos 1,3*n 1
03460   goto L3480
03470 L3470: write #93,using L3450,rec=1: mat dpt
03480 L3480: cda=1
03490 ! __________________________________________
03500 DPAMENU: ! 
03510   let fntos(sn$="bankrec-5") !:
        let respc=0
03520   let fnfra(1,1,9,60,"Reconciliation Options","Used to clear deposita by amounts and not by reference number",0) !:
        let frame=1
03530   let fnopt(1,3,"Erase Previous Amounts & Dates",0,frame) !:
        if sel_code=1 or sel_code=0 then let resp$(respc+=1)="True" else !:
          let resp$(respc+=1)="False"
03540   let fnopt(2,3,"Enter Deposit Amounts",0,frame) !:
        if sel_code=2 then let resp$(respc+=1)="True" else !:
          let resp$(respc+=1)="False"
03550   let fnopt(3,3,"Enter Types to Add Together",0,frame) !:
        if sel_code=3 then let resp$(respc+=1)="True" else !:
          let resp$(respc+=1)="False"
03560   let fnopt(4,3,"Enter Dates to Add Together",0,frame) !:
        if sel_code=4 then let resp$(respc+=1)="True" else !:
          let resp$(respc+=1)="False"
03570   let fnopt(5,3,"Clear Matches",0,frame) !:
        if sel_code=5 then let resp$(respc+=1)="True" else !:
          let resp$(respc+=1)="False"
03580   let fnopt(6,3,"Print listing of Un-Matched",0,frame) !:
        if sel_code=6 then let resp$(respc+=1)="True" else !:
          let resp$(respc+=1)="False"
03590   let fnopt(7,3,"Print Listing of All Entries",0,frame) !:
        if sel_code=7 then let resp$(respc+=1)="True" else !:
          let resp$(respc+=1)="False"
03600   let fnopt(8,3,"Make Corrections",0,frame) !:
        if sel_code=8 then let resp$(respc+=1)="True" else !:
          let resp$(respc+=1)="False"
03610 ! Let FNCOMBOA("bankrec-3",3,30,MAT ITEM3$,"Select the funtion you would like to perform.  Normally you would Erase Previous, Enter Amounts, Clear Matches and check your totals")!!:
        let resp$(respc+=1)=item3$(1)
03620   let resp$(2)="True"
03630   if t1 =0 and t2=0 then goto L3660
03640   let fnlbl(12,1,"Deposits Entered: "&cnvrt$("pic(ZZ,ZZZ,ZZZ.##)",t1),40,0)
03650   let fnlbl(13,1,"Deposits Cleared: "&cnvrt$("pic(ZZ,ZZZ,ZZZ.##)",t2),40,0)
03660 L3660: let fncmdset(2): let fnacs(sn$,0,mat resp$,ck)
03670   if ck=5 or ck=cancel then goto MENU1
03680   for j=1 to 8
03690     if resp$(j)(1:1)="T" then let sel_code=ti1=j: goto L3710
03700   next j
03710 L3710: on ti1 goto L3720,L3760,DPTYPES,DPDATES,L5060,PRINT_EDITS,PRINT_EDITS,COR1 none DPAMENU
03720 L3720: close #dpamnts,free: ioerr L3730
03730 L3730: close #92,free: ioerr L3740
03740 L3740: close #93: ioerr L3750
03750 L3750: goto L3370
03760 L3760: if lrec(91)=0 then goto ENTER_DEPOSITS_CLEARED
03770   read #dpamnts,using L3880,rec=lrec(91): tr$,am1,ti$
03780 ENTER_DEPOSITS_CLEARED: ! 
03790   let fntos(sn$="bankrec-4") !:
        let respc=0
03800   let fnlbl(1,1,"Amount to Clear:",25,1)
03810   let fntxt(1,28,12,0,1,"10",0,"Enter each deposit amount that shows as cleared on the bank statement.") !:
        let resp$(respc+=1)=""
03820   if am1>0 then let fnlbl(2,1,"Last Amount Entered:"&cnvrt$("N 13.2",am1),38,1)
03830   let fncmdset(2): let fnacs(sn$,0,mat resp$,ck)
03840   if ck=5 then goto PRINT_EDITS
03850   am1=val(resp$(1)) ! deposit amount entered
03860   if am1=0 then goto PRINT_EDITS
03870   write #dpamnts,using L3880: "",am1,""
03880 L3880: form pos 1,c 8,pd 10.2,c 2
03890   goto ENTER_DEPOSITS_CLEARED
03900 PRINT_EDITS: let t1=t2=0
03910   let fnopenprn
03920   let fncloseprn
03930   if ti1=6 then pr #255,using "form pos 10,cc 60": "Uncleared Entries"
03940   if ti1=7 then pr #255,using "form pos 10,cc 60": "Listing of All Entries Entered For Clearing"
03950   for j=1 to lrec(91)
03960     read #dpamnts,using L3880,rec=j: tr$,am1,ti$
03970     if am1=0 then goto L4040
03980     if ti1<6 then goto L4020
03990     if rtrm$(tr$)><"" and ti1=6 then goto L4040
04000     pr #255,using L4010: j,tr$,am1,ti$
04010 L4010: form pos 1,n 4,x 2,c 10,n 10.2,x 2,c 2,skip 1
04020 L4020: let t1=t1+am1
04030     if rtrm$(tr$)><"" then let t2=t2+am1
04040 L4040: next j
04050   if ti1<6 then goto DPAMENU
04060   pr #255,using L4070: "__________",t1,"=========="
04070 L4070: form pos 17,c 10,skip 1,pos 17,n 10.2,skip 1,pos 17,c 10,skip 1
04080   let fncloseprn
04090   goto DPAMENU
04100 DPTYPES: ! ENTER TYPES TO TOTAL   ! fix  KJ
04110   for j=1 to 3
04120     let io7$(j)=str$(j*2+11)&",40,N 1,UT,N"
04130   next j
04140   pr newpage
04150   close #101: ioerr L4160
04160 L4160: open #101: "SROW=04,SCOL=10,EROW=19,ECOL=73,Border=Sr,CAPTION=<Deposit Types to Total",display,outin 
04170   pr fields "5,11,C 62": "This is designed to take care of two or more types of Credit"
04180   pr fields "6,11,C 62": "Cards that are totaled together by the card company before"
04190   pr fields "7,11,C 62": "they are transmitted to your bank for deposit."
04200   pr fields "09,11,c 62": "credit card deposits begin with a prefix of c1,c2, or c3."
04210   pr fields "10,11,c 62": "place a 1 by the types to be added together for 1 bank deposit"
04220   for j=1 to 3
04230     pr fields str$(j*2+11)&",37,C 3": "C"&str$(j)
04240   next j
04250   pr fields "20,30,C 09,B,1": "Next (F1)"
04260 L4260: rinput fields mat io7$: mat dpt conv CONV7
04270   if ce>0 then let io7$(ce)(ce1:ce2)="U": ce=0
04280   if ck>0 then goto L4350 else ce=curfld
04290 L4290: ce=ce+1: if ce>udim(io7$) then ce=1
04300 L4300: let io7$(ce)=rtrm$(uprc$(io7$(ce))) : ce1=pos(io7$(ce),"U",10) : if ce1=0 then goto L4290
04310   ce2=ce1+1 : let io7$(ce)(ce1:ce1)="UC" : goto L4260
04320 CONV7: if ce>0 then let io7$(ce)(ce1:ce2)="U"
04330   ce=cnt+1
04340 ERR7: pr fields "24,78,C 1": bell : goto L4300
04350 L4350: rewrite #93,using L3450,rec=1: mat dpt
04360   goto DPAMENU
04370 ! ______________________________________________________________________
04380 DPDATES: ! ENTER DATES TO TOTAL
04390   pr newpage
04400   close #108: ioerr L4410
04410 L4410: open #108: "SROW=03,SCOL=10,EROW=23,ECOL=73,Border=Sr,CAPTION=<Credit Card Dates to be Totaled",display,outin 
04420   pr #108: newpage
04430   for j=1 to 5
04440     let io8$(j)="14,"&str$(j*9+15)&",N 6,UT,N"
04450     let io8$(j+5)="16,"&str$(j*9+15)&",N 6,UT,N"
04460     let io8$(j+10)="18,"&str$(j*9+15)&",N 6,UT,N"
04470     let io8$(j+15)="20,"&str$(j*9+15)&",N 6,UT,N"
04480     let io8$(j+20)="22,"&str$(j*9+15)&",N 6,UT,N"
04490   next j
04500   pr fields "4,11,c 62": "This is designed to take care of two or more dates on Credit"
04510   pr fields "5,11,c 62": "Card batched totaled together by the credit card company"
04520   pr fields "6,11,c 62": "before they are transmitted to your bank for deposit."
04530   pr fields "08,11,c 62": "this usually occurs on weekends or when a batch is transmitted"
04540   pr fields "09,11,c 62": "after 8pm (Eastern time) one day and before 8pm the next."
04550   pr fields "11,11,C 62": "You may have 5 different entries with up to 5 Dates per entry"
04560   for j=1 to 5
04570     pr fields str$(j*2+12)&",16,C 50": "Entry "&str$(j)&"        +        +        +        +"
04580   next j
04590   pr fields "13,24,C 42,N": "         (use only mmddyy format)"
04600   pr fields "24,35,C 09,B,1": "Next (F1)"
04610 L4610: rinput fields mat io8$: mat dpd conv ERR8
04620   if ce>0 then let io8$(ce)(ce1:ce2)="U": ce=0
04630   if ck>0 then goto L4700 else ce=curfld
04640 L4640: ce=ce+1: if ce>udim(io8$) then ce=1
04650 L4650: let io8$(ce)=rtrm$(uprc$(io8$(ce))) : ce1=pos(io8$(ce),"U",10) : if ce1=0 then goto L4640
04660   ce2=ce1+1 : let io8$(ce)(ce1:ce1)="UC" : goto L4610
04670 CONV8: if ce>0 then let io8$(ce)(ce1:ce2)="U"
04680   ce=cnt+1
04690 ERR8: pr fields "24,78,C 1": bell : goto L4650
04700 L4700: let dpd$=cnvrt$("PIC(######)",stmtdt)
04710   let mo2=val(dpd$(1:2))
04720   let da2=val(dpd$(3:4))
04730   let yr2=val(dpd$(5:6))
04740   for ce=1 to 25
04750     let j1=int((ce-1)/5)+1
04760     let j2=ce-(j1-1)*5
04770     if dpd(j1,j2)=0 then goto L4880
04780     let dpd$=cnvrt$("PIC(######)",dpd(j1,j2))
04790     let mo1=val(dpd$(1:2))
04800     let da1=val(dpd$(3:4))
04810     let yr1=val(dpd$(5:6))
04820     if mo1<01 or mo1>12 then goto ERR8
04830     if mo1=mo2 and yr1=yr2 then goto L4870 ! SAME MONTH SAME YEAR
04840     if yr1=yr2 and mo1+1=mo2 then goto L4870 ! PREVIOUS MONTH SAME YEAR
04850     if yr1+1><yr2 then goto ERR8
04860     if mo2=1 and mo1=12 then goto L4870 else goto ERR8
04870 L4870: if da1<01 or da1>31 then goto ERR8
04880 L4880: next ce
04890   ce=0
04900   rewrite #92,using L3400,rec=1: mat dpd
04910   goto DPAMENU
04920 ! ______________________________________________________________________
04930 COR1: pr newpage
04940   pr fields "10,10,c 50": "Item Number to correct:"
04950   pr fields "12,35,C 10,B,99": "Stop (Esc)"
04960 L4960: input fields "10,50,Nz 4,UT,N": cr1 conv L4960
04970   if cr1=0 or ck=5 then goto DPAMENU
04980   if cr1<1 or cr1>lrec(91) then goto L4960
04990   read #dpamnts,using L3880,rec=cr1: tr$,am1,ti$
05000   pr newpage
05010   pr fields "10,10,C 60": "Correct Amount (0 to Delete):"
05020   rinput fields "10,50,N 10.2,UT,N": am1
05030   rewrite #dpamnts,using L3880,rec=cr1: tr$,am1,ti$
05040   goto COR1
05050 ! ______________________________________________________________________
05060 L5060: restore #bankrec1,key>=wbc$&"2        ": nokey DPAMENU
05070   let t2=0
05080 L5080: read #bankrec1,using 'Form POS 79,c 12,POS 3,N 1,pos 4,c 8,G 6,pd 10.2,POS 72,N 6': bankgl$,tcde,tr$(1),tr2,tr3,clr eof L5260
05090   if bankgl$><wbc$ then goto L5260
05100   if tcde><2 then goto L5260
05110   if clr><0 then goto L5080
05120   if fndate_mmddyy_to_ccyymmdd(tr2)>fndate_mmddyy_to_ccyymmdd(stmtdt) then goto L5080
05130   for r1=1 to lrec(91)
05140     read #dpamnts,using L3880,rec=r1: tr$,am1,ti$
05150     if am1=0 then goto L5240
05160     if rtrm$(tr$)><"" then goto L5240
05170     if tr3><am1 then goto L5240
05180     rewrite #dpamnts,using L3880,rec=r1: tr$(1)
05190     let k$=lpad$(rtrm$(bankgl$),2)&str$(tcde)&tr$(1)
05200     rewrite #bankrec1,using 'Form POS 72,N 6',key=k$: stmtdt
05210     form pos 72,n 6
05220     let t2+=tr3
05230     goto L5080
05240 L5240: next r1
05250   goto L5080
05260 L5260: goto DPAMENU
05270 ! ______________________________________________________________________
05280 ! <Updateable Region: ERTN>
05290 ERTN: let fnerror(program$,err,line,act$,"xit")
05300   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
05310   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
05320   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
05330 ERTN_EXEC_ACT: execute act$ : goto ERTN
05340 ! /region
05350 ! ______________________________________________________________________
05360 CLEAR_TRANSACTIONS_FROM_LIST: ! 
05370   let lastrec=nextrec=total=0 !:
        let displayattop$="True"
05380   if ti3=1 then let type$="Checks" else let type$="Deposits"
05390   close #clearing: ioerr L5400
05400 L5400: open #clearing=89: "Name="&env$('Q')&"\GLmstr\clearing.H"&wsid$&",replace,RecL=43",internal,outin,relative 
05410   if ti3=2 then restore #bankrec1,key>=wbc$&"2        ": nokey DISPLAY_GRID else !:
          restore #bankrec1,key>=wbc$&"1        ": nokey DISPLAY_GRID
05420 L5420: read #bankrec1,using 'Form POS 79,c 12,POS 3,N 1,pos 4,c 8,G 6,pd 10.2,POS 72,N 6': bankgl$,tcde,tr$(1),tr2,tr3,clr eof DISPLAY_GRID
05430   if bankgl$><wbc$ then goto L5420
05440   if ti3=1 and tcde><1 then goto L5420 ! only disbursments
05450   if displaycleared=1 and clr=stmtdt then goto L5480 ! display only cleared on this date
05460   if displaycleared=1 and clr<>stmtdt then goto L5420 ! if only cleared this clearing date, don't allow others to list
05470   if clr><0 then goto L5420
05480 L5480: let k$=lpad$(rtrm$(bankgl$),12)&str$(tcde)&lpad$(rtrm$(tr$(1)),8)
05490   write #clearing,using 'Form POS 1,C 21,G 6,pd 10.2,N 6': k$,tr2,tr3,clr
05500   goto L5420
05510 DISPLAY_GRID: ! 
05520   mat chdr$(5) : mat cmask$(5) : mat flxitm$(5) !:
        chdr$(1)="Rec" !:
        chdr$(2)="Reference #" : chdr$(3)="Date" !:
        chdr$(4)="Amount" : chdr$(5)="Cleared" !:
        cmask$(1)='30' : cmask$(2)='' : cmask$(3)='1' !:
        cmask$(4)='10' : cmask$(5)='1'
05530 L5530: let fntos(sn$="bankrec-11") !:
        let respc=0 : mat resp$=('')
05540   let fnlbl(1,1,trim$(bn$(1:30))&"-"&type$,65,2)
05550   let fnflexinit1('Deposit-1',2,3,15,55,mat chdr$,mat cmask$,1)
05560   restore #clearing: 
05570   let x=lrec(89): if nextrec>0 and x>1 and displayattop$="True" then goto L5580 else goto L5670
05580 L5580: for j=nextrec to lrec(clearing) ! read starting with next record
05590     read #clearing,using 'Form POS 1,C 21,G 6,pd 10.2,c 6',rec=j: flxitm$(2),flxitm$(3),amount,flxitm$(5) norec L5610 !:
          let flxitm$(1)=str$(rec(clearing)) !:
          let flxitm$(4)=str$(amount)
05600     let fnflexadd1(mat flxitm$)
05610 L5610: next j
05620   for j=1 to max(nextrec-1,1) ! read records previously cleared or skipped
05630     read #clearing,using 'Form POS 1,C 21,G 6,pd 10.2,c 6',rec=j: flxitm$(2),flxitm$(3),amount,flxitm$(5) norec L5650 !:
          let flxitm$(1)=str$(rec(clearing)) !:
          let flxitm$(4)=str$(amount) !:
          if val(flxitm$(5))=stmtdt then let total+=amount
05640     let fnflexadd1(mat flxitm$)
05650 L5650: next j
05660   goto L5690
05670 L5670: read #clearing,using 'Form POS 1,C 21,G 6,pd 10.2,c 6': flxitm$(2),flxitm$(3),amount,flxitm$(5) eof L5690 !:
        let flxitm$(1)=str$(rec(clearing)) !:
        let flxitm$(4)=str$(amount) !:
        if val(flxitm$(5))=stmtdt then let total+=amount
05680   let fnflexadd1(mat flxitm$) : goto L5670
05690 L5690: let fnlbl(17,30,"Total Cleared:",16,1)
05700   let fntxt(17,49,12,0,1,"10",0," ") !:
        let resp$(respc+=1)=str$(total) !:
        if val(flxitm$(5))=stmtdt then let total+=amount
05710   let fnchk(18,49,"Display at Top:",1) !:
        let resp$(respc+=1)=displayattop$
05720   let fncmdkey("&Display Cleared",3,0,0,"Displays all transactions cleared on this clearing date") !:
        let fncmdkey("&Display Uncleared",2,0,0,"Displays all remaining uncleared transactions")
05730   if clear_from_range=1 then let fncmdkey("&Clear by Range",6,1,0,"Enter another range of reference numbers")
05740   if clear_from_range=0 then let fncmdkey("&Clear",1,1,0,"Clear the highlited transaction")
05750   let fncmdkey("C&ancel",5,0,1,"Return to Bank Reconciliation menu")
05760   let fnacs(sn$,0,mat resp$,ck)
05770   let displaycleared=total= clear_from_range=0
05780   if ck=5 or ck=cancel then goto BANK_STMT_INFO
05790   let displayattop$=resp$(3) !:
        ! do you want next uncleared check at the top of the screen
05800   if ck=2 then goto CLEAR_TRANSACTIONS_FROM_LIST ! redisplay on uncleared
05810   if ck=3 then let displaycleared=1: goto CLEAR_TRANSACTIONS_FROM_LIST !:
          ! displays only cleared on this date
05820   if ck=6 then goto CLEAR_BY_RANGE !:
          ! return to clearing by range of reference numbers
05830   read #clearing,using 'Form POS 1,C 21,G 6,pd 10.2,N 6',rec=val(resp$(1)): k$,tr2,tr3,clr norec L5900
05840 ! Let K$(1:12)=LPAD$(RTRM$(K$(1:12)),12)
05850   if clr=0 then let newclr=stmtdt else let newclr=0 ! if no previous clearing date, use new one; if it has a date, unclear it
05860   rewrite #clearing,using 'Form POS 38,n 6',rec=val(resp$(1)): newclr
05870   read #clearing,using 'Form POS 1,C 21,G 6,pd 10.2,N 6',rec=val(resp$(1)): k$,tr2,tr3,clr norec L5900
05880   rewrite #bankrec1,using 'Form POS 72,N 6',key=k$: newclr ! update the transaction history
05890   let lastrec=val(resp$(1)) !:
        if lastrec+1 <= lrec(clearing) then let nextrec=lastrec+1 else let nextrec=1
05900 L5900: goto L5530
05910 ! ______________________________________________________________________
05920 XIT: let fnxit
05930 ! ______________________________________________________________________
05940   execute "Index "&env$('Q')&"\GLmstr\bankrec.h"&str$(cno)&' '&env$('Q')&"\GLmstr\bankrec-idx.h"&str$(cno) &" 79/3/4 12/1/8 Replace,DupKeys"
