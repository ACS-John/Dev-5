00010 ! Replace R:\acsCL\TrJr
00020 ! Print Transaction Journals
00030 ! ______________________________________________________________________
00040   library 'R:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnerror,fncno,fntos,fnlbl,fntxt,fncomboa,fnchk,fncmdset,fnacs,fnwait,fncombof,fndate_mmddyy_to_ccyymmdd
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cnam$*40,vnam$*30,de$*35,slt(3),ti$(3)*20,tr5$*30,item2$(2)*15
00080   dim t1(3),glt(3),glts(3),bn$*30,cap$*128,des$*30,sltyn$(3)*1
00090   dim udf$*256
00100 ! ______________________________________________________________________
00110   let fntop(program$, cap$="Transaction Journals")
00120   let udf$=env$('temp')&'\'
00130   let cancel=99
00140 ! ______________________________________________________________________
00150   let fncno(cno,cnam$)
00160   let ti$(1)="Checks" !:
        let ti$(2)="Deposits" !:
        let ti$(3)="Adjustments"
00170 ! ______________________________________________________________________
00180   open #20: "Name=Q:\CLmstr\Company.h"&str$(cno)&",Shr",internal,input  !:
        read #20,using 'form POS 152,N 2': wbc !:
        close #20: 
00190 ! ______________________________________________________________________
00200 MAIN: ! 
00210   let fntos(sn$="Trjr") !:
        let respc=0
00220   let fnlbl(1,1,"Beginning Date:",38,1)
00230   let fntxt(1,40,10,0,1,"3",0,"Earliest transation date to be shown on journals!") !:
        let resp$(respc+=1)=""
00240   let fnlbl(2,1,"Ending Date:",38,1)
00250   let fntxt(2,40,10,0,1,"3",0,"Last transation date to be shown on journals!") !:
        let resp$(respc+=1)=""
00260   let fnlbl(4,1,"Information to Print:",38,1)
00270   let item2$(1)="Details" !:
        let item2$(2)="Totals Only"
00280   let fncomboa("claims-act",4,40,mat item2$) !:
        let resp$(respc+=1)=item2$(1)
00290   let fnchk(7,40,"Print Disbursments Journal:",1) !:
        let resp$(respc+=1)="True"
00300   let fnchk(8,40,"Print Receipts Journal:",1) !:
        let resp$(respc+=1)="True"
00310   let fnchk(9,40,"Print Adjustments Journal:",1) !:
        let resp$(respc+=1)="False"
00320   let fnlbl(11,1,"Bank Account:",38,1)
00330   let fncombof("Bankmstr",11,40,20,"Q:\CLmstr\bankmstr.h"&str$(cno),1,2,3,15,"Q:\CLmstr\Bankidx1.h"&str$(cno),1,0, "Select bank account for printing") !:
        let resp$(respc+=1)=str$(wbc)
00340   let fncmdset(2) !:
        let fnacs(sn$,0,mat resp$,ck)
00350   if ck=5 then goto XIT
00360   let dt1=val(resp$(1)) ! beginning date !:
        let dt2=val(resp$(2)) ! ending date !:
        let td1yn$=resp$(3)(1:1) !  detail
00370   if resp$(4)(1:1)="T" then let sltyn$(1)="Y": let slt(1)=1 !:
        else let sltyn$(1)="N" ! disb jrn
00380   if resp$(5)(1:1)="T" then let sltyn$(2)="Y" : let slt(2)=1 !:
        else let sltyn$(2)="N" ! rec jrn
00390   if resp$(6)(1:1)="T" then let sltyn$(3)="Y" : let slt(3)=1 !:
        else let sltyn$(3)="N" ! adj jrn
00400   let wbc=val(resp$(7)(1:2))
00410 ! FNWAIT
00420   open #trmstr=1: "Name=Q:\CLmstr\TrMstr.h"&str$(cno)&",KFName=Q:\CLmstr\TrIdx1.h"&str$(cno)&",Shr",internal,input,keyed 
00430   open #tralloc=2: "Name=Q:\CLmstr\TrAlloc.h"&str$(cno)&",KFName=Q:\CLmstr\TrAlloc-Idx.h"&str$(cno)&",Shr",internal,input,keyed 
00440   open #glmstr=4: "Name=Q:\CLmstr\GLmstr.H"&str$(cno)&",KFName=Q:\CLmstr\GLIndex.h"&str$(cno)&",Shr",internal,input,keyed 
00450   open #work=3: "Name="&udf$&"WORK,KFName="&udf$&"ADDR,RecL=40,KPS=1,KLN=12,Replace",internal,outin,keyed  !:
        ! this file is used to total Amounts by General Ledger Number
00460   open #bankmstr=12: "Name=Q:\CLmstr\BankMstr.h"&str$(cno)&",KFName=Q:\CLmstr\BankIdx1.h"&str$(cno)&",Shr",internal,outin,keyed 
00470   read #bankmstr,using 'Form POS 3,C 30,C 12,PD 6.2',key=cnvrt$("N 2",wbc),release: bn$ nokey MAIN
00480   close #bankmstr: 
00490   let bn$=rtrm$(bn$)
00500   let fnopenprn
00510 END1: ! 
00520   if wcd=0 or td1yn$="T" then goto HERE
00530   print #255,using 'Form POS 52,G 12.2': "  __________"
00540   print #255,using 'Form POS 52,G 12.2': t1(wcd)
00550   let npg=1
00560 HERE: if wcd>2 then goto ENDALL
00570   let wcd+=1
00580   if slt(wcd)<>1 then goto HERE
00590   if npg=1 then !:
          print #255: newpage
00600   let npg=0
00610   if td1yn$="D" then gosub HDR
00620   restore #trmstr,key>=lpad$(str$(wbc),2)&str$(wcd)&"        ": nokey END1
00630 READ_TRMSTR: ! 
00640   read #trmstr,using 'Form POS 1,N 2,N 1,C 8,G 6,pd 10.2,C 8,C 35': bank_code,tcde,ck$,tr2,amt,vn$,de$ eof ENDALL
00650   if tcde=1 then let de$=rpad$(ltrm$(vn$),8)&" "&de$(1:26)
00660   if bank_code><wbc then goto ENDALL
00670   if tcde><wcd then goto END1
00680   if fndate_mmddyy_to_ccyymmdd(tr2)<dt1 or fndate_mmddyy_to_ccyymmdd(tr2)>dt2 then !:
          goto READ_TRMSTR
00690   let sq$=" "
00700   if tcde><1 then goto L750
00710   let ck1=val(ck$) conv L750
00720   if ck2=0 then goto L740
00730   if ck2+1><ck1 then let sq$="*"
00740 L740: let ck2=ck1
00750 L750: if td1yn$="D" then !:
          print #255,using 'Form POS 1,C 2,C 10,PIC(ZZ/ZZ/ZZBB),C 29,N 12.2': sq$,ck$,tr2,de$(1:29),amt pageoflow NEWPGE
00760   let t1(wcd)+=amt
00770 RESTORE_TRALLOC: ! 
00775   let totalalloc=0 ! kj 52307
00780   let key$=cnvrt$("Pic(zz)",bank_code)&str$(tcde)&ck$ !:
        restore #tralloc,key>=key$: nokey PRINT_D_NEWPAGE
00790 READ_TRALLOC: ! 
00800   read #tralloc,using 'Form POS 1,C 11,C 12,PD 5.2,C 30,G 6': newkey$,gl$,am2,tr5$,ivd$ eof PRINT_D_NEWPAGE !:
        if newkey$<>key$ then goto PRINT_D_NEWPAGE else !:
          if am2=0 then goto READ_TRALLOC
00805   let totalalloc+=am2 ! kj 52307
00810   if wcd=2 then let am2=-am2
00820   if td1yn$="D" then !:
          print #255,using 'Form POS 66,C 12,N 11.2,X 2,C 32,c 6': gl$,am2,tr5$,ivd$ pageoflow NEWPGE
00830   goto SUMMARY_MAYBE
00840 ! ______________________________________________________________________
00850 PRINT_D_NEWPAGE: ! 
00855   if amt<>totalalloc then print #255,using "form pos 1,c 80": "The allocations on the above transaction do not agree with total transaction" ! kj 52307
00860   if td1yn$="D" then !:
          print #255: pageoflow NEWPGE !:
          ! if condition was left off and printed many blank pages if chose !:
          ! totals only 4/04/01
00870   goto READ_TRMSTR
00880 ! ______________________________________________________________________
00890 NEWPGE: print #255: newpage: gosub HDR : continue 
00900 ! ______________________________________________________________________
00910 HDR: ! 
00920   print #255,using 'Form POS 1,C 8,Cc 74': date$,cnam$
00930   if end3=0 then !:
          print #255,using 'Form POS 1,C 8,POS 24,CC 40': time$,"Bank # "&str$(wbc)&" "&bn$ !:
          print #255,using 'Form POS 24,Cc 40': ti$(wcd)&" Journal"
00940   if end3=1 then !:
          print #255,using 'Form POS 1,C 8,POS 24,CC 40': time$,"Bank # "&str$(wbc)&" "&bn$ !:
          print #255,using 'Form POS 24,Cc 40': " General Ledger Recap"
00950   print #255,using 'Form POS 1,C 26,C 60': "Page "&str$(pg+=1),"Date From: "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",dt1)&" Date To: "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",dt2)
00960   if end3=1 then goto EOHDR
00970   print #255: "                                                                                  Item                                    Invoice "
00980   if wcd=1 then let ref$="   Check # " else let ref$="   Ref #   "
00990   print #255: ref$& "   Date    Payee/Description                  Amount   GL Number      Amount   Item Description                  Date  "
01000   print #255: "  ________  ________  _______________________________ __________ ____________ __________  _______________________________ ________"
01010 EOHDR: return 
01020 ! ______________________________________________________________________
01030 ENDALL: if slt(wcd)=0 then goto L1080
01040   if td1yn$="D" then !:
          print #255,using 'Form POS 52,G 12.2': "  __________"
01050   if td1yn$="D" then !:
          print #255,using 'Form POS 52,G 12.2': t1(wcd)
01060   if td1yn$="T" then let end3=1 : gosub HDR
01070   if td1yn$="T" then !:
          print #255: "" !:
          print #255,using 'Form POS 1,C 60': "______________________________________" !:
          print #255: ""
01080 L1080: for j=1 to 3 !:
          print #255,using 'Form POS 7,C 18,N 12.2': "Total "&ti$(j),t1(j) !:
        next j
01090   if td1yn$="T" then !:
          print #255: "" !:
          print #255,using 'Form POS 1,C 60': "______________________________________" !:
          print #255: ""
01100   gosub RESTORE_WORK
01110   let fncloseprn
01120   goto XIT
01130 ! ______________________________________________________________________
01140 XIT: let fnxit
01150 ! ______________________________________________________________________
01160 SUMMARY_MAYBE: ! 
01170   mat glt=(0)
01180   read #work,using 'Form POS 1,C 12,3*PD 6.2',key=gl$: gl$,mat glt nokey READ_WORK_NOKEY
01190   let glt(wcd)+=am2
01200   rewrite #work,using 'Form POS 1,C 12,3*PD 6.2': gl$,mat glt
01210   goto READ_TRALLOC ! was restore_tralloc
01220 READ_WORK_NOKEY: let glt(wcd)=am2
01230   write #work,using 'Form POS 1,C 12,3*PD 6.2': gl$,mat glt
01240   goto READ_TRALLOC ! was RESTORE_TRALLOC (2)
01250 RESTORE_WORK: ! 
01260   restore #work,key>="            ": nokey END3
01270   if td1yn$<>"T" then !:
          print #255: newpage !:
          let end3=1 : gosub HDR
01280   print #255: "  GL Number   Disbursments    Receipts     Adjustments"
01290   print #255: "____________  ____________  ____________  ____________"
01300 READ_WORK: ! 
01310   read #work,using 'Form POS 1,C 12,3*PD 6.2': gl$,mat glt eof END3
01320   if gl$(1:3)=hgl$(1:3) or val(hgl$)=0 then goto L1350
01330 L1330: print #255,using 'Form POS 1,C 60': "____________  ____________  ____________  ____________" !:
        print #255,using 'Form POS 13,3*N 14.2': mat glts !:
        print #255: ""
01340   mat glts=(0)
01350 L1350: let hgl$=gl$ : mat glts=glts+glt
01360   if subcode=1 then goto END3B
01370   let des$=""
01380   read #glmstr,using 'Form POS 13,C 30',key=gl$: des$ nokey L1390
01390 L1390: print #255,using 'Form POS 1,C 12,3*N 14.2,X 2,C 30': gl$,mat glt,des$
01400   goto READ_WORK
01410 ! ___________________________
01420 END3: ! 
01430   if val(hgl$(1:3))<>0 then let subcode=1: goto L1330
01440 END3B: ! 
01450   print #255: "____________  ____________  ____________  ____________"
01460   print #255,using 'Form POS 1,C 12,3*N 14.2,X 2,C 30': "   Totals",mat t1
01470   return 
01480 ! ______________________________________________________________________
01490 ! <Updateable Region: ERTN>
01500 ERTN: let fnerror(cap$,err,line,act$,"xit")
01510   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
01520   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01530   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
01540 ERTN_EXEC_ACT: execute act$ : goto ERTN
01550 ! /region
01560 ! ______________________________________________________________________
