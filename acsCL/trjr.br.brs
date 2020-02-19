00010 ! Replace S:\acsCL\TrJr
00020 ! pr Transaction Journals
00030 !
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnerror,fncno,fnTos,fnLbl,fnTxt,fncomboa,fnChk,fnCmdSet,fnAcs,fnwait,fncombof,fndate_mmddyy_to_ccyymmdd
00050   on error goto Ertn
00060 !
00070   dim cnam$*40,vnam$*30,de$*35,slt(3),ti$(3)*20,tr5$*30,item2$(2)*15
00080   dim t1(3),glt(3),glts(3),bn$*30,cap$*128,des$*30,sltyn$(3)*1
00090   dim udf$*256
00100 !
00110   fntop(program$, cap$="Transaction Journals")
00120   udf$=env$('temp')&'\'
00130   cancel=99
00140 !
00150   fncno(cno,cnam$)
00160   ti$(1)="Checks" !:
        ti$(2)="Deposits" !:
        ti$(3)="Adjustments"
00170 !
00180   open #20: "Name=[Q]\CLmstr\Company.h[cno],Shr",internal,input  !:
        read #20,using 'form POS 152,N 2': wbc !:
        close #20: 
00190 !
00200 MAIN: ! 
00210   fnTos(sn$="Trjr") !:
        respc=0
00220   fnLbl(1,1,"Beginning Date:",38,1)
00230   fnTxt(1,40,10,0,1,"3",0,"Earliest transation date to be shown on journals!") !:
        resp$(respc+=1)=""
00240   fnLbl(2,1,"Ending Date:",38,1)
00250   fnTxt(2,40,10,0,1,"3",0,"Last transation date to be shown on journals!") !:
        resp$(respc+=1)=""
00260   fnLbl(4,1,"Information to Print:",38,1)
00270   item2$(1)="Details" !:
        item2$(2)="Totals Only"
00280   fncomboa("claims-act",4,40,mat item2$) !:
        resp$(respc+=1)=item2$(1)
00290   fnChk(7,40,"Print Disbursments Journal:",1) !:
        resp$(respc+=1)="True"
00300   fnChk(8,40,"Print Receipts Journal:",1) !:
        resp$(respc+=1)="True"
00310   fnChk(9,40,"Print Adjustments Journal:",1) !:
        resp$(respc+=1)="False"
00320   fnLbl(11,1,"Bank Account:",38,1)
00330   fncombof("Bankmstr",11,40,20,"[Q]\CLmstr\bankmstr.h[cno]",1,2,3,15,"[Q]\CLmstr\Bankidx1.h[cno]",1,0, "Select bank account for printing") !:
        resp$(respc+=1)=str$(wbc)
00340   fnCmdSet(2) !:
        fnAcs(sn$,0,mat resp$,ck)
00350   if ck=5 then goto XIT
00360   dt1=val(resp$(1)) ! beginning date !:
        dt2=val(resp$(2)) ! ending date !:
        td1yn$=resp$(3)(1:1) !  detail
00370   if resp$(4)(1:1)="T" then sltyn$(1)="Y": slt(1)=1 !:
        else sltyn$(1)="N" ! disb jrn
00380   if resp$(5)(1:1)="T" then sltyn$(2)="Y" : slt(2)=1 !:
        else sltyn$(2)="N" ! rec jrn
00390   if resp$(6)(1:1)="T" then sltyn$(3)="Y" : slt(3)=1 !:
        else sltyn$(3)="N" ! adj jrn
00400   wbc=val(resp$(7)(1:2))
00410 ! FNWAIT
00420   open #trmstr=1: "Name=[Q]\CLmstr\TrMstr.h[cno],KFName=[Q]\CLmstr\TrIdx1.h[cno],Shr",internal,input,keyed 
00430   open #tralloc=2: "Name=[Q]\CLmstr\TrAlloc.h[cno],KFName=[Q]\CLmstr\TrAlloc-Idx.h[cno],Shr",internal,input,keyed 
00440   open #glmstr=4: "Name=[Q]\CLmstr\GLmstr.H[cno],KFName=[Q]\CLmstr\GLIndex.h[cno],Shr",internal,input,keyed 
00450   open #work=3: "Name="&udf$&"WORK,KFName="&udf$&"ADDR,RecL=40,KPS=1,KLN=12,Replace",internal,outIn,keyed  !:
        ! this file is used to total Amounts by General Ledger Number
00460   open #bankmstr=12: "Name=[Q]\CLmstr\BankMstr.h[cno],KFName=[Q]\CLmstr\BankIdx1.h[cno],Shr",internal,outIn,keyed 
00470   read #bankmstr,using 'Form POS 3,C 30,C 12,PD 6.2',key=cnvrt$("N 2",wbc),release: bn$ nokey MAIN
00480   close #bankmstr: 
00490   bn$=rtrm$(bn$)
00500   fnopenprn
00510 END1: ! 
00520   if wcd=0 or td1yn$="T" then goto HERE
00530   pr #255,using 'Form POS 52,G 12.2': "  __________"
00540   pr #255,using 'Form POS 52,G 12.2': t1(wcd)
00550   npg=1
00560 HERE: if wcd>2 then goto ENDALL
00570   wcd+=1
00580   if slt(wcd)<>1 then goto HERE
00590   if npg=1 then !:
          pr #255: newpage
00600   npg=0
00610   if td1yn$="D" then gosub HDR
00620   restore #trmstr,key>=lpad$(str$(wbc),2)&str$(wcd)&"        ": nokey END1
00630 READ_TRMSTR: ! 
00640   read #trmstr,using 'Form POS 1,N 2,N 1,C 8,G 6,pd 10.2,C 8,C 35': bank_code,tcde,ck$,tr2,amt,vn$,de$ eof ENDALL
00650   if tcde=1 then de$=rpad$(ltrm$(vn$),8)&" "&de$(1:26)
00660   if bank_code><wbc then goto ENDALL
00670   if tcde><wcd then goto END1
00680   if fndate_mmddyy_to_ccyymmdd(tr2)<dt1 or fndate_mmddyy_to_ccyymmdd(tr2)>dt2 then !:
          goto READ_TRMSTR
00690   sq$=" "
00700   if tcde><1 then goto L750
00710   ck1=val(ck$) conv L750
00720   if ck2=0 then goto L740
00730   if ck2+1><ck1 then sq$="*"
00740 L740: ck2=ck1
00750 L750: if td1yn$="D" then !:
          pr #255,using 'Form POS 1,C 2,C 10,PIC(ZZ/ZZ/ZZBB),C 29,N 12.2': sq$,ck$,tr2,de$(1:29),amt pageoflow NEWPGE
00760   t1(wcd)+=amt
00770 RESTORE_TRALLOC: ! 
00775   totalalloc=0 ! kj 52307
00780   key$=cnvrt$("Pic(zz)",bank_code)&str$(tcde)&ck$ !:
        restore #tralloc,key>=key$: nokey PRINT_D_NEWPAGE
00790 READ_TRALLOC: ! 
00800   read #tralloc,using 'Form POS 1,C 11,C 12,PD 5.2,C 30,G 6': newkey$,gl$,am2,tr5$,ivd$ eof PRINT_D_NEWPAGE !:
        if newkey$<>key$ then goto PRINT_D_NEWPAGE else !:
          if am2=0 then goto READ_TRALLOC
00805   totalalloc+=am2 ! kj 52307
00810   if wcd=2 then am2=-am2
00820   if td1yn$="D" then !:
          pr #255,using 'Form POS 66,C 12,N 11.2,X 2,C 32,c 6': gl$,am2,tr5$,ivd$ pageoflow NEWPGE
00830   goto SUMMARY_MAYBE
00840 !
00850 PRINT_D_NEWPAGE: ! 
00855   if amt<>totalalloc then pr #255,using "form pos 1,c 80": "The allocations on the above transaction do not agree with total transaction" ! kj 52307
00860   if td1yn$="D" then !:
          pr #255: pageoflow NEWPGE !:
          ! if condition was left off and printed many blank pages if chose !:
          ! totals only 4/04/01
00870   goto READ_TRMSTR
00880 !
00890 NEWPGE: pr #255: newpage: gosub HDR : continue 
00900 !
00910 HDR: ! 
00920   pr #255,using 'Form POS 1,C 8,Cc 74': date$,cnam$
00930   if end3=0 then !:
          pr #255,using 'Form POS 1,C 8,POS 24,CC 40': time$,"Bank # "&str$(wbc)&" "&bn$ !:
          pr #255,using 'Form POS 24,Cc 40': ti$(wcd)&" Journal"
00940   if end3=1 then !:
          pr #255,using 'Form POS 1,C 8,POS 24,CC 40': time$,"Bank # "&str$(wbc)&" "&bn$ !:
          pr #255,using 'Form POS 24,Cc 40': " General Ledger Recap"
00950   pr #255,using 'Form POS 1,C 26,C 60': "Page "&str$(pg+=1),"Date From: "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",dt1)&" Date To: "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",dt2)
00960   if end3=1 then goto EOHDR
00970   pr #255: "                                                                                  Item                                    Invoice "
00980   if wcd=1 then ref$="   Check # " else ref$="   Ref #   "
00990   pr #255: ref$& "   Date    Payee/Description                  Amount   GL Number      Amount   Item Description                  Date  "
01000   pr #255: "  ________  ________  _______________________________ __________ ____________ __________  _______________________________ ________"
01010 EOHDR: return 
01020 !
01030 ENDALL: if slt(wcd)=0 then goto L1080
01040   if td1yn$="D" then !:
          pr #255,using 'Form POS 52,G 12.2': "  __________"
01050   if td1yn$="D" then !:
          pr #255,using 'Form POS 52,G 12.2': t1(wcd)
01060   if td1yn$="T" then end3=1 : gosub HDR
01070   if td1yn$="T" then !:
          pr #255: "" !:
          pr #255,using 'Form POS 1,C 60': "______________________________________" !:
          pr #255: ""
01080 L1080: for j=1 to 3 !:
          pr #255,using 'Form POS 7,C 18,N 12.2': "Total "&ti$(j),t1(j) !:
        next j
01090   if td1yn$="T" then !:
          pr #255: "" !:
          pr #255,using 'Form POS 1,C 60': "______________________________________" !:
          pr #255: ""
01100   gosub RESTORE_WORK
01110   fncloseprn
01120   goto XIT
01130 !
01140 XIT: fnxit
01150 !
01160 SUMMARY_MAYBE: ! 
01170   mat glt=(0)
01180   read #work,using 'Form POS 1,C 12,3*PD 6.2',key=gl$: gl$,mat glt nokey READ_WORK_NOKEY
01190   glt(wcd)+=am2
01200   rewrite #work,using 'Form POS 1,C 12,3*PD 6.2': gl$,mat glt
01210   goto READ_TRALLOC ! was restore_tralloc
01220 READ_WORK_NOKEY: glt(wcd)=am2
01230   write #work,using 'Form POS 1,C 12,3*PD 6.2': gl$,mat glt
01240   goto READ_TRALLOC ! was RESTORE_TRALLOC (2)
01250 RESTORE_WORK: ! 
01260   restore #work,key>="            ": nokey END3
01270   if td1yn$<>"T" then !:
          pr #255: newpage !:
          end3=1 : gosub HDR
01280   pr #255: "  GL Number   Disbursments    Receipts     Adjustments"
01290   pr #255: "____________  ____________  ____________  ____________"
01300 READ_WORK: ! 
01310   read #work,using 'Form POS 1,C 12,3*PD 6.2': gl$,mat glt eof END3
01320   if gl$(1:3)=hgl$(1:3) or val(hgl$)=0 then goto L1350
01330 L1330: pr #255,using 'Form POS 1,C 60': "____________  ____________  ____________  ____________" !:
        pr #255,using 'Form POS 13,3*N 14.2': mat glts !:
        pr #255: ""
01340   mat glts=(0)
01350 L1350: hgl$=gl$ : mat glts=glts+glt
01360   if subcode=1 then goto END3B
01370   des$=""
01380   read #glmstr,using 'Form POS 13,C 30',key=gl$: des$ nokey L1390
01390 L1390: pr #255,using 'Form POS 1,C 12,3*N 14.2,X 2,C 30': gl$,mat glt,des$
01400   goto READ_WORK
01410 ! ___________________________
01420 END3: ! 
01430   if val(hgl$(1:3))<>0 then subcode=1: goto L1330
01440 END3B: ! 
01450   pr #255: "____________  ____________  ____________  ____________"
01460   pr #255,using 'Form POS 1,C 12,3*N 14.2,X 2,C 30': "   Totals",mat t1
01470   return 
01480 !
01490 ! <Updateable Region: ERTN>
01500 ERTN: fnerror(program$,err,line,act$,"xit")
01510   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
01520   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01530   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01540 ERTN_EXEC_ACT: execute act$ : goto ERTN
01550 ! /region
01560 !
