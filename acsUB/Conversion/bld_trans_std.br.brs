00010 ! Replace S:\acsUB\Conversion\Bld_Trans
00020 ! Builds an ubTransVB from ubTrans.h, UBmstr.h and ubAccTrn.h !:
        ! this conversion must be done before ubmaster is converted to customer
00030 ! this program assumes the following !:
        ! service 1 is Water !:
        ! service 2 is Sewer !:
        ! service 3 is Electric !:
        ! Service 4 is Gas !:
        ! Service 5 is Sanitation !:
        ! Service 6 is Fire Protection !:
        ! Service 7 is Merchandise !:
        ! Service 8 is Other
00040 ! ______________________________________________________________________
00050   library 'S:\Core\Library': fnerror,fncno,fntop,fntos,fnacs,fncmdset,fncmbcno,fnlbl,fndate_mmddyy_to_ccyymmdd,fnchk,fnxit,fnindex_it
00060   on error goto ERTN
00070 ! ______________________________________________________________________
00080   dim cap$*128,resp$(10)*80,g(11),acctrn_form$*80,rw4(22,13),key$*19,ru(6)
00090 ! ______________________________________________________________________
00100   let fntop("S:\acsUB\Conversion\Bld_Trans",cap$="Build Transactions")
00110   let fncno(cno)
00115   pr newpage
00120 LOOP_STEP_1: ! 
00130   gosub MENU1
00140   gosub CONVERT_CNO !:
        goto XIT !:
        ! If CNO<>0 Then Gosub CONVERT_CNO : Goto LOOP_STEP_1 Else Goto XIT
00150 ! ______________________________________________________________________
00160 ! <Updateable Region: ERTN>
00170 ERTN: let fnerror(program$,err,line,act$,"xit")
00180   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00190   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00200   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00210 ERTN_EXEC_ACT: execute act$ : goto ERTN
00220 ! /region
00230 ! ______________________________________________________________________
00240 XIT: chain "S:\acsUB\conversion\UBmstr-vb"
00250 ! ______________________________________________________________________
00260 MENU1: ! 
00270   let fntos(sn$="bldtrans")
00280   let fnlbl(1,1,"Convert Transactions")
00290   let fnchk(4,1,"Delete existing transaction file before conversion") !:
        let resp$(1)="True"
00300   let fnchk(5,1,"Remove Transactions with Bad Dates") !:
        let resp$(2)="False"
00310   let fncmdset(2)
00320   let fnacs(sn$,0,mat resp$,ck)
00330   let delubtransvb$=resp$(1) !:
        let removebaddates$=resp$(2)
00340   if ck=5 then cno=0
00350 ! 
00360   return 
00370 ! ______________________________________________________________________
00380 CONVERT_CNO: ! 
00390   pr "conversion of cno="&str$(cno)&" has begun."
00400 ! 
00410 ! 
00420   if uprc$(delubtransvb$)=uprc$("True") and exists(env$('Q')&"\UBmstr\ubtransvb.h"&str$(cno)) then execute "Free "&env$('Q')&"\UBmstr\ubtransvb.h"&str$(cno)
00430 ! 
00440   open #master=3: "Name="&env$('Q')&"\UBmstr\ubMaster.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,outin,keyed 
00450 ! 
00460 ! open NEW files
00470   open #transvb=11: "Name="&env$('Q')&"\UBmstr\UBTransVB.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\UBTrIndx.h"&str$(cno)&",Shr,RecL=102,KPs=1,KLn=19,Use",internal,outin,keyed 
00480 PHASE1: ! 
00490   pr 'moving trans from ubAccTrn to ubTranVB'
00500   open #acctrn=1: "Name="&env$('Q')&"\UBmstr\ubAccTrn.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubAcTIx1.h"&str$(cno)&",Shr",internal,outin,keyed 
00510   if rln(acctrn)=64 or rln(acctrn)=72 then !:
          acctrn_form$='Form Pos 1,C 10,pd 4.2,N 8,n 1,n 1,10*pd 4.2' !:
        else !:
          if rln(acctrn)=62 or rln(acctrn)=70 then !:
            acctrn_form$='Form Pos 1,C 10,pd 4.2,N 8,n 1,n 1,10*pd 4.2' else !:
            if rln(acctrn)=68 then !:
              acctrn_form$='Form Pos 1,C 10,pd 4.2,n 8,n 1,n 1,10*pd 4.2'
00520 READ_ACCTRN: ! 
00530 L530: read #acctrn,using acctrn_form$: p$,tamt,tdate,transcode,postcode,g(1),g(2),g(3),g(4),g(5),g(6),g(7),g(8),g(9),g(10) eof PHASE2 ioerr R68F
00540   pr fields "1,1,C 20,R,N": str$(accnt+=1)&"/"&str$(lrec(acctrn))
00550   read #master,using 'form pos 1,c 10',key=p$: z$ nokey READ_ACCTRN
00560   gosub TRANSLATE_TRANSCODE
00570   if len(str$(tdate))<=6 then goto L530
00580   let postcode=9
00590   write #transvb,using 'form pos 1,C 10,N 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1': p$,tdate,transcode,tamt,mat g,d1,d3,d5,d7,d9,d11,bal,postcode
00600   goto READ_ACCTRN
00610 PHASE2: ! 
00620   close #acctrn: 
00630 ! 
00640 ! 
00650   pr 'moving trans from ubTrans to ubTranVB'
00660   open #trans=2: "Name="&env$('Q')&"\UBmstr\ubTrans.h"&str$(cno),internal,input 
00670 READ_TRANS: ! 
00680 L680: read #trans,using 'Form Pos 1,C 10,pd 4.2,pd 4,n 1,n 1,pd 3': p$,tamt,tdate,transcode,postcode,nta eof PHASE3
00682   if postcode=5 then goto L680 ! don't get trans from current file that also been transferred to history
00690 ! If TDATE=101504 AND TRANSCODE=1 Then Goto 680 ! temporary !!!! skip october 15, 2004 charges
00700   read #master,using 'form pos 1,c 10,pos 300,11*pd 4.2',key=p$: z$,mat g nokey READ_TRANS
00710   if transcode<>1 then mat g=(0) ! only pull mat g from main record if charge transaction in current file
00720   gosub TRANSLATE_TRANSCODE
00730   if len(str$(tdate))<=6 then let tdate=fndate_mmddyy_to_ccyymmdd(tdate)
00740   write #transvb,using 'form pos 1,C 10,N 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1': p$,tdate,transcode,tamt,mat g,d1,d3,d5,d7,d9,d11,bal,postcode
00750   goto READ_TRANS
00760 PHASE3: ! 
00770   close #trans: 
00780 ! 
00790 ! 
00800   pr 'moving trans from ubMaster to ubTranVB'
00810   restore #master: 
00820 READ_MASTER: ! 
00830   read #master,using 'form pos 1,c 10,pos 438,78*pd 5,13*pd 4.2,13*n 6,156*pd 4.2,13*n 6,13*pd 4.2': p$,mat rw4 eof PHASE4
00840   for month=1 to 13
00850     let tdate=fndate_mmddyy_to_ccyymmdd(rw4(8,month))
00860     if tdate=0 or tdate=20000000 then goto NEXT_MONTH
00870     let g(01)=rw4(09,month) : let g(02)=rw4(10,month) !:
          let g(03)=rw4(11,month) : let g(04)=rw4(12,month) !:
          let g(05)=rw4(13,month) : let g(06)=rw4(14,month) !:
          let g(07)=rw4(15,month) : let g(08)=rw4(16,month) !:
          let g(09)=rw4(17,month) : let g(10)=rw4(18,month) !:
          let g(11)=rw4(19,month)
00880     let ru(1)=rw4(1,month) : let ru(2)=rw4(2,month) !:
          let ru(3)=rw4(3,month) : let ru(4)=rw4(4,month) !:
          let ru(5)=rw4(5,month) : let ru(6)=rw4(6,month) !:
          bal=rw4(7,month) : let postcode=9 !:
          let transcode=1 : let tamt=rw4(19,month)
00890     let key$=p$&lpad$(str$(tdate),8)&str$(transcode) !:
          read #transvb,using 'form pos 1,C 10',key=key$: p$ nokey WRITE_A_RECORD
00900     rewrite #transvb,using 'form pos 24,11*pd 4.2,6*pd 5,pd 4.2,n 1',key=key$: mat g,mat ru,bal,postcode
00910     goto NEXT_MONTH
00920 WRITE_A_RECORD: ! 
00930     write #transvb,using 'form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1': p$,tdate,transcode,tamt,mat g,mat ru,bal,postcode
00940 NEXT_MONTH: next month
00950   goto READ_MASTER
00960 PHASE4: ! 
00970   close #master: 
00980 ! 
00990 ! 
01000   close #transvb: 
01010   pr "ReIndexing ubTransVB..."
01020   let fnindex_it(env$('Q')&"\UBmstr\UBTransVB.h"&str$(cno),env$('Q')&"\UBmstr\UBTrIndx.h"&str$(cno),"1 19")
01030   pr "Transactions for company "&str$(cno)&" were built successfully."
01040   pr ""
01050   if removebaddates$="True" then gosub REMOVEBADDATES
01060   return 
01070 ! ______________________________________________________________________
01080 TRANSLATE_TRANSCODE: ! 
01090   if transcode=1 and postcode=4 then let transcode=5 : goto EOTT !:
          ! Debit Memo
01100   if transcode=1 and postcode<>4 then let transcode=1 : goto EOTT ! charge
01110   if transcode=2 then let transcode=2 : goto EOTT ! penalty
01120   if transcode=3 then let transcode=3 : goto EOTT ! collection
01130   if transcode=4 then let transcode=4 : goto EOTT ! Credit Memo
01140 EOTT: return  ! end of translate transcode
01150 ! ______________________________________________________________________
01160 R68F: ! 
01170   if rln(acctrn)<>68 then goto ERTN
01180   if r68f=0 then !:
          acctrn_form$='Form Pos 1,C 10,pd 4.2,n 8,n 1,n 1,10*pd 4.2' !:
        else !:
          acctrn_form$='Form Pos 1,C 10,pd 4.2,x 2,n 6,n 1,n 1,10*pd 4.2'
01190   if r68f=1 then let r68f=0 else let r68f=1
01200   continue 
01210 ! ______________________________________________________________________
01220 REMOVEBADDATES: ! 
01230   open #transvb=11: "Name="&env$('Q')&"\UBmstr\UBTransVB.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\UBTrIndx.h"&str$(cno)&",Shr,RecL=102,KPs=1,KLn=19,Use",internal,outin,keyed 
01240 L1240: read #transvb,using "Form Pos 11,N 8": tdate eof L1270
01250   let tdate$=str$(tdate) !:
        if val(tdate$(1:4))<1950 or val(tdate$(1:4))>2049 or val(tdate$(5:6))<1 or val(tdate$(5:6))>12 or val(tdate$(7:8))<1 or val(tdate$(7:8))>31 then !:
          delete #transvb: 
01260   goto L1240
01270 L1270: close #transvb: 
01280   return 
