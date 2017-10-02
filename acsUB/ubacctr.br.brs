00010 ! Replace S:\acsUB\ubAccTr
00020 ! -- Accumulated Transaction Listing
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnacs,fnlbl,fntxt,fnwait,fntos, fnopenprn,fncloseprn, fndat,fncno,fnerror,fnwait,fnxit,fndate_mmddyy_to_ccyymmdd,fncmdset,fntop,fnpause
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim dat$*20,cap$*128,resp$(3)*20,pe2$*30,e2$*30,cnam$*40,text$*80
00080 ! ______________________________________________________________________
00090   let fncno(cno,cnam$) !:
        ! 
00100   let fndat(dat$)
00110   let fntop("S:\acsUB\ubAccTr",cap$="Accumulated Transaction List")
00120   code$(1)="Charge" : code$(2)="Penalty" !:
        code$(3)="Collection" : code$(4)="Credit Memo" !:
        code$(5)="Debit Memo" : code$(6)="INVALID !?!"
00130 ! ______________________________________________________________________
00140 SCREEN1: ! 
00150   let sn$ = "ubAccTr" !:
        let fntos(sn$) !:
        let mylen=26 !:
        let mypos=mylen+2
00160   let text$="Report Heading Date:" !:
        let fnlbl(1,1,text$,mylen,1)
00170   let fntxt(1,mypos,20) !:
        let resp$(1)=dat$
00180   let text$="Starting Date (mmddyy):" !:
        let fnlbl(2,1,text$,mylen,1)
00190   let fntxt(2,mypos,8,0,0,"1") !:
        let resp$(2)=""
00200   let text$="Ending Date (mmddyy):" !:
        let fnlbl(3,1,text$,mylen,1)
00210   let fntxt(3,mypos,8,0,0,"1") !:
        let resp$(3)=""
00220   let text$="You may leave Starting Date and/or Ending Date blank to indicate all." !:
        let fnlbl(5,1,text$,75)
00230   let fncmdset(3)
00240   let fnacs(sn$,0,mat resp$,ck)
00250   if ck=5 then goto XIT
00260   let dat$=resp$(1) !:
        let d(1)=val(resp$(2)) !:
        let d(2)=val(resp$(3))
00270   if d(1)<>0 then let d(1)=fndate_mmddyy_to_ccyymmdd(d(1))
00280   if d(2)<>0 then let d(2)=fndate_mmddyy_to_ccyymmdd(d(2))
00290   let fndat(d$(1),2)
00300   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,outin,keyed 
00310   open #2: "Name="&env$('Q')&"\UBmstr\UBTransVB.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\UBTrIndx.h"&str$(cno)&",Shr",internal,input,keyed 
00320   on fkey 5 goto DONE
00330   let fnopenprn
00340   gosub HDR
00350 READ_CUSTOMER: ! 
00360   read #1,using L370: z$,e2$,bal eof DONE
00370 L370: form pos 1,c 10,pos 41,c 30,pos 292,pd 4.2
00380   restore #2,key>=z$&"         ": nokey READ_CUSTOMER
00390   let first_trans_per_act=1 ! True
00400 READ_TRANS: ! 
00410   read #2,using L430: p$,tdate,tcode,tamount,tbal eof READ_CUSTOMER
00420 ! If TRIM$(P$)="100550.00" Then Let FNPAUSE
00430 L430: form pos 1,c 10,n 8,n 1,pd 4.2,pd 4.2
00440   if trim$(p$)<>trim$(z$) then goto READ_CUSTOMER
00450   if (d(1)<>0 and tdate<d(1)) or (d(2)<>0 and tdate>d(2)) then !:
          goto READ_TRANS
00460   gosub PRINT_TRANS
00470   let first_trans_per_act=0 ! false
00480   goto READ_TRANS
00490 ! ______________________________________________________________________
00500 DONE: ! 
00510   close #1: ioerr L520
00520 L520: close #2: ioerr L530
00530 L530: close #3: ioerr L540
00540 L540: let fncloseprn
00550   goto XIT
00560 ! ______________________________________________________________________
00570 HDR: ! 
00580   let p2=p2+1
00590   pr #255: "\qc {\b "&cnam$ !:
        pr #255: cap$ !:
        pr #255: dat$
00600   if d(1)<>0 then !:
          pr #255: "Starting Date: "&cnvrt$("pic(zzzz/zz/zz)",d(1))
00610   if d(2)<>0 then !:
          pr #255: "Ending Date: "&cnvrt$("pic(zzzz/zz/zz)",d(2))
00620   pr #255: "\qr Page "&str$(p2)
00630   pr #255: "\qc {\ul Act.Number} {\ul Account Name                } {\ul Trans. Type} {\ul   Date  } {\ul       Amount} {\ul       Balance}}"
00640   return 
00650 ! ______________________________________________________________________
00660 PRINT_TRANS: ! 
00670   if tcode<1 or tcode>5 then let tcode=6
00680   if first_trans_per_act=1 then !:
          let pz$=z$ : let pe2$=e2$ else !:
          let pz$="" : let pe2$=""
00690   pr #255,using L700: pz$,pe2$,code$(tcode),tdate,tamount,tbal pageoflow PGOF
00700 L700: form pos 1,c 10,x 1,c 30,cr 11,nz 9,n 13.2,n 14.2
00710   return 
00720 ! ______________________________________________________________________
00730 PGOF: ! !:
        pr #255: newpage !:
        gosub HDR !:
        continue 
00740 ! ______________________________________________________________________
00750 XIT: let fnxit
00760 ! ______________________________________________________________________
00770 ! <Updateable Region: ERTN>
00780 ERTN: let fnerror(program$,err,line,act$,"xit")
00790   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00800   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00810   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00820 ERTN_EXEC_ACT: execute act$ : goto ERTN
00830 ! /region
00840 ! ______________________________________________________________________
