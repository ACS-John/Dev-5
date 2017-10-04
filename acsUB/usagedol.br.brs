00010 ! Replace S:\acsUB\usagedol
00020 ! -- Accumulated Transaction Listing
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnacs,fnlbl,fntxt,fnwait,fntos, fnopenprn,fncloseprn, fndat,fncno,fnerror,fnwait,fnxit,fndate_mmddyy_to_ccyymmdd,fncmdset,fntop,fnpause,fncombof
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim dat$*20,cap$*128,resp$(4)*20,pe2$*30,e2$*30,cnam$*40,text$*80
00080   dim svce$*11,srvnam$(10)*20,srv$(10),data$*256,idx$*256
00090 ! ______________________________________________________________________
00100   fncno(cno,cnam$) !:
        ! 
00110   fndat(dat$)
00120   fntop("S:\acsUB\usagedol",cap$="Usage and Dollar Report")
00130   open #20: "Name="&env$('Q')&"\UBmstr\ubData\Service.h"&str$(cno)&",Shr",internal,input,relative  !:
        read #20,using "Form POS 1,10*C 20,10*C 2",rec=1: mat srvnam$,mat srv$ !:
        close #20: 
00140 ! ______________________________________________________________________
00150 SCREEN1: ! 
00160   let sn$ = "ubAccTr" !:
        fntos(sn$) !:
        let mylen=23 !:
        let mypos=mylen+2
00170   let text$="Report Heading Date:" !:
        fnlbl(1,1,text$,mylen,1)
00180   fntxt(1,mypos,20) !:
        let resp$(1)=dat$
00190   let text$="Starting Date (mmddyy):" !:
        fnlbl(2,1,text$,mylen,1)
00200   fntxt(2,mypos,8,0,0,"1") !:
        let resp$(2)=""
00210   let text$="Ending Date (mmddyy):" !:
        fnlbl(3,1,text$,mylen,1)
00220   fntxt(3,mypos,8,0,0,"1") !:
        let resp$(3)=""
00230   fnlbl(4,1,"Rate for Analysis:",mylen,1)
00240   fncombof("nerd",4,mypos,40,env$('Q')&"\UBmstr\ubData\RateMst.h"&str$(cno),1,4,5,50,env$('Q')&"\UBmstr\ubData\RateIdx1.h"&str$(cno),1,usa) !:
        let usa+=1 !:
        let resp$(4)="" ! just default to the first one
00250   fncmdset(3)
00260   fnacs(sn$,0,mat resp$,ck)
00270   if ck=5 then goto XIT
00280   let dat$=resp$(1) !:
        let d(1)=val(resp$(2)) !:
        let d(2)=val(resp$(3))
00290   let svce$=resp$(4)(1:4) !:
        cde=val(resp$(4)(3:4))
00300   pause 
00310   if d(1)<>0 then let d(1)=fndate_mmddyy_to_ccyymmdd(d(1))
00320   if d(2)<>0 then let d(2)=fndate_mmddyy_to_ccyymmdd(d(2))
00330   fndat(d$(1),2)
00340   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,outin,keyed 
00350   open #2: "Name="&env$('Q')&"\UBmstr\UBTransVB.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\UBTrIndx.h"&str$(cno)&",Shr",internal,input,keyed 
00360   on fkey 5 goto DONE
00370   fnopenprn
00380   gosub HDR
00390 READ_CUSTOMER: ! 
00400   read #1,using L410: z$,e2$,bal eof DONE
00410 L410: form pos 1,c 10,pos 41,c 30,pos 292,pd 4.2
00420   restore #2,key>=z$&"         ": nokey READ_CUSTOMER
00430   let first_trans_per_act=1 ! True
00440 READ_TRANS: ! 
00450   read #2,using L470: p$,tdate,tcode,tamount,tbal eof READ_CUSTOMER
00460 ! If TRIM$(P$)="100550.00" Then Let FNPAUSE
00470 L470: form pos 1,c 10,n 8,n 1,pd 4.2,pd 4.2
00480   if trim$(p$)<>trim$(z$) then goto READ_CUSTOMER
00490   if (d(1)<>0 and tdate<d(1)) or (d(2)<>0 and tdate>d(2)) then !:
          goto READ_TRANS
00500   gosub PRINT_TRANS
00510   let first_trans_per_act=0 ! false
00520   goto READ_TRANS
00530 ! ______________________________________________________________________
00540 DONE: ! 
00550   close #1: ioerr L560
00560 L560: close #2: ioerr L570
00570 L570: close #3: ioerr L580
00580 L580: let fncloseprn
00590   goto XIT
00600 ! ______________________________________________________________________
00610 HDR: ! 
00620   let p2=p2+1
00630   pr #255: "\qc {\b "&cnam$ !:
        pr #255: cap$ !:
        pr #255: dat$
00640   if d(1)<>0 then !:
          pr #255: "Starting Date: "&cnvrt$("pic(zzzz/zz/zz)",d(1))
00650   if d(2)<>0 then !:
          pr #255: "Ending Date: "&cnvrt$("pic(zzzz/zz/zz)",d(2))
00660   pr #255: "\qr Page "&str$(p2)
00670   pr #255: "\qc {\ul Act.Number} {\ul Account Name                } {\ul Trans. Type} {\ul   Date  } {\ul       Amount} {\ul       Balance}}"
00680   return 
00690 ! ______________________________________________________________________
00700 PRINT_TRANS: ! 
00710   if tcode<1 or tcode>5 then let tcode=6
00720   if first_trans_per_act=1 then !:
          let pz$=z$ : let pe2$=e2$ else !:
          let pz$="" : let pe2$=""
00730   pr #255,using L740: pz$,pe2$,code$(tcode),tdate,tamount,tbal pageoflow PGOF
00740 L740: form pos 1,c 10,x 1,c 30,cr 11,nz 9,n 13.2,n 14.2
00750   return 
00760 ! ______________________________________________________________________
00770 PGOF: ! !:
        pr #255: newpage !:
        gosub HDR !:
        continue 
00780 ! ______________________________________________________________________
00790 XIT: let fnxit
00800 ! ______________________________________________________________________
00810 ! <Updateable Region: ERTN>
00820 ERTN: let fnerror(program$,err,line,act$,"xit")
00830   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00840   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00850   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00860 ERTN_EXEC_ACT: execute act$ : goto ERTN
00870 ! /region
00880 ! ______________________________________________________________________
