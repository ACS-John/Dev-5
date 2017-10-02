00010 ! formerly S:\acsUB\ubUnBill
00020 ! ______________________________________________________________________
00030   library 'S:\Core\Library': fnacs,fnlbl,fntxt,fnwait,fncmbrt2,fntos,fndat,fnerror,fnopenprn,fncloseprn,fnxit,fnd1,fncmdset,fntop,fnchk
00040   on error goto ERTN
00050 ! ______________________________________________________________________
00060   dim z$*10,e$(4)*30,dat$*20,resp$(10)*40,text$*40, cap$*128
00070 ! ______________________________________________________________________
00090   let fnd1(d1)
00100   let fndat(dat$)
00110   let fntop(program$)
00120 ! ______________________________________________________________________
00130 MAIN: ! 
00140   let fntos(sn$:="UBUnBill") 
00150   let mylen=20 
00160   let mypos=mylen+2
00170   let fnlbl(1,1,"Report Heading Date:" ,mylen,1)
00180   let fntxt(1,mypos,20) 
00190   let resp$(1) = dat$
00200   let fnlbl(2,1,"Billing Date:" ,mylen,1)
00202   let fntxt(2,mypos,8,8,0,"1") 
00204   let resp$(2)=str$(d1)
00206   let fnlbl(3,1,"Route Number:" ,mylen,1)
00208   let fncmbrt2(3,mypos) 
00209   let resp$(3)="[All]"
00210   let fnchk(4,23,"Print Meter Address:",1)
00220   let resp$(4)="True"
00230   let fncmdset(3): let fnacs(sn$,0,mat resp$,ck)
00240   if ck=5 then goto XIT
00250   let dat$ = resp$(1) 
00252   let d1 = val(resp$(2))
00260   if resp$(3)="[All]" then let prtbkno=0 else let prtbkno = val(resp$(3))
00270   if resp$(4)="True" then let printadr=1 ! wants meter address printed
00280   if d1<10100 or d1>123199 then goto MAIN
00290   let fndat(dat$,2)
00300   on fkey 5 goto DONE
00310   let fnopenprn
00320   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndx5.h"&env$('cno')&",Shr",internal,input,keyed 
00330   gosub HDR
00340   if prtbkno=0 then goto READ_CUSTOMER
00350   let prtbkno$=lpad$(str$(prtbkno),2)&"       "
00360   let startcd=1
00370   restore #1,key>=prtbkno$: nokey TOTALS
00380   goto L410
00390 ! ______________________________________________________________________
00400 READ_CUSTOMER: ! 
00410 L410: read #1,using L420: z$,mat e$,final,bal,f,route eof TOTALS
00420 L420: form pos 1,c 10,4*c 30,pos 1821,n 1,pos 292,pd 4.2,pd 4,pos 1741,n 2
00430   if f=d1 then goto READ_CUSTOMER
00440   if final=1 or final=2 then goto READ_CUSTOMER           ! Skip if InActive
00450   if startcd=1 then goto L460 else goto PRINT_IT
00460 L460: if prtbkno=route then goto PRINT_IT else goto TOTALS
00470 PRINT_IT: ! 
00475   if final=3 then let final$="Final=3" else let final$=""
00476   if final=4 then let final$="Final=4"
00480   if printadr=1 then pr #255,using L490: z$,e$(2),f,bal,e$(1)(1:25),final$ pageoflow PGOF else pr #255,using L491: z$,e$(2),f,bal,final$ pageoflow PGOF
00490 L490: form pos 1,c 10,pos 13,c 30,pos 45,pic(zz/zz/zz),n 15.2,x 2,c 25,x 2,c 8
00491 L491: form pos 1,c 10,pos 13,c 30,pos 45,pic(zz/zz/zz),n 15.2,x 2,c 8
00500   let tbal=tbal+bal
00510   goto READ_CUSTOMER
00520 ! ______________________________________________________________________
00530 HDR: ! 
00540   let p2=p2+1
00550   pr #255: "\qc  {\f181 \fs18 \b "&env$('cnam')&"}"
00560   pr #255: "\qc  {\f181 \fs24 \b "&env$('program_caption')&"}"
00570   pr #255: "\qc  {\f181 \fs16 \b "&trim$(dat$)&"}"
00580   pr #255,using L590: "\ql "&date$,"Page "&str$(p2)
00590 L590: form pos 1,c 82,c 10
00600   pr #255: ""
00610 ! pr #255: "{\ul Account    }                               {\ul Date of}         Current"
00620   if printadr<>1 then pr #255: " {\ul Number   }  {\ul Name             }             {\ul Last Billing}      {\ul Balance}"
00630   if printadr=1 then pr #255: " {\ul Number   }  {\ul Name             }             {\ul Last Billing}      {\ul Balance}  {\ul Meter Address}"
00640   pr #255: ""
00650   return 
00660 ! ______________________________________________________________________
00670 PGOF: pr #255: newpage : gosub HDR : continue 
00680 ! ______________________________________________________________________
00690 TOTALS: ! 
00700   pr #255: rpt$(" ",55)&"{\ul             }" 
00702   pr #255,using "Form POS 56,N 12.2": tbal 
00704   pr #255: rpt$(" ",55)&"{\ul \strike             }"
00710 DONE: close #1: ioerr L720
00720 L720: let fncloseprn
00730 XIT: let fnxit
00740 ! ______________________________________________________________________
00750 ! <Updateable Region: ERTN>
00760 ERTN: let fnerror(program$,err,line,act$,"xit")
00770   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00780   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00790   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00800 ERTN_EXEC_ACT: execute act$ : goto ERTN
00810 ! /region
