00010 ! Replace S:\acsUB\Escrow1
00020 ! ______________________________________________________________________
00030   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fntop,fntos,fnacs,fnlbl,fntxt,fndat,fnxit,fncomboa,fncmdset,fnopenprn,fncloseprn
00040   on error goto ERTN
00050 ! ______________________________________________________________________
00060   dim cnam$*40,customer_name$*30,cap$*128,resp$(2)*20
00070 ! ______________________________________________________________________
00080   fntop("S:\acsUB\escrow1",cap$="Escrow Balance Report")
00090   fncno(cno,cnam$)
00100   fndat(resp$(1))
00110 ! 
00120   fntos(sn$="escrow1") !:
        let mylen=20 : let mypos=mylen+2 : lc=0
00130   fnlbl(lc+=1,1,"Report Heading Date:",mylen,1)
00140   fntxt(lc,mypos,20)
00150   fnlbl(lc+=1,1,"Sort by:",mylen,1)
00160   opt$(1)="1. Account" : opt$(2)="2. Name" : mat opt$(2) !:
        fncomboa("acc_or_nam",lc,mypos,mat opt$) !:
        let resp$(2)=opt$(1)
00170   fncmdset(3)
00180   fnacs(sn$,0,mat resp$,ck)
00190   if ck=5 then goto XIT
00200   fndat(resp$(1),put=2)
00210   customer=1 !:
        if resp$(2)=opt$(1) then !:
          open #customer: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,input,keyed else !:
          if resp$(2)=opt$(2) then !:
            open #customer: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\UBIndx2.h"&str$(cno)&",Shr",internal,input,keyed 
00220   fnopenprn
00230   on pageoflow goto PGOF
00240   gosub HDR
00250 READ_CUSTOMER: ! 
00260   read #customer,using 'Form POS 1,C 10,pos 41,C 30,POS 1859,PD 5.2': z$, customer_name$, escrow_bal eof DONE
00270   if escrow_bal=0 then goto READ_CUSTOMER
00280   pr #255,using 'Form POS 1,C 12,C 30,N 12.2': z$,customer_name$,escrow_bal
00290   let total_escrow+=escrow_bal
00300   goto READ_CUSTOMER
00310 ! ______________________________________________________________________
00320 PGOF: pr #255: newpage : gosub HDR : continue 
00330 ! ______________________________________________________________________
00340 HDR: ! 
00350   pr #255,using 'Form POS 20,Cc 40': "",cnam$
00360   pr #255,using 'Form POS 1,C 10,pos 20,Cc 40': "Page "&str$(pg+=1),cap$
00370   pr #255,using 'Form POS 1,C 10,pos 20,Cc 40': date$,resp$(1)
00380   pr #255: ""
00390   pr #255: "Account No  Customer Name                   Escrow Bal"
00400   pr #255: "__________  ______________________________  __________"
00410   return 
00420 ! ______________________________________________________________________
00430 DONE: ! 
00440   pr #255: tab(43);"  __________"
00450   pr #255,using 'Form POS 43,N 12.2': total_escrow
00460   fncloseprn
00470   goto XIT
00480 ! ______________________________________________________________________
00490 XIT: let fnxit
00500 ! ______________________________________________________________________
00510 ! <Updateable Region: ERTN>
00520 ERTN: let fnerror(program$,err,line,act$,"xit")
00530   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00540   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00550   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00560 ERTN_EXEC_ACT: execute act$ : goto ERTN
00570 ! /region
00580 ! ______________________________________________________________________
