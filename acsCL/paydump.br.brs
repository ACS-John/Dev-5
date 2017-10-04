00010 ! Replace S:\acsCL\PayDump
00020 ! Remove Payee Records
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnopenprn,fncloseprn,fncno,fnerror,fndat,fntop,fnxit,fntos,fnlbl,fntxt,fncmdset,fnacs,fndate_mmddyy_to_ccyymmdd,fngethandle
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim nam$*30,cnam$*40,dat$*20,gl(3),tr$(5)*35,cap$*128
00080 ! ______________________________________________________________________
00090   fntop(program$,cap$="Remove Payee Records")
00100   cancel=99 : let right=1
00110   fncno(cno,cnam$) !:
        fndat(dat$)
00120 ! ______________________________________________________________________
00130   open #20: "Name="&env$('Q')&"\CLmstr\Company.h"&str$(cno)&",Shr",internal,input,relative: read #20,using 'Form POS 150,2*N 1',rec=1: mat d !:
        close #20: 
00140   open #trmstr2=22: "Name="&env$('Q')&"\CLmstr\TrMstr.H"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\TrIdx2.H"&str$(cno)&",Shr",internal,input,keyed 
00150   open #paymstr1=1: "Name="&env$('Q')&"\CLmstr\PayMstr.H"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\PayIdx1.H"&str$(cno)&",Shr",internal,outin,keyed 
00160   open #paymstr2=2: "Name="&env$('Q')&"\CLmstr\PayMstr.H"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\PayIdx2.H"&str$(cno)&",Shr",internal,outin,keyed 
00170   open #payeeglbreakdown:=fngethandle: "Name="&env$('Q')&"\CLmstr\PayeeGLBreakdown.h"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\PayeeGLBkdidx.h"&str$(cno)&",Shr",internal,outin,keyed 
00180   fntos(sn$="PayDump") !:
        let respc=0 : let mylen=21 : let mypos=mylen+2
00190   fnlbl(1,1,"Oldest retained Date:",mylen,right)
00200   fntxt(1,mypos,10,0,1,"1003",0,"This program will dump payee records who have not received a check since a certain date.") !:
        let resp$(respc+=1)=str$(date('ccyymmdd')-50000)
00210   fnlbl(1,46,"",1,1)
00220   fncmdset(2) !:
        fnacs(sn$,0,mat resp$,ckey)
00230   if ckey=5 then goto XIT else !:
          let olddate=val(resp$(1))
00240   fnopenprn
00250   gosub HDR
00260 ! ___________________________
00270 READ_PAYMSTR1: ! 
00280   read #paymstr1,using 'Form POS 1,C 8,C 30': vn$,nam$ eof DONE
00290   restore #trmstr2,search>=vn$: nokey PRINT_IT
00300 READ_TRMSTR2: ! 
00310   read #trmstr2,using 'Form POS 1,N 2,N 1,C 8,G 6,PD 10.2,C 8,C 35,N 1,N 6,N 1',release: bank_code,tcde,tr$(1),tr$(2),tr3,tr$(4),tr$(5),pcde,clr,scd eof READ_PAYMSTR1 : let tr$(3)=str$(tr3)
00320   if vn$><tr$(4) then goto PRINT_IT !:
          ! moved thru all checks without finding a check with date !:
          ! later than one entered above
00330   let lastdate=val(tr$(2)) conv READ_TRMSTR2
00340   if fndate_mmddyy_to_ccyymmdd(lastdate)>olddate then goto READ_PAYMSTR1 !:
          ! keep this vendor recored
00350   goto READ_TRMSTR2
00360 ! ______________________________________________________________________
00370 PRINT_IT: ! 
00380   pr #255,using "Form POS 1,C 8,X 3,C 30": vn$,nam$ pageoflow NEWPGE
00390   delete #paymstr1,key=vn$: nokey L410
00400   gosub REMOVE_FROM_PAYEEGLBREAKDOWN
00410 L410: goto READ_PAYMSTR1
00420 ! ______________________________________________________________________
00430 REMOVE_FROM_PAYEEGLBREAKDOWN: ! uses VN$
00440   restore #payeeglbreakdown,key>=vn$: nokey OUTTA_PGB_LOOP
00450 READ_PAYEEGLBREAKDOWN: ! 
00460   read #payeeglbreakdown,using 'Form Pos 1,C 8': readvn$ eof OUTTA_PGB_LOOP
00470   if readvn$=vn$ then !:
          delete #payeeglbreakdown: !:
          goto READ_PAYEEGLBREAKDOWN else !:
          goto OUTTA_PGB_LOOP
00480 OUTTA_PGB_LOOP: ! 
00490   return 
00500 ! ______________________________________________________________________
00510 DONE: ! 
00520   fncloseprn !:
        goto XIT
00530 ! ______________________________________________________________________
00540 XIT: let fnxit
00550 ! ______________________________________________________________________
00560 NEWPGE: pr #255: newpage : gosub HDR : continue 
00570 ! ______________________________________________________________________
00580 HDR: ! 
00590   pr #255,using 'Form POS 1,Cc 80': cnam$ !:
        pr #255,using 'Form POS 1,Cc 80': cap$ !:
        pr #255,using 'Form POS 1,Cc 80': dat$
00600   return 
00610 ! ______________________________________________________________________
00620 ! <Updateable Region: ERTN>
00630 ERTN: let fnerror(program$,err,line,act$,"NOt")
00640   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00650   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00660   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00670 ERTN_EXEC_ACT: execute act$ : goto ERTN
00680 ! /region
00690 ! ______________________________________________________________________
