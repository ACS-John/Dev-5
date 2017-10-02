00010 ! Replace S:\acsGL\acglAuto
00020 ! automatic processing - chain program
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnxit, fnxit,fncno,fnerror,fnputcno,fnprocess,fnprg,fnchain,fnps,fnpgnum,fnkillauto,fnmsgbox
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim prg$*35,a$*40,cnam$*40,ml$(3)*80
00080 ! ______________________________________________________________________
00090   let fncno(cno)
00100 L100: if fnprocess=0 then goto XIT
00110 ! ______________________________________________________________________
00120   let pgnum=fnpgnum
00130   open #20: "Name="&env$('Q')&"\GLmstr\ACGLPGMN.h"&str$(cno)&",Shr",internal,input,relative ioerr MSGBOX1 !:
        read #20,using 'Form POS 1,C 35,POS 71,N 3,x 1,2*N 1',rec=pgnum+=1: prg$,pn,ps,srq eof XIT,norec XIT !:
        close #20: 
00140   if rtrm$(prg$)="" then goto L220
00150   let fnprg(prg$,put=2)
00160   let fnpgnum(pgnum) : let fnps(ps)
00170   goto CHAIN_PRG
00180 MSGBOX1: ! 
00190   mat ml$(3) !:
        let ml$(1)="The order for automatic processing has" !:
        let ml$(2)="never been set for company # "&str$(cno)&"." !:
        let ml$(3)="Click OK to skip this company." !:
        let fnmsgbox(mat ml$,resp$,cap$,49)
00200   goto L220
00210 ! ______________________________________________________________________
00220 L220: let fnkillauto : let fnpgnum(-1) !:
        ! ! CHECK FOR ADDITIONAL COMPANIES
00230   open #glclnt=1: "Name="&env$('Q')&"\GLmstr\glClnt.dat,NoShr",internal,outin,relative ioerr XIT
00240   for j=2 to 20
00250     read #glclnt,using 'Form POS 1,N 5',rec=j: cno
00260     if cno<>0 then goto L300
00270   next j
00280   goto XIT
00290 ! ______________________________________________________________________
00300 L300: let fnputcno(cno) : let fnprocess(process=1)
00310   rewrite #glclnt,using 'Form POS 1,N 5,C 40',rec=j: 0," "
00320   close #glclnt: 
00330   goto L100
00340 ! ______________________________________________________________________
00350 XIT: ! 
00360   execute "Free AutoPrn."&wsid$&" -n" ioerr L370
00370 L370: let fnxit
00380 CHAIN_PRG: let fnchain(prg$)
00390 ! ______________________________________________________________________
00400 ! <Updateable Region: ERTN>
00410 ERTN: let fnerror(program$,err,line,act$,"xit")
00420   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00430   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00440   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00450 ERTN_EXEC_ACT: execute act$ : goto ERTN
00460 ! /region
