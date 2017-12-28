00010 ! Replace S:\acsCL\Conversion\Paymstr-v0-to-v1
00020 ! converts the CL TRmstr file from version 1 to Version 2
00030   def library fnpaymstr_v0_to_v1
00040     library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnmsgbox,fnwait,fngethandle,fnCopy,fnStatus,fnindex_it
00050     on error goto ERTN
00060 ! ______________________________________________________________________
00070     dim cnam$*40,cap$*128,message$*40,msgline$(6)*48,response$(5)*1
00080     dim gl$*12,gl$(10)*12,percent(10),de$*30,desc$(10)*30
00090 ! ______________________________________________________________________
00100     fncno(cno,cnam$)
00110     cap$="Checkbook update Payees from v0 to v1"
00120 ! ______________________________________________________________________
00130     fnStatus('updating payee file.')
00180     open #paymstr:=fngethandle: "Name="&env$('Q')&"\CLmstr\Paymstr.h"&env$('cno'),internal,outIn,relative 
00190     if version(paymstr)=1 then !:
            msgline$(4)="Paymstr is already version 1" !:
            msgline$(5)="press enter to continue" : msgline$(6)="" !:
            fnmsgbox(mat msgline$,response$,cap$,1) !:
            goto XIT
00200     close #paymstr: 
00210     fnCopy(env$('Q')&"\CLmstr\Paymstr.h"&env$('cno'),env$('Q')&"\CLmstr\Paymstr.h"&env$('cno'),736)
00232     fnindex_it(env$('Q')&"\CLmstr\PayMstr.h"&env$('cno'),env$('Q')&"\CLmstr\PayIdx2.h"&env$('cno'),"9 30")
00240     open #paymstr1:=fngethandle: "Name="&env$('Q')&"\CLmstr\PayMstr.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\PayIdx1.h"&env$('cno'),internal,outIn,keyed 
00250     open #paymstr2:=fngethandle: "Name="&env$('Q')&"\CLmstr\PayMstr.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\PayIdx2.h"&env$('cno'),internal,outIn,keyed 
00261     version(paymstr1,1)
00262     open #payalloc:=fngethandle: "Name="&env$('Q')&"\CLmstr\PayAlloc.h"&env$('cno'),internal,input,relative ioerr EO_PAYALLOC
00263     open #payeegl:=fngethandle: "Name="&env$('Q')&"\CLmstr\payeeglbreakdown.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\Payeeglbkdidx.h"&env$('cno')&",Use,RecL=56,KPs=1,KLn=8,Shr",internal,outIn,keyed 
00264     version(payeegl,1)
00270 READ_PAYALLOC: ! 
00280     read #payalloc,using 'Form POS 1,C 8,C 12,PD 3.2,C 30': vn$,gl$,pct,de$ eof EO_PAYALLOC
00290     write #payeegl,using 'Form POS 1,C 8,C 12,n 6.2,C 30': vn$,gl$,pct,de$
00370     goto READ_PAYALLOC
00380 ! ______________________________________________________________________
00390 EO_PAYALLOC: ! 
00400     version(paymstr1,1)
00410     close #paymstr1: ioerr ignore
00420     close #paymstr2: ioerr ignore
00425     close #payeegl: ioerr ignore
00430     close #payalloc,free: ioerr ignore
00440     execute "Index "&env$('Q')&"\CLmstr\PayMstr.H"&env$('cno')&' '&env$('Q')&"\CLmstr\Paydx1.H"&env$('cno')&" 1 8 Replace DupKeys -n" !:
          execute "Index "&env$('Q')&"\CLmstr\PayMstr.H"&env$('cno')&' '&env$('Q')&"\CLmstr\PayIdx2.H"&env$('cno')&" 9 30 Replace DupKeys -n" !:
          execute "Index "&env$('Q')&"\CLmstr\payeeglbreakdown.h"&env$('cno')&' '&env$('Q')&"\CLmstr\payeeglbkdidx.H"&env$('cno')&" 1 8 Replace DupKeys -n"
00450     goto XIT
00460 ! ______________________________________________________________________
00470 ! <Updateable Region: ERTN>
00480 ERTN: fnerror(program$,err,line,act$,"NO")
00490     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00500     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00510     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00520 ERTN_EXEC_ACT: execute act$ : goto ERTN
00530 ! /region
00540 ! ______________________________________________________________________
00550 XIT: ! 
00560   fnend 
00570 ! ______________________________________________________________________
