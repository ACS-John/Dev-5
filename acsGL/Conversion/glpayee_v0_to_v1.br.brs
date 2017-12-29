00010 ! Replace S:\acsGL\Conversion\glpayee_v0_to_v1
00020 ! converts general ledger payee files to new rln
00022 ! from recl=127 to recl=276 and version 1
00030   def library fnglpayee_v0_to_v1
00040     library 'S:\Core\Library': fntop,fnxit, fnerror,fnmsgbox,fngethandle,fnStatus,fnindex_it,fncopy
00050     on error goto ERTN
00060 ! ______________________________________________________________________
00070     dim cap$*128
00080     dim nam$*35,ad1$*20,ad2$*20,csz$*20,ss$*11,ph$*12,contact$*30,email$*50
00090     dim fax$*12,myact$*20
00150 ! 
00160 ! ______________________________________________________________________
00171     fnStatus('updating Payee file format.')
00210     open #1: "Name="&env$('Q')&"\GLmstr\GL1099.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\GL109IDX.h"&env$('cno'),internal,outIn,keyed ioerr ignore
00215     if exists(env$('Q')&"\GLmstr\paymstr.h"&env$('cno'))=0 then open #2: "Name="&env$('Q')&"\GLmstr\paymstr.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\payidx1.h"&env$('cno')&",RecL=276,kps=1,kln=8,replace",internal,outIn,keyed: version(2,1): close #2: 
00220     open #2: "Name="&env$('Q')&"\GLmstr\paymstr.h"&env$('cno'),internal,outIn,relative  ! open #2: "Name="&env$('Q')&"\GLmstr\paymstr.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\payidx1.h"&env$('cno'),internal,outIn,keyed
00221     if rln(2)<>276 then 
00222       close #2: 
00223       fnCopy(env$('Q')&"\GLmstr\paymstr.h"&env$('cno'),env$('temp')&"\WORK."&session$,276)
00224       fnCopy(env$('temp')&"\WORK."&session$,env$('Q')&"\GLmstr\paymstr.h"&env$('cno'))
00225       open #2: "Name="&env$('Q')&"\GLmstr\paymstr.h"&env$('cno'),internal,outIn,relative  ! open #2: "Name="&env$('Q')&"\GLmstr\paymstr.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\payidx1.h"&env$('cno'),internal,outIn,keyed
00226     end if 
00227     do 
00230       read #1,using "form pos 1,c 8,c 35,3*c 20,pd 5.2,n 2,c 11": vn$,nam$,ad1$,ad2$,csz$,ytdp,typ,ss$ eof EOD_GL1099 ioerr EOD_GL1099
00240       write #2,using 'Form Pos 1,C 8,4*c 30,x 5,n 2,c 11,x 6,c 12,c 30,c 50,c 12,c 20',release: vn$,nam$(1:30),ad1$,ad2$,csz$,typ,ss$,ph$,contact$,email$,fax$,myact$
00250     loop 
00252 EOD_GL1099: ! 
00260     version(2,1)
00270     close #1,free: ioerr ignore
00280     close #2: ioerr ignore
00290     fnindex_it(env$('Q')&"\GLmstr\paymstr.h"&env$('cno'),env$('Q')&"\GLmstr\payidx1.h"&env$('cno'),"1 8")
00300     fnindex_it(env$('Q')&"\GLmstr\paymstr.h"&env$('cno'),env$('Q')&"\GLmstr\payidx2.h"&env$('cno'),"9 38")
00310     fnindex_it(env$('Q')&"\GLmstr\payeeglbreakdown.h"&env$('cno'),env$('Q')&"\GLmstr\payeeglbkdidx.h"&env$('cno'),"1 8")
00314     if ~exists(env$('Q')&"\GLmstr\gltr1099.h"&env$('cno')) then let fnCopy("S:\General Ledger\mstr\gltr1099.h99999",env$('Q')&"\GLmstr\gltr1099.h"&env$('cno'))
00316     fnindex_it(env$('Q')&"\GLmstr\gltr1099.h"&env$('cno'),env$('Q')&"\GLmstr\gltridx.h"&env$('cno'),"1 8")
00320     goto XIT
00330 ! ______________________________________________________________________
00340 ! <Updateable Region: ERTN>
00350 ERTN: fnerror(program$,err,line,act$,"xit")
00360     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00370     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00380     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00390 ERTN_EXEC_ACT: execute act$ : goto ERTN
00400 ! /region
00410 ! ______________________________________________________________________
00420 XIT: fnend 
00422 IGNORE: continue 
00430 ! ______________________________________________________________________
