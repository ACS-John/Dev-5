00010 ! Replace S:\acsGL\acglRest
00020 ! pr Retained Earnings Statement
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnwin3,fnopenprn,fncloseprn,fncno,fndat,fnerror,fnprocess,fnpedat$,fnglfs
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim ln1$*78,ln$*78,shd$*60,fli$(10),cnam$*40,fli1$(2),hdr$*78,foot$*78
00080   dim sc2$(2),dat$*20,cap$*128
00090 ! ______________________________________________________________________
00100   let fntop(program$,cap$="Retained Earnings Statement")
00110   let fncno(cno,cnam$) !:
        let fndat(dat$)
00120   let sh$="2,10,C 60,H,N"
00130   for j=1 to 10 : let fli$(j)=str$(j+2)&",2,C 78,UT,N" : next j
00140   let fli1$(1)="5,2,C 78,UT,N" : let fli1$(2)="8,2,C 78,UT,N"
00150   if fnprocess=1 then let t=2 : goto L240
00160 ! ______________________________________________________________________
00170 MENU1: ! 
00180   let fnwin3(win=101,cap$,6,40,1,1,5)
00190   let sc2$(1)="1. Edit" : let sc2$(2)="2. Print"
00200   for j=1 to 2 : let fl2$(j)=str$(j+3)&",02,C 08,N" : next j
00210   rinput #win,select mat fl2$,attr "H": mat sc2$ !:
        let t=curfld
00220   close #win: 
00230   if cmdkey=5 or cmdkey=99 then goto XIT
00240 L240: let j=0
00250   on t goto EDIT,L300 none MENU1
00260 ! ______________________________________________________________________
00270 EDIT: ! 
00280   execute 'SY NotePad "'&os_filename$(env$('Q')&"\GLmstr\ACGLSTMT.h"&str$(cno))&'"'
00290   goto MENU1
00300 L300: ! ______________________________________________________________________
00310   if fnglfs=5 then goto MENU1
00320   let fnopenprn
00330   pr newpage
00340   open #1: "Name="&env$('Q')&"\GLmstr\AcGLStmt.h"&str$(cno)&",Shr",display,input ioerr EDIT
00350   pr newpage !:
        pr fields "10,20,Cc 30,H,N": "R/E Statement Printing..." !:
        pr fields "12,34,C 11,B,5": "Cancel (F5)" !:
        on fkey 5 goto L480
00360 L360: linput #1: ln$ eof L480
00370   for j2=1 to len(rtrm$(ln$))
00380     if ln$(j2:j2)><"@" then goto L450
00390     if ln$(j2+1:j2+1)="1" then !:
            let ln1$=ln$(1:j2-1)&rtrm$(fnpedat$)&ln$(j2+2:78-len(rtrm$(fnpedat$))) !:
          else goto L410
00400     goto L440
00410 L410: if ln$(j2+1:j2+1)="2" then !:
            let ln1$=ln$(1:j2-1)&rtrm$(dat$)&ln$(j2+2:78-len(rtrm$(dat$))) !:
          else goto L430
00420     goto L440
00430 L430: if ln$(j2+1:j2+1)="3" then !:
            let ln1$=ln$(1:j2-1)&rtrm$(actpd$)&ln$(j2+2:78-len(rtrm$(actpd$)))
00440 L440: let ln$=ln1$
00450 L450: next j2
00460   pr #255: tab(10);ln$
00470   goto L360
00480 L480: close #1: 
00490   let fncloseprn
00500   on fkey 5 ignore 
00510   if fnprocess=1 then goto XIT else goto MENU1
00520 ! ______________________________________________________________________
00530 ! <Updateable Region: ERTN>
00540 ERTN: let fnerror(program$,err,line,act$,"xit")
00550   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00560   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00570   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00580 ERTN_EXEC_ACT: execute act$ : goto ERTN
00590 ! /region
00600 ! ______________________________________________________________________
00610 XIT: let fnxit
00620 ! ______________________________________________________________________
