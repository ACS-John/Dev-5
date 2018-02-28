00010 ! Replace S:\acsGL\AcGlCovl
00020 ! -- Edit/Print Cover Letter
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnxit,fntop, fnopenprn,fncloseprn,fnerror,fncno,fndat,fnprocess,fnpedat$,fnactpd$,fnchain
00050   fntop(program$,cap$="Cover Leter")
00060   on error goto ERTN
00070 ! ______________________________________________________________________
00080   dim tb$*32,cap$*128,p$(20)*50
00090   dim ln1$*78,ln$*78,shd$*60,fli$(20),cnam$*40,dat$*20,fl2$(2),sc2$(2)*46
00100 ! ______________________________________________________________________
00120   fncno(cno,cnam$)
00130   fndat(dat$)
00140   open #1: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,input,relative  !:
        read #1,using 'Form POS 195,C 30',rec=1: tb$ !:
        close #1: !:
        tb$="("&trim$(tb$)&")"
00150   tempx=val(fnactpd$) conv L190
00160   if tempx=1 then actpd$="one" else !:
          if tempx=2 then actpd$="two" else !:
            if tempx=3 then actpd$="three" else !:
              if tempx=4 then actpd$="four" else !:
                if tempx=5 then actpd$="five"
00170   if tempx=6 then actpd$="six" else !:
          if tempx=7 then actpd$="seven" else !:
            if tempx=8 then actpd$="eight" else !:
              if tempx=9 then actpd$="nine" else !:
                if tempx=10 then actpd$="ten"
00180   if tempx=11 then actpd$="eleven" else !:
          if tempx=12 then actpd$="twelve" else !:
            if tempx=13 then actpd$="thirteen" else !:
              if tempx=14 then actpd$="fourteen"
00190 L190: sh$="1,10,C 60,H,N"
00200   for j=1 to 20 : fli$(j)=str$(j+2)&",2,C 78,UT,N" : next j
00210   if fnprocess=1 then t=2 : goto L320 else goto MENU1
00220 ! _____________________________________________________________________
00230 MENU1: pr newpage
00240   close #101: ioerr L250
00250 L250: open #101: "SROW=3,SCOL=13,EROW=9,ECOL=63,BORDER=DR,CAPTION=<Cover Letter",display,outIn 
00260   pr f "3,13,Cc 51,R,N": cnam$ !:
        pr f "4,13,Cc 51,R,N": "Company Number [cno]"
00270   sc2$(1)=" 1. Edit Cover Letter" !:
        sc2$(2)=" 2. pr Cover Letter"
00280   for j=1 to 2: fl2$(j)=str$(j+5)&",15,C 46": next j
00290   pr f "10,35,Cc 09,B,5": "Exit (F5)"
00300 L300: rinput select mat fl2$,attr "H": mat sc2$ !:
        t=curfld
00310   if cmdkey=5 then goto XIT
00320 L320: on t goto L370,L390 none L300
00330 ! _____________________________________________________________________
00340   close #101: ioerr L350
00350 L350: open #101: "SROW=5,SCOL=13,EROW=15,ECOL=64,BORDER=SR,CAPTION=<Initial Build Cover Letter",display,outIn 
00360   pr #101,fields "1,1,Cc 52,R,N": cnam$
00370 L370: execute "SY -s NotePad "&os_filename$("[Q]\GLmstr\ACGLCovF.h[cno]")
00380   goto MENU1
00390 L390: open #1: "Name=[Q]\GLmstr\ACGLCovF.h[cno],Shr",display,input ioerr MENU1
00400   pr newpage !:
        pr f "10,20,Cc 25,H,N": "Cover Letter Printing..." !:
        pr f "12,2,C 18,B,5": " Press F5 to stop"
00410   on fkey 5 goto L550
00420   fnopenprn
00430 L430: linput #1: ln$ eof L550
00440   for j2=1 to len(rtrm$(ln$))
00450     if ln$(j2:j2)><"@" then goto L520
00460     if ln$(j2+1:j2+1)="1" then !:
            ln1$=ln$(1:j2-1)&fnpedat$&ln$(j2+2:78-len(fnpedat$)) !:
          else goto L480
00470     goto L510
00480 L480: if ln$(j2+1:j2+1)="2" then !:
            ln1$=ln$(1:j2-1)&rtrm$(dat$)&ln$(j2+2:78-len(rtrm$(dat$))) !:
          else goto L500
00490     goto L510
00500 L500: if ln$(j2+1:j2+1)="3" then !:
            ln1$=ln$(1:j2-1)&rtrm$(actpd$)&ln$(j2+2:78-len(rtrm$(actpd$)))
00510 L510: ln$=ln1$
00520 L520: next j2
00530   pr #255: tab(10);ln$
00540   goto L430
00550 L550: close #1: 
00555   pr newpage
00560   fncloseprn
00570 XIT: fnchain("S:\acsGL\acglAuto")
00580 ! ______________________________________________________________________
00590 ! <Updateable Region: ERTN>
00600 ERTN: fnerror(program$,err,line,act$,"xit")
00610   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00620   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00630   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00640 ERTN_EXEC_ACT: execute act$ : goto ERTN
00650 ! /region
