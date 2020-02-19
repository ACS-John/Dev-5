00010 ! Replace S:\acsGL\CoverLetterPrint
00020 ! -- pr Cover Letter
00030 !
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnerror,fncno,fndat,fnprocess,fnpedat$,fnactpd$,fnconsole
00050   on error goto Ertn
00060 !
00070   dim tb$*32,cap$*128,ln1$*8800,ln$*8800,dat$*20
00080 !
00090   fntop("S:\acsGL\CoverLetterPrint",cap$="Print Cover Leter")
00100   fnconsole(off=0)
00110   fncno(cno)
00120   fndat(dat$)
00130   open #1: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,input,relative  !:
        read #1,using 'Form POS 195,C 30',rec=1: tb$ !:
        close #1: !:
        tb$="("&trim$(tb$)&")"
00140   tempx=val(fnactpd$) conv L180
00150   if tempx=1 then actpd$="one" else !:
          if tempx=2 then actpd$="two" else !:
            if tempx=3 then actpd$="three" else !:
              if tempx=4 then actpd$="four" else !:
                if tempx=5 then actpd$="five"
00160   if tempx=6 then actpd$="six" else !:
          if tempx=7 then actpd$="seven" else !:
            if tempx=8 then actpd$="eight" else !:
              if tempx=9 then actpd$="nine" else !:
                if tempx=10 then actpd$="ten"
00170   if tempx=11 then actpd$="eleven" else !:
          if tempx=12 then actpd$="twelve" else !:
            if tempx=13 then actpd$="thirteen" else !:
              if tempx=14 then actpd$="fourteen"
00180 L180: open #1: "Name=[Q]\GLmstr\ACGLCovF.h[cno],Shr",display,input ioerr XIT
00200   on fkey 5 goto DONE
00210   fnopenprn
00220 READ_ACGLCOVF: ! 
00230   linput #1: ln$ eof DONE ioerr DONE
00240   for j2=1 to len(rtrm$(ln$))
00250     if ln$(j2:j2)><"@" then goto L320
00260     if ln$(j2+1:j2+1)="1" then !:
            ln$(j2:j2+1)=fnpedat$&ln$(j2+2:132-len(fnpedat$)) !:
          else goto L280
00270     goto L310
00280 L280: if ln$(j2+1:j2+1)="2" then !:
            ln$(j2:j2+1)=rtrm$(dat$)&ln$(j2+2:132-len(rtrm$(dat$))) !:
          else goto L300
00290     goto L310
00300 L300: if ln$(j2+1:j2+1)="3" then !:
            ln$(j2:j2+1)=rtrm$(actpd$)&ln$(j2+2:132-len(rtrm$(actpd$))) else goto L320
00310 L310: ! lN$=LN1$
00320 L320: next j2
00330   pr #255: tab(10);ln$
00340   goto READ_ACGLCOVF
00350 !
00360 DONE: close #1: 
00370   fncloseprn
00380   goto XIT
00390 !
00400 XIT: fnxit
00410 !
00420 ! <Updateable Region: ERTN>
00430 ERTN: fnerror(program$,err,line,act$,"xit")
00440   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00450   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00460   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00470 ERTN_EXEC_ACT: execute act$ : goto ERTN
00480 ! /region
00490 !
