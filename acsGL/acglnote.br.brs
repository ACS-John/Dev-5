00010 ! Replace S:\acsGL\AcGlNote
00020 ! -- Foot Notes
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnerror,fncno,fndat,fnprocess,fnactpd$,fnpedat$,fntos,fncomboa,fncmdkey,fnacs,fnget_wordprocessor_exe
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim tb$*32,fl2$(2),sc2$(2)*46,ln$*8000,cnam$*40,dat$*20,cap$*128
00080   dim option$(2)*42,resp$(1)*50,atlantis$*80
00090 ! ______________________________________________________________________
00100   fntop(program$,cap$="Financial Statements Notes")
00110   fncno(cno,cnam$) !:
        fndat(dat$)
00120   pr newpage
00130   tempx=val(fnactpd$) conv L170
00140   if tempx=1 then actpd$="one" else !:
          if tempx=2 then actpd$="two" else !:
            if tempx=3 then actpd$="three" else !:
              if tempx=4 then actpd$="four" else !:
                if tempx=5 then actpd$="five"
00150   if tempx=6 then actpd$="six" else !:
          if tempx=7 then actpd$="seven" else !:
            if tempx=8 then actpd$="eight" else !:
              if tempx=9 then actpd$="nine" else !:
                if tempx=10 then actpd$="ten"
00160   if tempx=11 then actpd$="eleven" else !:
          if tempx=12 then actpd$="twelve" else !:
            if tempx=13 then actpd$="thirteen" else !:
              if tempx=14 then actpd$="fourteen"
00170 L170: open #1: "Name="&env$('Q')&"\GLmstr\Company.h"&str$(cno)&",Shr",internal,input  !:
        read #1,using 'Form POS 195,C 30': tb$ !:
        close #1: 
00180   tb$="("&trim$(tb$)&")"
00190   if fnprocess=1 then t=2 : goto L290
00200 MENU1: pr newpage
00210   fntos(sn$="acglnote") !:
        mylen=20: mypos=mylen+3 : right=1
00220   option$(1)="1 = Edit Notes to Financial Statements" !:
        option$(2)="2 = pr Notes"
00230   fncomboa("NoteOption",1,mypos,mat option$,"You can edit or pr notes to the financial statements ",40)
00240   fncmdkey("&Next",1,1,0,"Allows you to enter information.")
00250   fncmdkey("&Cancel",5,0,1,"Return to menu.")
00260   fnacs(sn$,0,mat resp$,ckey)
00270   if ckey=5 then goto XIT
00280   if resp$(1)=option$(1) then t=1 else t=2
00290 L290: on t goto L300,L320 none MENU1
00300 L300: ! 
00301   fnget_wordprocessor_exe(atlantis$) !:
        execute 'SY -w '&atlantis$&' '&env$('Q')&"\GLmstr\ACGLNote.h"&str$(cno)&" -n"
00310   goto MENU1
00320 L320: pr newpage
00330   open #1: "Name="&env$('Q')&"\GLmstr\AcGLNote.h"&str$(cno)&",Shr",display,input ioerr MENU1
00340   pr newpage
00350   pr f "10,20,Cc 25,N": "Foot Notes Printing..." !:
        pr f "12,2,C 11,B,5": "Cancel (F5)" !:
        on fkey 5 goto L460 !:
        fnopenprn
00360 ! 
00370 L370: linput #1: ln$ eof L460
00380   for j2=1 to len(rtrm$(ln$))
00390     if ln$(j2:j2)><"@" then goto L430
00400     if ln$(j2+1:j2+1)="1" then !:
            ln$(j2:j2+1)=rtrm$(fnpedat$)&ln$(j2+2:78-len(rtrm$(fnpedat$))) !:
            goto L420
00410     if ln$(j2+1:j2+1)="2" then !:
            ln$(j2:j2+1)=rtrm$(dat$)&ln$(j2+2:78-len(rtrm$(dat$))) !:
          else if ln$(j2+1:j2+1)="3" then !:
            ln$(j2:j2+1)=rtrm$(actpd$)&ln$(j2+2:78-len(rtrm$(fnactpd$)))
00420 L420: ! 
00430 L430: next j2
00440 pr #255: tab(10);ln$
00450 goto L370
00460 L460: close #1: 
00470 fncloseprn
00480 on fkey 5 ignore 
00490 if fnprocess=1 then goto XIT else goto MENU1
00500 goto XIT
00510 ! ______________________________________________________________________
00520 XIT: fnxit
00530 ! ______________________________________________________________________
00540 ! <updateable region: ertn>
00550 ERTN: fnerror(program$,err,line,act$,"xit")
00560 if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00570 if trim$(env$("ACSDeveloper"))<>"" then !:
        execute "list -"&str$(line) : pause : goto ERTN_EXEC_ACT
00580 pr "program pause: type go and press [enter] to continue." !:
      pr "" : pause : goto ERTN_EXEC_ACT
00590 ERTN_EXEC_ACT: execute act$ : goto ERTN
00600 ! /region
00610 ! ______________________________________________________________________
80070 dim tb$*32,fl2$(2),sc2$(2)*46,ln1$*8000,ln$*8000,cnam$*40,dat$*20,cap$*128
