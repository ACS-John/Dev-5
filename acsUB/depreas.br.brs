00010 ! Replace S:\acsUB\DepReas
00020 ! -- Reassign Deposit Change Addresses
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnmsgbox,fnwait,fnerror,fnxit,fntop
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim msgline$(3)*60,cap$*128,ta(2),message$*40
00080   fncno(cno)
00090 ! 
00100   fntop(program$,cap$="Reassign Deposit Change Addresses")
00110 ! ______________________________________________________________________
00120 MAIN: ! 
00130   msgline$(1)="No other users may be using the Deposit file" !:
        msgline$(2)="while this option is running.  Do you want to run" !:
        msgline$(3)="Reassign Deposit Change Addresses now?" !:
        fnmsgbox(mat msgline$,resp$,cap$,49)
00140   if uprc$(resp$)=uprc$("CANCEL") then goto XIT
00150 ! ______________________________________________________________________
00160   open #1: "Name="&env$('Q')&"\UBmstr\Deposit1.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\DepIdx1.h"&env$('cno'),internal,outin,keyed ioerr MAIN
00170   open #2: 'Name='&env$('Q')&'\UBmstr\Deposit2.h'&env$('cno')&',KFName='&env$('Q')&'\UBmstr\Deposit2Index.h'&env$('cno')&',Shr,Use,RecL=73,KPs=1,KLn=10',internal,outin,keyed ! "Name="&env$('Q')&"\UBmstr\Deposit2.h"&env$('cno'),internal,outin,relative ioerr MAIN
00180 ! ______________________________________________________________________
00190 TOP: ! 
00200   read #1,using "Form POS 11,2*PD 3": mat ta eof L240
00210   rewrite #1,using "Form POS 11,2*PD 3": 0,0
00220   goto TOP
00230 ! ______________________________________________________________________
00240 L240: lr2=lrec(2)
00250   if lr2=0 then goto XIT
00260   rewrite #2,using "Form POS 71,PD 3",rec=1: lr2
00270   for j=1 to lr2
00280     read #2,using "Form POS 1,C 10,POS 71,PD 3",rec=j: k$,nta norec L350
00290     read #1,using "Form POS 11,2*PD 3",key=k$: mat ta nokey L350
00300     if ta(1)=0 then ta(1)=j
00310     if ta(2)>0 then !:
            rewrite #2,using "Form POS 71,PD 3",rec=ta(2): j
00320     ta(2)=j
00330     rewrite #1,using "Form POS 11,2*PD 3",key=k$: mat ta
00340     rewrite #2,using "Form POS 71,PD 3",rec=j: 0
00350 L350: next j
00360   goto XIT
00370 ! ______________________________________________________________________
00380 XIT: fnxit
00390 ! ______________________________________________________________________
00400 ! <Updateable Region: ERTN>
00410 ERTN: fnerror(program$,err,line,act$,"xit")
00420   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00430   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00440   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00450 ERTN_EXEC_ACT: execute act$ : goto ERTN
00460 ! /region
00470 ! ______________________________________________________________________
