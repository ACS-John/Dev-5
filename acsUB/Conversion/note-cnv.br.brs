20000 ! Replace S:\acsUB\conversion\Note-cnv
20020 ! this program converts the footnotes to new format
20040   def library fnub_cnv_note
20060     library 'S:\Core\Library': fnerror,fncno,fnstatus,fnCopy,fnub_cnv_note_phase_1,fngethandle,fnindex_it
20080     fnub_cnv_note_phase_1 ! this needs to be done first in case they are on version 1 on note1 and note2 
20100     let fnstatus('Converting Notes (S:\acsUB\conversion\Note-cnv)')
20120     dim nam$*30,rm$*60,ra(2),z$*10
20140 ! ______________________________________________________________________
20160     let fncno(cno)
20180 ! 
20200     if exists(env$('Q')&"\UBmstr\Note1.h"&str$(cno)) then
20220       fnindex_it(env$('Q')&"\UBmstr\Note1.h"&str$(cno), env$('Q')&"\UBmstr\NoteIdx1.h"&str$(cno),"1 10")
20240       open #h_note1:=fngethandle: "Name="&env$('Q')&"\UBmstr\Note1.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\NoteIdx1.h"&str$(cno)&",Shr",internal,input,keyed 
20260 !   fnCopy(env$('Q')&"\UBmstr\Note2.h"&str$(cno),env$('Q')&"\UBmstr\Note2.h"&str$(cno),73)  ! BAD IDEA - THIS PROGRAM DOES NOT HANDLE THAT FORMAT OF NOTE2.hxx file - I think S:\acsUB\Conversion\note-cnv-c7.br will convert it to the format necessary for this to continue though
20280     
20300     open #h_note2:=fngethandle: "Name="&env$('Q')&"\UBmstr\Note2.h"&str$(cno)&",Shr",internal,outin,relative
20320     if kln(h_note2)<73 then
20340       close #h_note2:
20360       fnCopy(env$('Q')&"\UBmstr\Note2.h"&str$(cno),env$('Q')&"\UBmstr\Note2.h"&str$(cno), 73)
20380       open #h_note2:=fngethandle: "Name="&env$('Q')&"\UBmstr\Note2.h"&str$(cno)&",Shr,Use,RecL=73",internal,outin,relative 
20400     end if
20420
20440 READ_NOTE1: ! 
20460     read #h_note1,using 'form pos 1,c 10,2*pd 3': z$,mat ra eof DONE
20480 L230: ! 
20500     close #33: ioerr ignore
20520     if ~exists(env$('Q')&"\UBmstr\notes.h"&str$(cno)) then execute "mkdir "&env$('Q')&"\UBmstr\notes.h"&str$(cno)
20540     open #33: "Name="&env$('Q')&"\UBmstr\notes.h"&str$(cno)&"\"&trim$(z$)&".txt,RecL=128,replace",display,output 
20560     let adr=ra(1)
20580     do 
20600       if adr=0 then goto READ_NOTE1
20620       read #h_note2,using 'form pos 1,c 10,c 60,pd 3',rec=adr: k32$,rm$,adr norec READ_NOTE1
20640       print #33: rm$
20660     loop
20680     ! 
20700     DONE: ! 
20720     let fnstatus('Note Conversion of company number '&str$(cno)&" completed successfully")
20740   end if
20760 ! Goto 110
20780 XIT: fnend  ! chain "S:\acsUB\conversion\ubadrbil-cnv"
20800 ! ______________________________________________________________________
20820 ! <updateable region: ertn>
20840 ERTN: let fnerror(program$,err,line,act$,"xit")
20860   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
20880   if uprc$(act$)="PAUSE" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT ! if env$("ACSDeveloper")<>"" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
20900   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
20920 ERTN_EXEC_ACT: execute act$ : goto ERTN
20940 ! </updateable region: ertn>
