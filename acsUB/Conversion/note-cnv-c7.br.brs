20000 ! Replace S:\acsUB\Conversion\Note-Cnv-c7
20020 ! Note1 and Note2 conversion program - converts them up to version 1 with account length of 10 (adds .00)
20040 def library fnub_cnv_note_phase_1
20060   dim rm$*60,ra(2),newra(2)
20080   library 'S:\Core\Library': fncno,fnstatus,fnindex_it,fnCopy
20100   let fncno(cno)
20120 ! 
20140 ! r: ** phase 1 **
20160   open #note1=1: "Name="&env$('Q')&"\UBmstr\Note1.h"&str$(cno),internal,outin,relative 
20180   if version(note1)=>1 then 
20200     close #note1: 
20220     fnstatus(env$('Q')&"\UBmstr\Note1.h"&str$(cno)&" is already at least version 1")
20240     goto EOPHASE1
20260   else
20280     fnstatus("converting "&env$('Q')&"\UBmstr\Note1.h"&str$(cno)&" to version 1")
20300   end if
20320   open #work=2: "Name="&env$('temp')&"\Work."&session$&",Replace,RecL=16",internal,output 
20340 READ_NOTE1_PHASE1: ! 
20360   read #note1,using 'Form POS 1,C 7,2*PD 3': z$,a1,a2 eof EO1
20380   write #work,using 'Form POS 1,C 7,C 3,2*PD 3': z$,".00",a1,a2
20400   goto READ_NOTE1_PHASE1
20420 ! ______________________________________________________________________
30000 EO1: ! 
30020   close #note1: 
30040   close #work: 
30060   execute "Free "&env$('Q')&"\UBmstr\Note1.h"&str$(cno)&" -n"
30080   execute "Rename "&env$('temp')&"\Work."&session$&' '&env$('Q')&"\UBmstr\Note1.h"&str$(cno)&" -n"
30100   fnCopy(env$('Q')&"\UBmstr\Note2.h"&str$(cno),env$('Q')&"\UBmstr\Note2.h"&str$(cno),73) 
30140   fnindex_it(env$('Q')&"\UBmstr\Note1.h"&str$(cno),env$('Q')&"\UBmstr\NoteIdx1.h"&str$(cno),"1 10")
30160   open #note1=1: "Name="&env$('Q')&"\UBmstr\Note1.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\NoteIdx1.h"&str$(cno),internal,outin,keyed 
30180   let version(note1,1)
30200   close #note1: 
30220   fnstatus(env$('Q')&"\UBmstr\Note1.h"&str$(cno)&" converted successfully to version 1.")
30240   goto EOPHASE1
30260 ! ______________________________________________________________________
30280 EOPHASE1: ! /r
40000 ! r: *** Phase 2 ***" 
40020   open #note1=3: "Name="&env$('Q')&"\UBmstr\Note1.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\NoteIdx1.h"&str$(cno),internal,outin,keyed 
40040   open #note2=4: "Name="&env$('Q')&"\UBmstr\Note2.h"&str$(cno),internal,outin,relative 
40060   if version(note2)=>1 then 
40080     close #note2: 
40100     fnstatus(env$('Q')&"\UBmstr\Note2.h"&str$(cno)&" is already at least version 1")
40120     goto EOPHASE2
40140   else
40160     fnstatus("converting "&env$('Q')&"\UBmstr\Note2.h"&str$(cno)&" to version 1")
40180   end if
40200   fnstatus("Initial Record Len of "&file$(note2)&" is "&str$(rln(note2))&".")
40220 READ_NOTE1_PHASE2: ! 
40240   read #note1,using 'Form POS 1,C 10,2*PD 3': rk$,mat ra eof EO3
40260   let r32=ra(1)
40280 READ_NOTE2_PHASE2: ! 
40300   if r32<1 then goto READ_NOTE1_PHASE2
40320   read #note2,using 'Form POS 1,C 07,C 60,PD 3',rec=r32: k32$,rm$,n32 conv READ_NOTE1_PHASE2
40340   rewrite #note2,using 'Form POS 1,C 10,C 60,PD 3',rec=r32: rk$,rm$,n32
40360   let r32=n32
40380   goto READ_NOTE2_PHASE2
40400 ! ______________________________________________________________________
50000 EO3: ! /r
50020   let version(note1,1)
50040   let version(note2,1)
50060   close #note1: 
50080   close #note2: 
50100   fnstatus(env$('Q')&"\UBmstr\Note2.h"&str$(cno)&" converted successfully to version 1")
50120   goto EOPHASE2
50140 ! ______________________________________________________________________
60000 EOPHASE2: ! 
60020 ! ** Phase 3 **
60040 ! Note conversion program
60060   if exists(env$('Q')&"\UBmstr\Customer.h"&str$(cno)) then open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno),internal,input,relative else if exists(env$('Q')&"\UBmstr\ubMaster.h"&str$(cno)) then open #1: "Name="&env$('Q')&"\UBmstr\ubMaster.h"&str$(cno),internal,input,relative 
60080 close #3: ioerr ignore
60120 fnindex_it(env$('Q')&"\UBmstr\Note1.h"&str$(cno),env$('Q')&"\UBmstr\NoteIdx1.h"&str$(cno),"1 10")
60140 open #3: "Name="&env$('Q')&"\UBmstr\Note1.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\NoteIdx1.h"&str$(cno),internal,outin,keyed 
60160 open #4: "Name="&env$('Q')&"\UBmstr\Note2.h"&str$(cno),internal,outin,relative 
60180 L50100: !
60200 read #1,using 'Form POS 1,C 10': z$ eof EO4
60220 if z$(8:10)=".00" then goto L50100 ! skip base records
60240 let x$=z$(1:7)&".00"
60260 mat newra=(0)
60280 read #3,using 'Form POS 1,C 10,2*PD 3',key=x$: rk$,mat ra nokey L50100
60300 write #3,using 'Form POS 1,C 10,2*PD 3': z$,0,0
60320 let r32=ra(1)
60340 do
60360   if r32<1 then goto L50100
60380   read #4,using 'Form POS 1,C 10,C 60,PD 3',rec=r32: rk$,rm$,r32
60400   let n32=lrec(4)+1
60420   if r32>0 then let ntr=n32+1 else let ntr=0
60440   write #4,using 'Form POS 1,C 10,C 60,PD 3',rec=n32: z$,rm$,ntr
60460   if newra(1)=0 then let newra(1)=n32
60480   let newra(2)=n32
60500   rewrite #3,using 'form pos 11,2*pd 3',key=z$: mat newra
60520 loop
70000 EO4: !
70020 close #1: 
70040 close #3: 
70060 close #4: 
70080 XIT: ! 
70100 fnend 