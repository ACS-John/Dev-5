00010 ! Replace S:\acsUB\Conversion\Note-Cnv-c7-Ashgrove
00020 ! Note1 and Note2 conversion program !:
        ! converts them up to version 1 with account length of 10
00030 ! ______________________________________________________________________
00040   dim rm$*60,ra(2),newra(2)
00045   library 'S:\Core\Library': fnfree
00050 ! ______________________________________________________________________
00060   pr newpage
00080 ! 
00090 ! ** phase 1 **
00100   pr " *** Phase 1 ***" !:
        pr "convert [Q]\UBmstr\Note1.h[cno] to version 1"
00110   open #note1=1: "Name=[Q]\UBmstr\Note1.h[cno],KFName=[Q]\UBmstr\NoteIdx1.h[cno]",internal,outIn,keyed 
00120   if version(note1)=>1 then !:
          close #note1: !:
          pr "[Q]\UBmstr\Note1.h[cno] is already at least version 1" !:
          goto EOPHASE1
00130   open #work=2: "Name="&env$('Temp')&"\Work."&session$&",Replace,RecL=16",internal,output 
00140 READ_NOTE1_PHASE1: ! 
00150   read #note1,using 'Form POS 1,C 7,2*PD 3': z$,a1,a2 eof EO1
00160   write #work,using 'Form POS 1,C 7,C 3,2*PD 3': z$,".00",a1,a2
00170   goto READ_NOTE1_PHASE1
00180 ! ______________________________________________________________________
00190 EO1: close #note1: 
00200   close #work: 
00210   fnFree("[Q]\UBmstr\Note1.h[cno]")
00220   execute "Rename "&env$('Temp')&"\Work."&session$&' '&"[Q]\UBmstr\Note1.h[cno] -n"
00230   execute "Index [Q]\UBmstr\Note1.h[cno],[Q]\UBmstr\NoteIdx1.h[cno],1,10,Replace,DupKeys -n"
00240   execute "Copy [Q]\UBmstr\Note2.h[cno] X -73 -n"
00250   execute "Copy X [Q]\UBmstr\Note2.h[cno] -n"
00260   execute "Free X -n"
00270   open #note1=1: "Name=[Q]\UBmstr\Note1.h[cno],KFName=[Q]\UBmstr\NoteIdx1.h[cno]",internal,outIn,keyed 
00280   version(note1,1)
00290   close #note1: 
00300   pr "[Q]\UBmstr\Note1.h[cno] converted successfully to version 1."
00310   goto EOPHASE1
00320 ! ______________________________________________________________________
00330 EOPHASE1: ! 
00340   pr " *** Phase 2 ***" !:
        pr "convert [Q]\UBmstr\Note2.h[cno] to version 1"
00350   open #note1=3: "Name=[Q]\UBmstr\Note1.h[cno],KFName=[Q]\UBmstr\NoteIdx1.h[cno]",internal,outIn,keyed 
00360   open #note2=4: "Name=[Q]\UBmstr\Note2.h[cno]",internal,outIn,relative 
00370   if version(note2)=>1 then close #note2: !:
          pr "[Q]\UBmstr\Note2.h[cno] is already at least version 1" !:
          goto EOPHASE2
00380   pr "Initial Record Len of "&file$(note2)&" is "&str$(rln(note2))&"."
00400 READ_NOTE1_PHASE2: ! 
00410   read #note1,using 'Form POS 1,C 10,2*PD 3': rk$,mat ra eof EO3
00420   r32=ra(1)
00430 READ_NOTE2_PHASE2: ! 
00440   if r32<1 then goto READ_NOTE1_PHASE2
00450   read #note2,using 'Form POS 1,C 07,C 60,PD 3',rec=r32: k32$,rm$,n32 conv READ_NOTE1_PHASE2
00460   rewrite #note2,using 'Form POS 1,C 10,C 60,PD 3',rec=r32: rk$,rm$,n32
00470   r32=n32
00480   goto READ_NOTE2_PHASE2
00490 ! ______________________________________________________________________
00500 EO3: ! 
00510   version(note1,1)
00520   version(note2,1)
00530   close #note1: 
00540   close #note2: 
00550   pr "[Q]\UBmstr\Note2.h[cno] converted successfully to version 1"
00560   goto EOPHASE2
00570 ! ______________________________________________________________________
00580 EOPHASE2: ! 
00590 ! ** Phase 3 **
50020 ! Note conversion program
50070   open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno]",internal,outIn,keyed 
50080   open #3: "Name=[Q]\UBmstr\Note1.h[cno],KFName=[Q]\UBmstr\NoteIdx1.h[cno]",internal,outIn,keyed 
50090   open #4: "Name=[Q]\UBmstr\Note2.h[cno]",internal,outIn,relative 
50100 L50100: read #1,using 'Form POS 1,C 10': z$ eof EO4
50120   if z$(8:10)=".00" then goto L50100 ! skip base records
50130   x$=z$(1:7)&".00"
50140   mat newra=(0)
50150   read #3,using 'Form POS 1,C 10,2*PD 3',key=x$: rk$,mat ra nokey L50100
50160   write #3,using 'Form POS 1,C 10,2*PD 3': z$,0,0
50180   r32=ra(1)
50190 L50190: if r32<1 then goto L50100
50200   read #4,using 'Form POS 1,C 10,C 60,PD 3',rec=r32: rk$,rm$,r32
50210   n32=lrec(4)+1
50220   if r32>0 then ntr=n32+1 else ntr=0
50230   write #4,using 'Form POS 1,C 10,C 60,PD 3',rec=n32: z$,rm$,ntr
50240   if newra(1)=0 then newra(1)=n32
50250   newra(2)=n32
50260   rewrite #3,using 'form pos 11,2*pd 3',key=z$: mat newra
50270   goto L50190
50280 EO4: close #3: 
50290   close #4: 
50300 XIT: chain "S:\acsUB\conversion\note-cnv"
50310 ! ______________________________________________________________________
