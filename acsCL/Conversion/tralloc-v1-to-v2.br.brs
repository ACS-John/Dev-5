00010 ! Replace S:\acsCL\Conversion\TrAlloc-v1-to-v2
00020 ! converts the CL TrAlloc file from version 0 or version 1 to Version 2
00030   def library fntralloc_v1_to_v2
00040     library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnmsgbox,fnwait,fnchain,fnCopy,fnindex_it,fnstatus
00050     on error goto ERTN
00060 ! ______________________________________________________________________
00070     dim cnam$*40,cap$*128,message$*40,msgline$(6)*48,response$(5)*1
00080 ! ______________________________________________________________________
00090     fncno(cno,cnam$)
00100     cap$="Checkbook update Trans from v1 to v2"
00110 ! ______________________________________________________________________
00120     fnstatus("Updating Checkbook Transaction Allocation from v1 to v2")
00180 ! fnwait(101,cap$,message$="Converting: please wait...",0)
00190     fnindex_it(env$('Q')&"\CLmstr\TrAlloc.H"&str$(cno),env$('Q')&"\CLmstr\TrAlloc-Idx.H"&str$(cno),"1 11")
00200     open #tralloc=1: "Name="&env$('Q')&"\CLmstr\TrAlloc.h"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\TrAlloc-Idx.h"&str$(cno),internal,outin,keyed 
00210     close #tralloc: 
00220     open #tralloc: "Name="&env$('Q')&"\CLmstr\TrAlloc.h"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\TrAlloc-Idx.h"&str$(cno),internal,outin,keyed 
00240     if version(tralloc)=2 then let fnstatus("TrAlloc is already version 2") : goto XIT
00250     version(tralloc,2)
00260     goto XIT
00270 ! ______________________________________________________________________
00280 ! <Updateable Region: ERTN>
00290 ERTN: fnerror(program$,err,line,act$,"NO")
00300     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00310     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00320     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00330 ERTN_EXEC_ACT: execute act$ : goto ERTN
00340 ! /region
00350 ! ______________________________________________________________________
00360 CANCEL: fnchain('S:\Core\Exit')
00370 ! ______________________________________________________________________
00380 XIT: ! 
00390     close #tralloc: 
00400     fnCopy(env$('Q')&"\CLmstr\TrAlloc.h"&str$(cno),env$('Q')&"\CLmstr\TrAlloc.h"&str$(cno),80)
00430     fnindex_it(env$('Q')&"\CLmstr\TrAlloc.H"&str$(cno),env$('Q')&"\CLmstr\TrAlloc-Idx.H"&str$(cno),"1 11")
00440   fnend 
00450 ! ______________________________________________________________________
