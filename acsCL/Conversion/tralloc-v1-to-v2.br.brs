00010 ! Replace R:\acsCL\Conversion\TrAlloc-v1-to-v2
00020 ! converts the CL TrAlloc file from version 0 or version 1 to Version 2
00030   def library fntralloc_v1_to_v2
00040     library 'R:\Core\Library': fntop,fnxit, fncno,fnerror,fnmsgbox,fnwait,fnchain,fncopy,fnindex_it,fnstatus
00050     on error goto ERTN
00060 ! ______________________________________________________________________
00070     dim cnam$*40,cap$*128,message$*40,msgline$(6)*48,response$(5)*1
00080 ! ______________________________________________________________________
00090     let fncno(cno,cnam$)
00100     let cap$="Check Book update Trans from v1 to v2"
00110 ! ______________________________________________________________________
00120     let fnstatus("Updating Check Book Transaction Allocation from v1 to v2")
00180 ! let fnwait(101,cap$,message$="Converting: please wait...",0)
00190     let fnindex_it("Q:\CLmstr\TrAlloc.H"&str$(cno),"Q:\CLmstr\TrAlloc-Idx.H"&str$(cno),"1 11")
00200     open #tralloc=1: "Name=Q:\CLmstr\TrAlloc.h"&str$(cno)&",KFName=Q:\CLmstr\TrAlloc-Idx.h"&str$(cno),internal,outin,keyed 
00210     close #tralloc: 
00220     open #tralloc: "Name=Q:\CLmstr\TrAlloc.h"&str$(cno)&",KFName=Q:\CLmstr\TrAlloc-Idx.h"&str$(cno),internal,outin,keyed 
00240     if version(tralloc)=2 then let fnstatus("TrAlloc is already version 2") : goto XIT
00250     let version(tralloc,2)
00260     goto XIT
00270 ! ______________________________________________________________________
00280 ! <Updateable Region: ERTN>
00290 ERTN: let fnerror(cap$,err,line,act$,"NO")
00300     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00310     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00320     print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00330 ERTN_EXEC_ACT: execute act$ : goto ERTN
00340 ! /region
00350 ! ______________________________________________________________________
00360 CANCEL: let fnchain('R:\Core\Exit')
00370 ! ______________________________________________________________________
00380 XIT: ! 
00390     close #tralloc: 
00400     let fncopy("Q:\CLmstr\TrAlloc.h"&str$(cno),"Q:\CLmstr\TrAlloc.h"&str$(cno),80)
00430     let fnindex_it("Q:\CLmstr\TrAlloc.H"&str$(cno),"Q:\CLmstr\TrAlloc-Idx.H"&str$(cno),"1 11")
00440   fnend 
00450 ! ______________________________________________________________________
