00010 ! Replace S:\acsCL\Conversion\TrAlloc-v1-to-v2
00020 ! converts the CL TrAlloc file from version 0 or version 1 to Version 2
00030   def library fntralloc_v1_to_v2
00040     library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnmsgbox,fnwait,fnchain,fnCopy,fnindex_it,fnStatus
00050     on error goto Ertn
00060 !
00070     dim cnam$*40,cap$*128,message$*40,msgline$(6)*48,response$(5)*1
00080 !
00090     fncno(cno,cnam$)
00100     cap$="Checkbook update Trans from v1 to v2"
00110 !
00120     fnStatus("Updating Checkbook Transaction Allocation from v1 to v2")
00180 ! fnwait(message$="Converting: please wait...",0)
00190     fnindex_it("[Q]\CLmstr\TrAlloc.H[cno]","[Q]\CLmstr\TrAlloc-Idx.H[cno]","1 11")
00200     open #tralloc=1: "Name=[Q]\CLmstr\TrAlloc.h[cno],KFName=[Q]\CLmstr\TrAlloc-Idx.h[cno]",internal,outIn,keyed 
00210     close #tralloc: 
00220     open #tralloc: "Name=[Q]\CLmstr\TrAlloc.h[cno],KFName=[Q]\CLmstr\TrAlloc-Idx.h[cno]",internal,outIn,keyed 
00240     if version(tralloc)=2 then let fnStatus("TrAlloc is already version 2") : goto XIT
00250     version(tralloc,2)
00260     goto XIT
00270 !
00280 ! <Updateable Region: ERTN>
00290 ERTN: fnerror(program$,err,line,act$,"NO")
00300     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00310     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00320     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00330 ERTN_EXEC_ACT: execute act$ : goto ERTN
00340 ! /region
00350 !
00360 CANCEL: execute "System"
00370 !
00380 XIT: ! 
00390     close #tralloc: 
00400     fnCopy("[Q]\CLmstr\TrAlloc.h[cno]","[Q]\CLmstr\TrAlloc.h[cno]",80)
00430     fnindex_it("[Q]\CLmstr\TrAlloc.H[cno]","[Q]\CLmstr\TrAlloc-Idx.H[cno]","1 11")
00440   fnend 
00450 !
