! Replace S:\acsCL\Conversion\TrAlloc-v1-to-v2
! converts the CL TrAlloc file from version 0 or version 1 to Version 2
def library fntralloc_v1_to_v2
		autoLibrary
		on error goto Ertn

		dim cnam$*40,cap$*128,message$*40,msgline$(6)*48,response$(5)*1

		fncno(cno,cnam$)
		cap$="Checkbook update Trans from v1 to v2"

		fnStatus("Updating Checkbook Transaction Allocation from v1 to v2")
		! fnwait(message$="Converting: please wait...",0)
		fnIndex("[Q]\CLmstr\TrAlloc.H[cno]","[Q]\CLmstr\TrAlloc-Idx.H[cno]","1 11")
		open #tralloc=1: "Name=[Q]\CLmstr\TrAlloc.h[cno],KFName=[Q]\CLmstr\TrAlloc-Idx.h[cno]",internal,outIn,keyed
		close #tralloc:
		open #tralloc: "Name=[Q]\CLmstr\TrAlloc.h[cno],KFName=[Q]\CLmstr\TrAlloc-Idx.h[cno]",internal,outIn,keyed
		if version(tralloc)=2 then fnStatus("TrAlloc is already version 2") : goto Xit
		version(tralloc,2)
	goto Xit


	CANCEL: execute "System"
	Xit: !
		close #tralloc:
		fnCopy("[Q]\CLmstr\TrAlloc.h[cno]","[Q]\CLmstr\TrAlloc.h[cno]",80)
		fnIndex("[Q]\CLmstr\TrAlloc.H[cno]","[Q]\CLmstr\TrAlloc-Idx.H[cno]","1 11")
fnend
include: ertn No
