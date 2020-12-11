! Replace S:\acsCL\Conversion\TRmstr-v1-to-v2
! converts the CL TRmstr file from version 1 to Version 2
def library fntrmstr_v1_to_v2
		autoLibrary
		on error goto Ertn
 
		dim cap$*128,message$*40,msgline$(6)*48,response$(5)*1
 
		cap$="Checkbook update Trans from v1 to v2"
 
		fnStatus(cap$)
 
		open #trmstr:=fnH: "Name=[Q]\CLmstr\TRmstr.h[cno]",internal,outIn,relative
		if version(trmstr)<>1 and version(trmstr)<>2 then fnStatus("TRmstr is not version 1.  You must update it to version 1 before running this conversion program") : goto Xit
		if version(trmstr)=2 then fnStatus("TRmstr is already version 2") : goto Xit
		version(trmstr,2)
	goto Xit
 

	Xit: !
		if file$(trmstr)>'' then close #trmstr:
		fnCopy("[Q]\CLmstr\TRmstr.h[cno]","[Q]\CLmstr\TRmstr.h[cno]",78)
		fnIndex("[Q]\CLmstr\TrMstr.h[cno]","[Q]\CLmstr\TrIdx1.h[cno]","1 11")
		fnIndex("[Q]\CLmstr\TrMstr.h[cno]","[Q]\CLmstr\TrIdx2.h[cno]","28/1 8/11")
fnend
 
include: ertn No