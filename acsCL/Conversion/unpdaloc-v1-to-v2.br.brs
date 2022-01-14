! Replace S:\acsCL\Conversion\UnPdAloc-v1-to-v2
! converts the CL UnPdAloc file from version 0 or version 1 to Version 2
def library fnunpdaloc_v1_to_v2
		autoLibrary
		on error goto Ertn
 
		dim cnam$*40,cap$*128,message$*40,msgline$(6)*48,response$(5)*1
 
		fncno(cno,cnam$)
		cap$="Checkbook update UnPdAloc from v1 to v2"
		fnStatus("Payment Allocation file until it is updating from v1 to v2")
		open #unpdaloc=1: "Name=[Q]\CLmstr\UnPdAloc.h[cno],KFName=[Q]\CLmstr\UAIdx1.h[cno]",internal,outIn,keyed
!      close #unpdaloc:
		if version(unpdaloc)=2 then let fnStatus("UnPdAloc is already version 2") : goto Xit
		version(unpdaloc,2)
		close #unpdaloc:
 
! change the record length
		fnCopy("[Q]\CLmstr\UnPdAloc.h[cno]","[Q]\CLmstr\UnPdAloc.h[cno]",67)
 
! make sure the Key is right justified
		open #unpdaloc=1: "Name=[Q]\CLmstr\UnPdAloc.h[cno],KFName=[Q]\CLmstr\UAIdx1.h[cno]",internal,outIn,keyed
		for j=1 to lrec(unpdaloc)
			read #unpdaloc,using 'form pos 1,C 8,c 12',rec=j: vn$,iv$ noRec L330
			vn$=lpad$(rtrm$(vn$),8): iv$=lpad$(rtrm$(iv$),12)
			rewrite #unpdaloc,using 'form pos 1,Cr 8,c 12',rec=j: vn$,iv$
L330: next j
		close #unpdaloc:
		fnIndex("[Q]\CLmstr\UnPdAloc.h[cno]","[Q]\CLmstr\UAIdx1.h[cno]","9 12")
		fnIndex("[Q]\CLmstr\UnPdAloc.h[cno]","[Q]\CLmstr\UAIdx2.h[cno]","1 20")
		goto Xit
 
include: ertn
 
Xit: fnend
 
