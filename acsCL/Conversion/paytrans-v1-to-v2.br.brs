! Replace S:\acsCL\Conversion\PayTrans-v1-to-v2
! converts the CL PayTrans file from version 0 or version 1 to Version 2
def library fnpaytrans_v1_to_v2
		autoLibrary
		on error goto Ertn
 
		dim cnam$*40,cap$*128,message$*40,msgline$(6)*48,response$(5)*1
 
		fncno(cno,cnam$)
		cap$="Checkbook update PayTrans from v1 to v2"
 
		fnStatus('updating Unpaid Invoice file')
! fnwait(message$="Converting: please wait...",0)
 
		open #paytrans1=1: "Name=[Q]\CLmstr\PayTrans.h[cno],KFName=[Q]\CLmstr\UnPdIdx1.h[cno]",internal,outIn,keyed
		if version(paytrans1)=2 then : _
			msgline$(4)="PayTrans is already version 2" : _
			msgline$(5)="press enter to continue" : msgline$(6)="" : _
			fnmsgbox(mat msgline$,response$,cap$,1) : _
			close #paytrans1: : _
			goto Xit
		close #paytrans1:
 
! change the record length of the file
		fnCopy("[Q]\CLmstr\PayTrans.h[cno]","[Q]\CLmstr\PayTrans.h[cno]",114)
		open #paytrans1=1: "Name=[Q]\CLmstr\PayTrans.h[cno],KFName=[Q]\CLmstr\UnPdIdx1.h[cno]",internal,outIn,keyed
		for j=1 to lrec(paytrans1)
			read #paytrans1,using 'Form Pos 96,N 1,N 6': gde,pdte eof L320
			rewrite #paytrans1,using 'Form Pos 90,N 1,N 6,N 10.2,N 8': gde,pdte,disamt=0,ddate=0
		next j
L320: version(paytrans1,2)
		close #paytrans1:
		fnindex_it("[Q]\CLmstr\PayTrans.H[cno]","[Q]\CLmstr\UnPdIdx1.H[cno]","1 20")
		fnindex_it("[Q]\CLmstr\PayTrans.h[cno]","[Q]\CLmstr\UnPdIdx2.h[cno]","31/27/1 2/4/26")
		goto Xit
 
include: Ertn
 
Xit: !
fnend
 
