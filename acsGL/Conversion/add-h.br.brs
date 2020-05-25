! Replace S:\acsGL\Conversion\add-h.wb
! this program renames [Q]\GLmstr\*.[cno] to [Q]\GLmstr\*.H[cno]
 
	autoLibrary
	fnTop(program$)
	on error goto Ertn
 
	fncno(cno)
	pr newpage
MENU1: !
	close #101: ioerr L120
L120: open #101: "SROW=9,SCOL=23,EROW=11,ECOL=60,BORDER=DR,CAPTION="&cap$,display,outIn: pr #101: newpage
	pr f "10,24,C 32": "Company Number:"
	pr f "12,32,C 16,B,5": "Cancel (F5)"
L150: rinput fields "10,57,N 5,UT,N": cno conv L150
	if cmdkey=5 then goto Xit
	goto START
 
Xit: stop
 
START: !
	execute "Rename [Q]\GLmstr\*.[cno]"&' '&"[Q]\GLmstr\*.h[cno] -n" ioerr RER
	pr 'company [cno] completed.'
	goto MENU1
 
RER: !
	pr 'company [cno] had problems.'
	goto MENU1
 
include: Ertn
