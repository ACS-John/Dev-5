! Replace S:\Core\Parse\Remove2.br
! removes a big hunk instead of 1 character.
! only does it once instead of all instances.
! removes any character (and$) from any sting (word$)
def library fnremove2(&and$,&word$)
	and$=trim$(and$)
	x=pos(word$,and$,1)
	if x=1 then word$=word$(x+len(and$):len(word$))
	if x>1 then word$=word$(1:x-1)&word$(x+len(and$):len(word$))
fnend
 
