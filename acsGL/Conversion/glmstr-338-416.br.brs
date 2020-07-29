! Replace S:\acsGL\Conversion\GLmstr-338-416
! convert [Q]\GLmstr from RECL 338 to RECL 416 Format
def library fnglmstr_338_416
	autoLibrary
	fnStatus('Converting GLmstr from 338 to 416...')
	fnCopy("[Q]\GLmstr\GLmstr.h[cno]","[Q]\GLmstr\GLmstr.h[cno]",416) ! &" -416 -n"
	open #1: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno]",internal,outIn,keyed
	do
		dim revb(13)
		read #1,using 'Form POS 339,13*PD 6.2': mat revb eof DONE
		mat revb=(0)
		rewrite #1,using 'Form POS 339,13*PD 6.2': mat revb
	loop
	DONE: !
	close #1:
	fnIndex("[Q]\GLmstr\GLmstr.h[cno]","[Q]\GLmstr\glIndex.h[cno]","1 12")
	fnacglblds
fnend
