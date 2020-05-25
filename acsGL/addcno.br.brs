! Replace S:\acsGL\AddCNo
! was GLCopy - but the functionality needed to be in addFRO_C_N_O, which there wasn't one of so I just renamed it to that for the time being and we need to fix and test addCno later.
 
	autoLibrary
	fnTop(program$,cap$="Add Company")
	on error goto Ertn
 
	dim zer(57),resp$(10)*80
! ___________________________
	right=1
	fncno(to_cno)
! ___________________________
MENU1: !
	fnTos(sn$="GLAddCNo") : _
	mylen=37 : mypos=mylen+2
	fnLbl(1,1,"Copy Chart of Accounts from Company:",mylen,right)
	fncmbcno(1,mypos)
	fnCmdSet(2)
	fnAcs2(mat resp$,ckey)
	if ckey=5 then fro_cno=99999: goto L210 ! use company #99999 if no company to copy from
	fro_cno=val(resp$(1)(43:47))
	if fro_cno=0 then fro_cno=99999
L210: if to_cno<1 or to_cno=fro_cno then goto MENU1
! ___________________________
	fnCopy("[Q]\GLmstr\*.h"&str$(fro_cno),"[Q]\GLmstr\*.h"&str$(to_cno))
	open #20: "Name=[Q]\GLmstr\GLmstr.h"&str$(to_cno)&",KFName=[Q]\GLmstr\GLIndex.h"&str$(to_cno)&",NoShr",internal,outIn,keyed
L250: read #20,using 'Form POS 87,PD 6.2': cb eof L280
	rewrite #20,using 'Form POS 81,42*PD 6.2,POS 333,2*PD 3,13*pd 6.2': mat zer
	goto L250
L280: close #20:
! ___________________________
	execute 'drop "'&"[Q]GLmstr\GLTrans.H"&str$(to_cno)&'"' err ignore
	fnFree("[Q]\GLmstr\ACTrans.h"&str$(to_cno))
	open #1: "Name=[Q]\GLmstr\ACTrans.h"&str$(to_cno)&",Size=0,RecL=72,NoShr",internal,output
	close #1:
Xit: fnchain("S:\acsGL\Company")
IGNORE: continue
include: Ertn
