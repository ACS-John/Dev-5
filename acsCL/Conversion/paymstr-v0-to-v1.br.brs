! Replace S:\acsCL\Conversion\Paymstr-v0-to-v1
! converts the CL TRmstr file from version 1 to Version 2
def library fnpaymstr_v0_to_v1
		autoLibrary
		on error goto Ertn
		dim message$*40,msgline$(6)*48,response$(5)*1
		dim gl$*12,gl$(10)*12,percent(10),de$*30,desc$(10)*30
		dim cap$*128
		cap$="Checkbook update Payees from v0 to v1"

		fnStatus('updating payee file.')
		open #paymstr=fnH: "Name=[Q]\CLmstr\Paymstr.h[cno]",internal,outIn,relative
		if version(paymstr)=1 then
			msgline$(4)="Paymstr is already version 1"
			msgline$(5)="press enter to continue" : msgline$(6)=""
			fnmsgbox(mat msgline$,response$,cap$,1)
			goto Xit
		end if
		close #paymstr:
		fnCopy("[Q]\CLmstr\Paymstr.h[cno]","[Q]\CLmstr\Paymstr.h[cno]",736)
		fnIndex("[Q]\CLmstr\PayMstr.h[cno]","[Q]\CLmstr\PayIdx2.h[cno]","9 30")
		open #paymstr1=fnH: "Name=[Q]\CLmstr\PayMstr.h[cno],KFName=[Q]\CLmstr\PayIdx1.h[cno]",internal,outIn,keyed
		open #paymstr2=fnH: "Name=[Q]\CLmstr\PayMstr.h[cno],KFName=[Q]\CLmstr\PayIdx2.h[cno]",internal,outIn,keyed
		version(paymstr1,1)
		open #payalloc=fnH: "Name=[Q]\CLmstr\PayAlloc.h[cno]",internal,input,relative ioerr EO_PAYALLOC
		open #payeegl=fnH: "Name=[Q]\CLmstr\payeeglbreakdown.h[cno],KFName=[Q]\CLmstr\Payeeglbkdidx.h[cno],Use,RecL=56,KPs=1,KLn=8,Shr",internal,outIn,keyed
		version(payeegl,1)
	READ_PAYALLOC: !
		read #payalloc,using 'Form POS 1,C 8,C 12,PD 3.2,C 30': vn$,gl$,pct,de$ eof EO_PAYALLOC
		write #payeegl,using 'Form POS 1,C 8,C 12,n 6.2,C 30': vn$,gl$,pct,de$
	goto READ_PAYALLOC

	EO_PAYALLOC: !
		version(paymstr1,1)
		close #paymstr1: ioerr ignore
		close #paymstr2: ioerr ignore
		close #payeegl: ioerr ignore
		close #payalloc,free: ioerr ignore
		fnIndex('[Q]\CLmstr\PayMstr.h[cno]','[Q]\CLmstr\Paydx1.h[cno]','1 8')
		fnIndex('[Q]\CLmstr\PayMstr.h[cno]','[Q]\CLmstr\PayIdx2.h[cno]','9 30')
		fnIndex('[Q]\CLmstr\payeeglbreakdown.h[cno]','[Q]\CLmstr\payeeglbkdidx.h[cno]','1 8')
	goto Xit

	Xit: !
fnend

include: ertn No