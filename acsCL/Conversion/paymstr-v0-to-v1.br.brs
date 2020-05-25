! Replace S:\acsCL\Conversion\Paymstr-v0-to-v1
! converts the CL TRmstr file from version 1 to Version 2
def library fnpaymstr_v0_to_v1
		autoLibrary
		on error goto Ertn
 
		dim cnam$*40,cap$*128,message$*40,msgline$(6)*48,response$(5)*1
		dim gl$*12,gl$(10)*12,percent(10),de$*30,desc$(10)*30
 
		fncno(cno,cnam$)
		cap$="Checkbook update Payees from v0 to v1"
 
		fnStatus('updating payee file.')
		open #paymstr:=fngethandle: "Name=[Q]\CLmstr\Paymstr.h[cno]",internal,outIn,relative
		if version(paymstr)=1 then : _
			msgline$(4)="Paymstr is already version 1" : _
			msgline$(5)="press enter to continue" : msgline$(6)="" : _
			fnmsgbox(mat msgline$,response$,cap$,1) : _
			goto Xit
		close #paymstr:
		fnCopy("[Q]\CLmstr\Paymstr.h[cno]","[Q]\CLmstr\Paymstr.h[cno]",736)
		fnindex_it("[Q]\CLmstr\PayMstr.h[cno]","[Q]\CLmstr\PayIdx2.h[cno]","9 30")
		open #paymstr1:=fngethandle: "Name=[Q]\CLmstr\PayMstr.h[cno],KFName=[Q]\CLmstr\PayIdx1.h[cno]",internal,outIn,keyed
		open #paymstr2:=fngethandle: "Name=[Q]\CLmstr\PayMstr.h[cno],KFName=[Q]\CLmstr\PayIdx2.h[cno]",internal,outIn,keyed
		version(paymstr1,1)
		open #payalloc:=fngethandle: "Name=[Q]\CLmstr\PayAlloc.h[cno]",internal,input,relative ioerr EO_PAYALLOC
		open #payeegl:=fngethandle: "Name=[Q]\CLmstr\payeeglbreakdown.h[cno],KFName=[Q]\CLmstr\Payeeglbkdidx.h[cno],Use,RecL=56,KPs=1,KLn=8,Shr",internal,outIn,keyed
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
		execute "Index [Q]\CLmstr\PayMstr.H[cno]"&' '&"[Q]\CLmstr\Paydx1.H[cno] 1 8 Replace DupKeys -n" : _
		execute "Index [Q]\CLmstr\PayMstr.H[cno]"&' '&"[Q]\CLmstr\PayIdx2.H[cno] 9 30 Replace DupKeys -n" : _
		execute "Index [Q]\CLmstr\payeeglbreakdown.h[cno]"&' '&"[Q]\CLmstr\payeeglbkdidx.H[cno] 1 8 Replace DupKeys -n"
		goto Xit
 
! <Updateable Region: ERTN>
ERTN: fnerror(program$,err,line,act$,"NO")
		if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
		execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
		pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
ERTN_EXEC_ACT: execute act$ : goto ERTN
! /region
 
Xit: !
fnend
 
