! Replace S:\acsCL\InitGLPay
autoLibrary
on error goto Ertn
dim item1$(2)*45,resp$(10)*25,ml$(3)*70,de$*50 
fnTop(program$,"Import GL Payee Records")
MENU1: !
	fnTos
	mylen=45 : mypos=mylen+2 : lc=0
	fnLbl(lc+=1,1,"Extract Payee Information from general ledger:",45,1)
	item1$(1)="ACS G/L system"
	item1$(2)="Accountant's Diskette"
	fncomboa("claims-srt",lc,mypos,mat item1$,tt$) 
	resp$(1)=item1$(1)
	fnLbl(lc+=1,1,"General Ledger Company Number:",mylen,1)
	fnTxt(lc,mypos,5,0,0,'30') 
	resp$(2)=env$('cno')
	fnCmdSet(2) 
	fnAcs2(mat resp$,ckey)
	if ckey=5 then goto Xit else : _
		if resp$(1)=item1$(1) then pas$="BUILD" else : _
			if resp$(1)=item1$(2) then pas$="COPY"
	glcno=val(resp$(2))
	execute "COPY A:paymstr.H"&str$(glcno)&' '&"[Q]\CLmstr\*.*" ioerr MSGBOX2
	execute "COPY A:payeeglbreakdown.H"&str$(glcno)&' '&"[Q]\CLmstr\*.*" ioerr MSGBOX2
	execute "Index [Q]\CLmstr\paymstr.H"&str$(glcno)&' '&"[Q]\CLmstr\payidx1.H"&str$(glcno)&",1,8,replace,DupKeys"
	execute "Index [Q]\CLmstr\payeeglbreakdown.H"&str$(glcno)&' '&"[Q]\CLmstr\Payeeglbkdidx.H"&str$(glcno)&",1,8,replace,DupKeys"
	open #paymstr:=fngethandle: "Name=[Q]\CLmstr\PayMstr.h[cno],KFName=[Q]\CLmstr\PayIdx1.h[cno],Shr",internal,outIn,keyed
	open #payeegl:=fngethandle: "Name=[Q]\CLmstr\PayeeGLBreakdown.h[cno],KFName=[Q]\CLmstr\Payeeglbkdidx.h[cno],Use,RecL=56,KPs=1,KLn=8,Shr",internal,outIn,keyed
	version(payeegl,1)
	version(paymstr,1)
	close #paymstr:
	close #payeegl:
Xit: fnXit
 
include: Ertn
MSGBOX1: !
	mat ml$(2) : _
	ml$(1)="A general ledger chart of accounts has not been set up" : _
	ml$(2)="for this company.  You must choose a different option" : _
	fnmsgbox(mat ml$,resp$,'',16) : _
	goto MENU1
MSGBOX2: !
	mat ml$(1) : _
	ml$(1)="Be sure the diskette is properly inserted and try again" : _
	fnmsgbox(mat ml$,resp$,'',16) : _
	goto MENU1
