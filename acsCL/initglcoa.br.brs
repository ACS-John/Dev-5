! Replace S:\acsCL\InitGLCoA
! Import General Ledger Chart of Accounts
 
	autoLibrary
	on error goto Ertn
 
	dim cap$*128,item1$(2)*45,resp$(10)*25,ml$(3)*70,de$*50
 
	fnTop(program$,cap$="Import GL Chart of Accounts")
	cancel=99 : right=1 : left=0 : center=2 : number$='30'
L120: open #1: "Name=[Q]\CLmstr\GLmstr.H[cno],KFName=[Q]\CLmstr\GLINDEX.H[cno],Shr",internal,outIn,keyed
MENU1: !
	fnTos(sn$="InitGLCoA") : _
	mylen=38 : mypos=mylen+2 : lc=0
	fnLbl(lc+=1,1,"Extract general ledger accounts from:",38,right)
	item1$(1)="ACS G/L system" : _
	item1$(2)="Accountant's Diskette"
	fncomboa("claims-srt",lc,mypos,mat item1$,tt$) : _
	resp$(1)=item1$(1)
	fnLbl(lc+=1,1,"General Ledger Company Number:",mylen,right)
	fnTxt(lc,mypos,5,0,left,number$) : _
	resp$(2)=env$('cno')
	fnCmdSet(2) : _
	fnAcs2(mat resp$,ckey)
	if ckey=5 then 
		goto Xit
	else if resp$(1)=item1$(1) then 
		pas$="BUILD" 
	else if resp$(1)=item1$(2) then 
		pas$="COPY"
	end if
	glcno=val(resp$(2))
	if pas$="COPY" then 
		close #1: ioerr ignore
		if fnCopy('A:GLmstr.H'&str$(glcno),'[Q]\CLmstr\*.*')<0 then 
			mat ml$(1)
			ml$(1)="Be sure the diskette is properly inserted and try again" 
			fnmsgbox(mat ml$,resp$,cap$,16) 
			goto MENU1
		end if
		goto Xit
	end if
	if trim$(pas$)><"BUILD" then goto MENU1
	close #1: ioerr ignore
	open #2: "Name=[Q]\GLmstr\GLmstr.h"&str$(glcno)&",KFName=[Q]\GLmstr\GLINDEX.h"&str$(glcno)&",Shr",internal,input,keyed ioerr MSGBOX1
	open #1: "Name=[Q]\CLmstr\GLmstr.H[cno],Size=0,RecL=62,Replace",internal,output
	do
		read #2,using 'Form POS 1,C 12,C 50': gl$,de$ eof END1
		write #1,using 'Form POS 1,C 12,C 50': gl$,de$
	loop
	END1: !
	close #1:
	close #2:
	execute "Index [Q]\CLmstr\GLmstr.H[cno]"&' '&"[Q]\CLmstr\GLINDEX.H[cno] 1 12 Replace DupKeys"
goto Xit
Xit: fnXit

MSGBOX1: ! r:
	mat ml$(2) : _
	ml$(1)="A general ledger chart of accounts has not been set up" : _
	ml$(2)="for this company.  You must choose a different option" : _
	fnmsgbox(mat ml$,resp$,cap$,16) : _
goto MENU1 ! /r
include: Ertn