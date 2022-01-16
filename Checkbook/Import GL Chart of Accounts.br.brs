autoLibrary
on error goto Ertn
 
dim resp$(10)*25
dim ml$(0)*128

 
fnTop(program$)
open #1: 'Name=[Q]\CLmstr\GLmstr.H[cno],KFName=[Q]\CLmstr\glindex.H[cno],Shr',internal,outIn,keyed
do
	MENU1: !
	fnTos
	mylen=38 : mypos=mylen+2 : lc=0
	fnLbl(lc+=1,1,'Extract General Ledger accounts from:',38,1)
	dim item1$(1)*45
	item1$(1)='ACS G/L system'
	! item1$(2)='Accountant's Diskette'
	fncomboa('claims-srt',lc,mypos,mat item1$,tt$)
	resp$(1)=item1$(1)
	fnLbl(lc+=1,1,'General Ledger Company Number:',mylen,1)
	fnTxt(lc,mypos,5,0,0,'30')
	resp$(2)=env$('cno')
	fnCmdSet(2)
	fnAcs(mat resp$,ckey)
	if ckey=5 then 
		goto Xit
	else ! else if resp$(1)=item1$(1) then 
		pas$='BUILD' 
	! else if resp$(1)=item1$(2) then 
	! 	pas$='COPY'
	end if
	glcno=val(resp$(2))
	! if pas$='COPY' then 
	! 	close #1: ioerr ignore
	! 	if fnCopy('A:GLmstr.H'&str$(glcno),'[Q]\CLmstr\*.*')<0 then 
	! 		mat ml$(1)
	! 		ml$(1)='Be sure the diskette is properly inserted and try again' 
	! 		fnmsgbox(mat ml$,resp$,'',16) 
	! 		goto MENU1
	! 	end if
	! 	goto Xit
	! end if
	if trim$(pas$)='BUILD' then ! else if
		close #1: ioerr ignore
		open #2: 'Name=[Q]\GLmstr\GLmstr.h'&str$(glcno)&',KFName=[Q]\GLmstr\GLINDEX.h'&str$(glcno)&',Shr',i,i,k ioerr MSGBOX1
		open #1: 'Name=[Q]\CLmstr\GLmstr.H[cno],Size=0,RecL=62,Replace',internal,output
		do
			dim de$*50
			read #2,using 'form pos 1,C 12,C 50': gl$,de$ eof END1
			write #1,using 'form pos 1,C 12,C 50': gl$,de$
		loop
		END1: !
		close #1:
		close #2:
		fnIndex('[Q]\CLmstr\GLmstr.H[cno]','[Q]\CLmstr\GLINDEX.H[cno]','1 12')
		goto Xit
	end if
loop
Xit: fnXit

MSGBOX1: ! r:
	mat ml$(2)
	ml$(1)='A General Ledger Chart of Accounts has not been set up'
	ml$(2)='for this company.  You must choose a different option'
	fnmsgbox(mat ml$,resp$,'',16)
goto MENU1 ! /r
include: Ertn