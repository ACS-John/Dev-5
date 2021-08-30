! Replace S:\acsGL\ASCIIGLm
! create Ascii oputput file
 
	autoLibrary
	fnTop(program$,cap$="Create ASCII File")
	on error goto Ertn
 
	dim de$*50,fl$*50
 
	fncno(cno,cnam$)
 
	crlf$=chr$(13)&chr$(10)
	fl$="C:\ASCIIGLM.txt"
	fnTos(sn$="GLascii") : _
	mylen=30: mypos=mylen+3 : right=1
	fnLbl(1,1,"Path and File Name to Create:",mylen,right)
	fnTxt(1,mypos,50,0,0,"",0,"Enter the drive and filename where the file should be stored. Remember this name so you can find the file and access it with some other software.",0 ) : _
	resp$(1)=fl$
	fnCmdKey("&Next",1,1,0,"Will create an ascii file of the general ledger accounts.")
	fnCmdKey("&Cancel",5,0,1,"Will return to menu without creating a file.")
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	fl$=trim$(resp$(1))
	open #2: "Name="&fl$&",RecL=79,EOL=CRLF,Replace",external,output
	open #1: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno]",i,i,k
L240: read #1,using 'Form POS 1,C 12,C 50,POS 87,PD 6.2': gl$,de$,cb eof Xit
	write #2,using 'Form POS 1,C 12,X 2,C 50,N 12.2,c 2': gl$,de$,cb,crlf$
	goto L240
Xit: fnXit
! 
include: ertn
