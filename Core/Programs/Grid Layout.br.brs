! Replace S:\Core\GridLayout  ! layout for grid program
autoLibrary
on error goto Ertn
fnTop(program$)

ScrAskOutputFile: !
	fnTos
	dim resp$(6)*128
	lablen=15
	fnLbl(1,1,'File Name:',lablen,1)
	fnTxt(1,lablen+2,60,0,0,'')
	resp$(1)='S:\[curSystem]\grid\(DataBaseName)\(FileName)'
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	dim outputfile$*60
	outputfile$=trim$(resp$(1))&'.fil'
	open #hOut=fnH: 'Name='&outputfile$&',RecL=87,use',d,o ioerr ScrAskOutputFile
do
	fnTos
	lablen=30
	fnLbl(1,1,'Field name:',lablen,1)
	fnTxt(1,lablen+2,30,0,0,'')
	resp$(1)=''
	fnLbl(2,1,'Variable Name:',lablen,1)
	fnTxt(2,lablen+2,20,0,0,'')
	resp$(1)=''
	fnLbl(3,1,'Field Length:',lablen,1)
	fnTxt(3,lablen+2,4,0,0,'20')
	resp$(1)=''
	fnLbl(4,1,'# of Decimal Positions:',lablen,1)
	fnTxt(4,lablen+2,2,0,0,'20')
	resp$(1)=''
	fnLbl(5,1,'Format (eg. C 30,pd 4.2):',lablen,1)
	fnTxt(5,lablen+2,11,0,0,'')
	resp$(5)=''
	fnLbl(6,1,'Abbreviated Name:',lablen,1)
	fnTxt(6,lablen+2,20,0,0,'')
	resp$(6)=''
	fnCmdSet(11)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	dim fieldnam$*30
	fieldnam$=trim$(resp$(1))
	dim vn$*20
	vn$=trim$(resp$(2))
	fl=val(resp$(3))
	dp=val(resp$(4))
	dim ft$*11
	ft$=trim$(resp$(5))
	dim an$*20
	an$=trim$(resp$(6))
	pr #hOut,using L400: fieldnam$,vn$,fl,dp,ft$,an$
	L400: form pos 1,c 30,c 20,n 4,n 2,c 11,c 20
loop
Xit: !
close #hOut:
stop
include: ertn
