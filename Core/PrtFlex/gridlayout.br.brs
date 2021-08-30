! Replace S:\Core\PrtFlex\GridLayout
! create layout for grid program
 
	autoLibrary
	on error goto Ertn
 
	dim resp$(6)*60,text$*50,outputfile$*60,fieldnam$*30,vn$*20,ft$*11,an$*20
 
	fnTop("S:\Core\PrtFlex\GridLayout","Grid Layout")
L100: fnTos(sn$="file_layout") : _
	lablen=15
	fnLbl(1,1,"File Name:",lablen,1)
	fnTxt(1,lablen+2,60,0,0,"") : _
	resp$(1)="programfolder\grid\data_base_name\filename"
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	outputfile$=trim$(resp$(1))&".fil"
	open #10: "Name="&outputfile$&",RecL=87,Use",d,o ioerr L100
L170: fnTos(sn$="file_layout") : _
	lablen=30
	fnLbl(1,1,"Field name:",lablen,1)
	fnTxt(1,lablen+2,30,0,0,"") : _
	resp$(1)=""
	fnLbl(2,1,"Variable Name:",lablen,1)
	fnTxt(2,lablen+2,20,0,0,"") : _
	resp$(1)=""
	fnLbl(3,1,"Field Length:",lablen,1)
	fnTxt(3,lablen+2,4,0,0,"20") : _
	resp$(1)=""
	fnLbl(4,1,"# of Decimal Positions:",lablen,1)
	fnTxt(4,lablen+2,2,0,0,"20") : _
	resp$(1)=""
	fnLbl(5,1,"Format (eg. C 30,pd 4.2):",lablen,1)
	fnTxt(5,lablen+2,11,0,0,"") : _
	resp$(5)=""
	fnLbl(6,1,"Abbreviated Name:",lablen,1)
	fnTxt(6,lablen+2,20,0,0,"") : _
	resp$(6)=""
	fnCmdSet(11)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	fieldnam$=trim$(resp$(1))
	vn$=trim$(resp$(2))
	fl=val(resp$(3))
	dp=val(resp$(4))
	ft$=trim$(resp$(5))
	an$=trim$(resp$(6))
	pr #10,using 'Form POS 1,C 30,C 20,N 4,N 2,C 11,C 20': fieldnam$,vn$,fl,dp,ft$,an$
	goto L170
 
Xit: stop
 
include: ertn
 
