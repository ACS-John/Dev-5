! Replace S:\Core\GridLayout  ! layout for grid program
	autoLibrary
	dim resp$(6)*60,text$*50,outputfile$*60,fieldnam$*30,vn$*20,ft$*11,an$*20
 
	on error goto Ertn
	fnTop(program$,"Grid Layout")
 
L80: fnTos(sn$="file_layout")
	lablen=15
	text$="File Name:"
	fnLbl(1,1,text$,lablen,1)
	fnTxt(1,lablen+2,60,0,0,"")
	resp$(1)="programfolder\grid\data_base_name\filename"
	fnCmdSet(2)
	fnAcs(mat resp$,ckey)
	outputfile$=trim$(resp$(1))&".fil"
	open #10: "Name="&outputfile$&",RecL=87,use",display,output ioerr L80
L160: sn$="file_layout"
	fnTos(sn$)
	lablen=30
	fnLbl(1,1,"Field name:",lablen,1)
	fnTxt(1,lablen+2,30,0,0,"")
	resp$(1)=""
	fnLbl(2,1,"Variable Name:",lablen,1)
	fnTxt(2,lablen+2,20,0,0,"")
	resp$(1)=""
	text$="Field Length:"
	fnLbl(3,1,text$,lablen,1)
	fnTxt(3,lablen+2,4,0,0,"20")
	resp$(1)=""
	text$="# of Decimal Positions:"
	fnLbl(4,1,text$,lablen,1)
	fnTxt(4,lablen+2,2,0,0,"20")
	resp$(1)=""
	text$="Format (eg. C 30,pd 4.2):"
	fnLbl(5,1,text$,lablen,1)
	fnTxt(5,lablen+2,11,0,0,"")
	resp$(5)=""
	text$="Abbreviated Name:"
	fnLbl(6,1,text$,lablen,1)
	fnTxt(6,lablen+2,20,0,0,"")
	resp$(6)=""
	fnCmdSet(11)
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	fieldnam$=trim$(resp$(1))
	vn$=trim$(resp$(2))
	fl=val(resp$(3))
	dp=val(resp$(4))
	ft$=trim$(resp$(5))
	an$=trim$(resp$(6))
	pr #10,using L400: fieldnam$,vn$,fl,dp,ft$,an$
L400: form pos 1,c 30,c 20,n 4,n 2,c 11,c 20
	goto L160
Xit: stop
include: ertn
