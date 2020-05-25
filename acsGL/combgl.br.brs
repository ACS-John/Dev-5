!  Replace S:\acsGL\CombGL
! Consolidate Companies
 
	autoLibrary
	fnTop(program$,cap$="Consolidate Master Files")
	on error goto Ertn
 
	dim a$*416,n$*40,cap$*128,resp$(2)*80
 
	cap$="Consolidate Companies"
	dcno=99
MAIN: !
	fnTos(sn$='Combgl') : _
	lc=rc=0 : _
	mylen=29 : mypos=mylen+2
	fnLbl(lc+=1,1,"&Source Company Number:",mylen,1)
	fncmbcno(lc,mypos) : _
	resp$(rc+=1)=''
	fnLbl(lc+=1,1,"&Destination Company Number:",mylen,1)
	fnTxt(lc,mypos,5,0,0,'30') : _
	resp$(rc+=1)=str$(dcno)
	if hcno>0 then let fnLbl(lc+=1,1,"Last Company Selected: "&str$(hcno),mylen,1)
	lc+=1
	fnLbl(lc+=1,1,"Warning",80,2,1)
	fnLbl(lc+=1,1,"Please make sure no one else is",80,2) : _
	fnLbl(lc+=1,1,"using either company. If the destination",80,2) : _
	fnLbl(lc+=1,1,"company exists, it will be over written",80,2) : _
	fnLbl(lc+=1,1,"by the first company selected.  All others",80,2) : _
	fnLbl(lc+=1,1,"will be combined with the first company selected. ",80,2)
	fnCmdKey("&Next",1,1,0,"Allows you to combine this company and select more if desired.")
	fnCmdKey("C&omplete",2,0,0,"All companies have been combined.  Return to the menu.")
	fnCmdKey("Cancel",5,0,1,"Stop without combining any companies.")
	fnAcs2(mat resp$,ck)
 
	if ck=5 then goto Xit
	if ck=2 then goto END1
	cno=val(resp$(1)(43:47)) : _
	dcno=val(resp$(2)) : _
	hcno=cno
	if cno=0 or ckey=5 then goto END1
	ctr+=1
	if ctr>1 then goto L390
	cno1=cno
	execute "Copy [Q]\GLmstr\*.H"&str$(cno1)&' '&"[Q]\GLmstr\*.H"&str$(dcno)&" -n" ioerr MAIN
	open #1: "Name=[Q]\GLmstr\Company.h"&str$(dcno)&"",internal,outIn  : _
	read #1,using ' Form POS 1,C 40': n$ : _
	n$(25:40)=" (Consolidated)" : _
	rewrite #1,using ' Form POS 1,C 40': n$ : _
	close #1:
	open #1: "Name=[Q]\GLmstr\GLmstr.H"&str$(dcno)&"",internal,output
	goto MAIN
 
L390: open #2: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno]",internal,input,keyed ioerr MAIN
L400: read #2,using 'Form POS 1,C 416': a$ eof L430
	write #1,using 'Form POS 1,C 416': a$
	goto L400
L430: close #2:
	goto MAIN
 
END1: close #1: ioerr L470
L470: execute "Index [Q]\GLmstr\GLmstr.H"&str$(dcno)&' '&"[Q]\GLmstr\GLIndex.H"&str$(dcno)&" 1 12 Replace DupKeys" ioerr Xit
	execute "Index [Q]\GLmstr\GLmstr.H"&str$(dcno)&' '&"[Q]\GLmstr\glIndx2.H"&str$(dcno)&" 13 30 Replace DupKeys"
	fnputcno(cno=dcno)
Xit: fnXit
 
include: Ertn
 
