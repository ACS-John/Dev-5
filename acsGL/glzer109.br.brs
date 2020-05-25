! Replace S:\acsGL\glZer109
! Zero Year To Date Purchases
 
	autoLibrary
	fnTop(program$,cap$="Zero Year To Date Purchases")
	on error goto Ertn
 
	dim adr(2),cnam$*40,cap$*128,de$*30,re$*12
 
	fncno(cno,cnam$)
 
	open #1: "Name=[Q]\GLmstr\GL1099.h[cno],KFName=[Q]\GLmstr\GL109IDX.h[cno],Shr",internal,outIn,keyed ioerr L440
	open #2: "Name=[Q]\GLmstr\gltr1099.h[cno]",internal,outIn,relative
	pr newpage
SCR1: !
	fnTos(sn$="Glzer109") : _
	lc=0 : mylen=55 : mypos=mylen+3 : center=2 : right=1
	fnLbl(lc+=1,1,"* * *   Warning   * * *",60,center)
	fnLbl(lc+=1,1,"This selection will dump all old purchase transactions from each",width,0)
	fnLbl(lc+=1,1,"vendor (payee) record. This selection should only be run at year end",width,0)
	fnLbl(lc+=1,1," after all 1099 forms have been printed.:",mylen,0)
	fnLbl(lc+=1,1," Enter ZERO to continue:",mylen,right)
	fnTxt(lc,mypos,5) : _
	resp$(1)=""
	fnCmdSet(2)
	fnAcs2(mat resp$,ckey)
	if ckey=5 then goto Xit
	pas$=resp$(1)
	if lwrc$(pas$)<>lwrc$("zero") then goto SCR1
OLDEST_DATE: !
	fnTos(sn$="Glzer1092") : _
	lc=0 : mylen=30 : mypos=mylen+3 : width=0
	fnLbl(lc+=1,1,"Oldest Date to be Retained:",mylen,right)
	fnTxt(1,mypos,8,0,left,'CCYYMMDD',0,'For example, if you wantto dump all transactions up to the beginning of the new year, you would enter the first day of the new year.') : _
	resp$(1)=str$(transactionendingdate)
	fnLbl(lc,45,"",0,right)
	fnCmdSet(2)
	fnAcs2(mat resp$,ckey)
	if ckey=5 then goto Xit
	lastdate=val(resp$(1))
L370: read #2,using 'Form POS 1,C 8,N 6,PD 5.2,C 12,C 30': trvn$,da,amt,re$,de$ eof L400
	x=fndate_mmddyy_to_ccyymmdd(da)
	if x<lastdate then delete #2,rec=rec(2):
	goto L370
L400: close #2:
	execute "Copy [Q]\GLmstr\gltr1099.h[cno]" & " x -D"
	execute "Copy X [Q]\GLmstr\gltr1099.h[cno]"
	execute "Index [Q]\GLmstr\gltr1099.H[cno]"&' '&"[Q]\GLmstr\gltridx1.H[cno] 1 8 Replace DupKeys -N"
L440: if fnprocess=1 then let fnchain("S:\acsGL\acglAuto")
Xit: fnXit
 
include: Ertn
