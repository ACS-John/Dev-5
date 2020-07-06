! Replace S:\acsGL\glreass
 
	autoLibrary
	fnTop(program$,"Reassign Transaction Addresses")
	on error goto Ertn
	dim ta(2),tr1$*70,wrd1$(2)*35,fil$(12,4)
 
MENU1: !
	fnTos(sn$="glreorg") : _
	mylen=20: mypos=mylen+3 : right=1
	fnLbl(1,1,"If you get errors trying to access general ledger")
	fnLbl(2,1,"transaction, this option might help.  It")
	fnLbl(3,1,"will reassign all transactions back to the ")
	fnLbl(4,1,"correct general ledger accounts.")
	fnLbl(5,1,"Take Next to continue with the reassign process.")
	fnCmdKey("&Next",1,1,0,"Reassigns the general ledger transactions.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu without processing.")
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	if ckey=1 then gosub REORG
	goto Xit
REORG: ! r:
	open #1: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.H[cno],Shr",internal,outIn,keyed
	open #2: "Name=[Q]\GLmstr\GLTRANS.H[cno],Shr",internal,outIn,relative
	L320: read #1,using L330: mat ta eof L360
	L330: form pos 333,2*pd 3
	rewrite #1,using L330: 0,0
	goto L320
	L360: lr2=lrec(2)
	rewrite #2,using L470,rec=1: lr2
	for j=1 to lr2
		read #2,using L400,rec=j: k$,nta noRec L480
		L400: form pos 1,c 12,pos 71,pd 3
		read #1,using L330,key=k$: mat ta nokey L480
		if ta(1)=0 then ta(1)=j
		if ta(2)>0 then rewrite #2,using L470,rec=ta(2): j
		ta(2)=j
		rewrite #1,using L330,key=k$: mat ta
		rewrite #2,using L470,rec=j: 0
		L470: form pos 71,pd 3
		L480: !
	next j
return ! /r
 
Xit: fnXit
 
 
ERTN: fnerror(program$,err,line,act$,er_out$)
	if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
	execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
	pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
ERTN_EXEC_ACT: execute act$ : goto ERTN
