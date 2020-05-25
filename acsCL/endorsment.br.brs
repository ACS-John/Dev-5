! REPLACE S:\acsCL\Endorsment
! pr endorsment on back of check
 
	autoLibrary
	fnTop(program$,"Endorse Checks")
	on error goto Ertn
ENDORSE_CHECKS: !
	fnTos(sn$="Endorse")
	fnLbl(1,1,"Number of Endorsements:",25,1,0)
	fnTxt(1,28,6,0,0,"30",0,"You can guess. Too many will only cause you to have to cancel print.")
	resp$(1)=""
	fnLbl(2,1,"Bank Account:",25,1,0)
	fnTxt(2,28,15,0,0,"30",0,"Enter your bank account number if you want it shown on the back of the check.")
	resp$(2)=""
	fnCmdKey("&Next",1,1,0,"Proceed with printing.")
	fnCmdKey("&Cancel",5,0,1,"Cancel printing any check endorsments.")
	fnAcs2(mat resp$,ckey) ! endorse check
	if ckey=5 then goto Xit
	endorsements=val(resp$(1))
	bank=val(resp$(2))
	fnopenprn
	for j=1 to endorsements
		pr #255,using "Form pos 1,cc 30,skip 1,cc 30": "For Deposit Only",env$('cnam')(1:30)
		if bank>0 then pr #255,using "Form pos 1,cc 30": "Account: "&str$(bank)
		pr #255: newpage
	next j
	fncloseprn
	goto Xit
Xit: fnXit
! <updateable region: ertn>
ERTN: fnerror(program$,err,line,act$,"Xit")
	if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
	if uprc$(act$)="PAUSE" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT ! if env$("ACSDeveloper")<>"" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
	pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
ERTN_EXEC_ACT: execute act$ : goto ERTN
! </updateable region: ertn>
