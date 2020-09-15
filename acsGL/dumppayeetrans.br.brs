! Replace S:\acsGL\DumpPayeeTrans
! Vendor(Payee)  Dump old Transactions

fn_setup

fnTop(program$,"Dump Old Payee Transactions")
open #hTrans:=fnH: "Name=[Q]\GLmstr\GLTR1099.H[cno],KFName=[Q]\GLmstr\gltridx1.h[cno],Shr",internal,outIn,keyed

fnTos
fnLbl(1,1,"Oldest Transaction Date to Retain:",35,right)
fnTxt(1,37,8,0,left,'CCYYMMDD',0,'All payee transactions older than the date you enter here will be removed.')
resp$(1)=str$(oldestdate)
fnLbl(1,50,'')
fnCmdSet(2)
fnAcs(mat resp$,ckey)
if ckey=5 then
	goto Xit
else
	oldestdate=val(resp$(1))
	do
		read #hTrans,using 'form pos 1,x 8,n 6': dt eof Finis
		if oldestdate>fndate_mmddyy_to_ccyymmdd(dt) then
			delete #hTrans:
		end if
	loop
end if
Finis: !
close #hTrans:
goto Xit
Xit: fnXit
include: fn_setup
