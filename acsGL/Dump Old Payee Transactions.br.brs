! Vendor(Payee)  Dump old Transactions

fn_setup

fnTop(program$)
open #hPayeeTran=fnH: "Name=[Q]\GLmstr\GLTR1099.h[cno],KFName=[Q]\GLmstr\gltridx1.h[cno],Shr",i,outIn,k

fnTos
fnLbl(1,1,"Oldest Transaction Date to Retain:",35,right)
fnTxt(1,37,8,0,left,'CCYYMMDD',0,'All payee transactions older than the date you enter here will be removed.')
resp$(1)=str$(oldestdate)
fnLbl(1,50,'')
fnCmdSet(2)
ckey=fnAcs(mat resp$)
if ckey<>5 then
	oldestdate=val(resp$(1))
	do
		read #hPayeeTran,using 'form pos 1,x 8,n 6': dt eof Finis
		if oldestdate>fndate_mmddyy_to_ccyymmdd(dt) then
			delete #hPayeeTran:
		end if
	loop
end if
Finis: !
close #hPayeeTran:
Xit: fnXit
include: fn_setup
