autoLibrary
on error goto Ertn
fnTop(program$)
MAIN: !
	fnTos
	fnLbl(1,1,"Wrong Date:",22,1)
	fnTxt(1,23,8,0,0,"1")
	resp$(1)='' ! '120101' ! '012011'
	fnLbl(2,1,"Correct Date:",22,1)
	fnTxt(2,23,8,0,0,"1")
	resp$(2)='' ! '120102' ! '012012'
	fnLbl(4,1,"Lowest Record Number:",22,1)
	fnTxt(4,23,8,0,0,"0")
	resp$(3)='0' ! str$(33430) ! str$(88000)
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	date_bad=fndate_mmddyy_to_ccyymmdd(val(resp$(1)))
	date_good=fndate_mmddyy_to_ccyymmdd(val(resp$(2)))
	rec_low=val(resp$(3))

	open #h_trans=fnH: "Name=[Q]\UBmstr\ubtransvb.h[cno],KFName=[Q]\UBmstr\ubTrIndx.h[cno],Shr",i,outIn,k
	open #hTrans2=fnH: "Name=[Q]\UBmstr\ubtransvb.h[cno],KFName=[Q]\UBmstr\UBTrdt.h[cno],Shr",i,outIn,k
	do
		read #h_trans,using 'form pos 11,N 8': trans_date eof EO_TRANS
		! if trans_date=date_bad then pause
		if (rec_low=0 or rec(h_trans)=>rec_low) and trans_date=date_bad then
			chg_count+=1
			rewrite #h_trans,using 'form pos 11,N 8': date_good
		end if  ! rec_low>0 and rec(h_trans)<=rec_low and trans_date=date_bad then
	loop
EO_TRANS: !
	close #h_trans:
	close #hTrans2:
goto Xit

Xit: !
! pr 'chg_count=';chg_count : end
fnXit

include: ertn
