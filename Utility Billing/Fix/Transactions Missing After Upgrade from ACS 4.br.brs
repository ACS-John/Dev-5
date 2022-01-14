! GreeneCo - Melissa
! Fred Cook
! 102068.00
!  is missing a lot of newer transactions
! transaction import process stopped around 9/01/2010
! we need the remainder of the transactions imported.
 
fn_setup
if env$('acsDeveloper')<>'' then
	open #hOld=fnH: 'Name=C:\ACS\(Client_Files)\GreeneCo\Old ACS 4 - Program Files (x86)-ACS\UBmstr\UBTransVB.h1,Shr',i,i,r
else
	open #hOld=fnH: 'Name=@:C:\Program Files (x86)\ACS\UBmstr\UBTransVB.h1,Shr',internal,input
end if
dim tg(11)
hTrans=fn_openFio('UB Transaction',mat t$,mat tN)
 
	do
		read #hOld,using 'form pos 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof EoOld
		if ~fn_transAlreadyExist(p$,tdate,tcode,tamount) then
			mat t$=('') : mat tN=(0)
			t$(trans_acct)=p$
			tN(trans_tdate)=tdate
			tN(trans_tcode  )=tcode
			tN(trans_tamount)=tamount
			tN(trans_tg_1   )=tg(1 )
			tN(trans_tg_2   )=tg(2 )
			tN(trans_tG_3   )=tg(3 )
			tN(trans_tG_4   )=tg(4 )
			tN(trans_tG_5   )=tg(5 )
			tN(trans_tG_6   )=tg(6 )
			tN(trans_tG_7   )=tg(7 )
			tN(trans_tG_8   )=tg(8 )
			tN(trans_tG_9   )=tg(9 )
			tN(trans_TG_10  )=tg(10)
			tN(trans_TG_11  )=tg(11)
			tN(trans_s1read )=wr
			tN(trans_s1use  )=wu
			tN(trans_s3read )=er
			tN(trans_s3use  )=eu
			tN(trans_s4read )=gr
			tN(trans_s4use  )=gu
			tN(trans_tbal   )=tbal
			tN(trans_pcode  )=pcode
			write #hTrans,using form$(hTrans): mat t$,mat tN
		end if
	loop
	EoOld: !
Xit: stop
def fn_transAlreadyExist(p$,tdate,tcode,tamount; ___,returnN,taeKeyMatch)
	if ~taeSetup then
		taeSetup=1
		dim tae$(0)*256,taeN(0)
		hTaeTrans=fn_openFio('UB Transaction',mat tae$,mat taeN, 1,1)
	end if
	returnN=0
	mat tae$=('')
	mat taeN=(0)
	tae$(trans_acct)=p$
	taeN(trans_tdate)=tdate
	taeN(trans_tcode)=tcode
	! taeN(trans_tamount)=tamount
	restore #hTaeTrans,key=>fnbuildkey$('UB Transaction',mat tae$,mat taeN, 1): nokey TaeFinis
	do
		read #hTaeTrans,using form$(hTaeTrans): mat tae$,mat taeN eof TaeFinis
		if tae$(trans_acct)=p$ and taeN(trans_tdate)=tdate and taeN(trans_tcode)=tcode then
			taeKeyMatch=1
		else
			taeKeyMatch=0
		end if
		if taeN(trans_tamount)=tamount then
			returnN=1
		end if
	loop while taeKeyMatch and ~returnN
	TaeFinis: !
	fn_transAlreadyExist=returnN
fnend
include: fn_open
include: fn_setup
