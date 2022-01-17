autoLibrary
fnTop(program$)
on error goto Ertn
if fn1099NecAsk(seltp,unused_type,minamt,beg_date,end_date) then
	open #hPayee=fnH: 'Name=[Q]\GLmstr\PayMstr.h[cno],KFName=[Q]\GLmstr\PayIdx2.h[cno],Shr',internal,outIn,keyed
	open #hTrans=fnH: 'Name=[Q]\GLmstr\GLTR1099.h[cno],KFName=[Q]\GLmstr\gltridx1.h[cno],Shr',internal,outIn,keyed
	do
		dim vn$*8
		dim nam$*30
		dim ss$*11
		dim box(11)
		dim ad$(3)*30
		read #hPayee,using 'form pos 1,C 8,4*c 30,x 5,n 2,c 11',release: vn$,nam$,mat ad$,typ,ss$ eof EoPayee
		ytdp=fn_YearToDapPay(hTrans,vn$, beg_date,end_date)
		form pos 1,c 8,c 35,3*c 20,x 5,n 2,c 11
		if typ<>0 then
			if ytdp=>minamt then
				if seltp=0 or seltp=typ then
					mat box=(0)
					if typ<1 or typ>8 then typ=1
					box(1)=ytdp ! box(typ)=ytdp  ! set to force box 1 (non employee compensation) for 2021 1099-NEC   (john working with Zaleski 1/16/22)
					fn1099NecPrint(vn$,nam$,mat ad$,ss$,mat box)
				end if
			end if
		end if
	loop
	EoPayee: !
	close #hPayee: ioerr ignore
	close #hTrans: ioerr ignore
	fn1099NecPrintClose
end if
Xit: fnXit
def fn_YearToDapPay(hTrans,key$; beg_date,end_date,___,returnN)
	restore #hTrans,key>=key$: nokey ytdpFinis
	do
		read #hTrans,using 'form pos 1,c 8,N 6,PD 5.2',release: trvn$,dt,am eof ytdpFinis
		if trim$(key$)=trim$(trvn$) then
			if beg_date=0 or fndate_mmddyy_to_ccyymmdd(dt)=>beg_date then
				if end_date=0 or fndate_mmddyy_to_ccyymmdd(dt)<=end_date then
					returnN+=am
				end if
			end if
		end if
	loop while trim$(key$)=trim$(trvn$)
	ytdpFinis: !
	fn_YearToDapPay=returnN
fnend
include: ertn
