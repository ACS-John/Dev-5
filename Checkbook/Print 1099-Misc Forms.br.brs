! r: setup library, fnTop, dims, on error
	autoLibrary
	fnTop(program$)
	on error goto Ertn
! /r
! r: body of program
	if fn1099MiscAsk(seltp,unused_type,minamt,beg_date,end_date) then
		open #payee=fnH: 'Name=[Q]\CLmstr\PayMstr.h[cno],KFName=[Q]\CLmstr\PayIdx2.h[cno],Shr',i,i,k
		open #trmstr2=fnH: 'Name=[Q]\CLmstr\TrMstr.h[cno],KFName=[Q]\CLmstr\TrIdx2.h[cno],Shr',i,i,k
		do
			dim vn$*8
			dim nam$*30
			dim ad$(3)*30
			dim ss$*11
			read #payee,using 'form pos 1,C 8,4*c 30,x 5,n 2,c 11',release: vn$,nam$,mat ad$,typ,ss$ eof Finis
			gosub ReadTransactions
			if typ<>0 and ytdp>minamt then
				if seltp=0 or seltp=typ then
					dim box(20)
					mat box=(0)
					if typ<1 or typ>8 then typ=1
					box(typ)=ytdp
					fn1099MiscPrint(vn$,nam$,mat ad$,ss$,mat box)
				end if
			end if
		loop
		Finis: !
		close #payee: ioerr ignore
		close #trmstr2: ioerr ignore
		fn1099MiscPrintClose  !  if lz1$='E' then close #5: else    gosub RELEASE_PRINT
	end if
goto Xit ! /r
Xit: fnXit
include: ertn
ReadTransactions: ! r: passed trmstr2,vn$,beg_date,end_date    returns ytdp (year to date pay)
	ytdp=wbc=0 : wtt=1 ! all banks and only checks
	dim key$*19
	key$=vn$&cnvrt$('pic(Z#)',wbc)&cnvrt$('pic(#)',wtt)&rpt$(chr$(0),8)
	restore #trmstr2,key>=key$: nokey rtFinis
	do
		dim tr$(5)*35
		read #trmstr2,using 'form pos 1,n 2,n 1,C 8,G 6,PD 10.2,C 8,C 35,N 1,N 6,N 1',release: bank_code,tcde,tr$(1),tr$(2),tr3,tr$(4),tr$(5),pcde,clr,scd eof rtFinis
		if trim$(vn$)=trim$(tr$(4)) then
			tranDate=fndate_mmddyy_to_ccyymmdd(val(tr$(2)))
			if tranDate=>beg_date and tranDate<=end_date then
				ytdp+=tr3
			end if
		end if
	loop while trim$(vn$)=trim$(tr$(4))
	rtFinis: !
return ! /r
