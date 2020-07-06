fn_setup
dim filename$*512
if fnAskFileName(filename$,'open','*.txt;*.csv','Tab Delimited',env$('cap'))>0 then
	! r: read file and make mat col_account$ and mat col_meterNumber$
	fnOpenPrn
 
	open #hIn:=fngethandle: 'name='&filename$,d,i
	open #hCustomer:=fngethandle: 'Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr',internal,outin,keyed
	open #hCustomer2:=fngethandle: 'Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx2.h[cno],Shr',internal,outin,keyed
	open #hCustomer3:=fngethandle: 'Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx3.h[cno],Shr',internal,outin,keyed
	open #hCustomer4:=fngethandle: 'Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx4.h[cno],Shr',internal,outin,keyed
	open #hCustomer5:=fngethandle: 'Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx5.h[cno],Shr',internal,outin,keyed
	dim line$*512
	dim item$(0)*80
	dim col_account$(0)*80
	dim col_meterNumber$(0)*80
	dim col_meterNumber$(0)*80
	! r: headings - mostly just consume, but use to determine delimiter
	linput #hIn: line$ eof EoIn
	if pos(line$,chr$(9))>0 then
		delimiter$=chr$(9)
	else if pos(line$,',')>0 then
		delimiter$=','
	else
		pr #255: 'unable to determine delimiter'
		pause
	end if
	item_account=2
	item_meterNumber=3
	item_transmitterNumber=4
	! /r
	do
		linput #hIn: line$ eof EoIn
		if trim$(line$)<>'' then
			str2mat(line$,mat item$,delimiter$)
			if trim$(item$(item_account))='' then
				blankAccountCount+=1
			else if fnKeyExists(hCustomer,item$(item_account), 1) then
				! rewrite #hCustomer,using 'form pos 1741,N 2',key=item$(item_account): 11
				if srch(mat col_account$,item$(item_account))>0 then
					pr #255: 'dupe account: '&item$(item_account)
					dupeAccountCount+=1
				else if srch(mat col_meterNumber$,item$(item_meterNumber))>0 then
					pr #255: 'dupe meterNumber: '&item$(item_meterNumber) : pause
				else if srch(mat col_transmitterNumber$,item$(item_transmitterNumber))>0 then
					pr #255: 'dupe transmitterNumber: '&item$(item_transmitterNumber) : pause
				else
					fnAddOneC(mat col_account$,trim$(item$(item_account)))
					fnAddOneC(mat col_meterNumber$,item$(item_meterNumber))
					fnAddOneC(mat col_transmitterNumber$,item$(item_transmitterNumber))
				end if
			else
				invalidAccountCount+=1
				pr #255: 'no such account: '&item$(item_account)
			end if
		en if
	loop
	EoIn: !
	! /r
	! r: update the Meter Location table from mat col_account$ and mat col_meterNumber$ (and update mat used)
		mat used(udim(mat col_account$))
		dim location$(0)*256,locationN(0)
		hLocation=fn_open('U4 Meter Location',mat location$,mat locationN,mat form$, 0,1)
		hLocation4=hLocation+3
		for x=1 to udim(col_account$)
			mat location$=('')
			mat locationN=(0)
			location$(loc_activeCustomer)=col_account$(x)
			location$(loc_serviceId)='WA'
			locationKey4$=fnbuildkey$('U4 Meter Location',mat location$,mat locationN,4)
			read #hLocation4,using form$(hLocation),key=locationKey4$: mat location$,mat locationN nokey Location4NoKey
			location$(loc_meterNumber)=col_meterNumber$(x)
			location$(loc_transmitter)=col_transmitterNumber$(x)
			rewrite #hLocation4,using form$(hLocation): mat location$,mat locationN
			locationUpdateCount+=1
			NextLoccation: !
		nex x
		goto EoLocation
		Location4NoKey: !
			locationAddCount+=1
		goto NextLoccation
		! do
		! 	which=srch(mat col_account$,trim$(location$(loc_activeCustomer)))
		! 	if which>0 then
		! 		matchCount+=1
		! 		! write
		! 		location$(loc_meterNumber)=col_meterNumber$(which)
		! 		location$(loc_transmitter)=col_transmitterNumber$(which)
		! 	else
		! 		missCount+=1
		! 	end if
		! loop
		EoLocation: !
		pr #255: 'blank Account Count=';blankAccountCount
		pr #255: 'dupe Account Count=';dupeAccountCount
		pr #255: 'invalid Account Count=';invalidAccountCount
		pr #255: 'Update Count=';locationUpdateCount
		pr #255: 'Add Count=';locationAddCount
		fnClosePrn
	! /r
end if
goto Xit
Xit: stop ! fnXit
include: fn_open
include: fn_setup
