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
				rewrite #hCustomer,using 'form pos 1741,N 2',key=item$(item_account): 11
			end if
		en if
	loop
	EoIn: !
	! /r
		fnClosePrn
	! /r
end if
goto xit
Xit: stop ! fnXit
include: fn_open
def fn_setup
	if ~setup then
		setup=1
		library 'S:\Core\Library': fngethandle
		library 'S:\Core\Library': fnmsgbox
		library 'S:\Core\Library': fnXit
		library 'S:\Core\Library': fnAddOneC
		library 'S:\Core\Library': fnureg_read,fnureg_write
		library 'S:\Core\Library': fnKeyExists
		library 'S:\Core\Library': fnAskFileName
		library 'S:\Core\Library': fnOpenPrn,fnClosePrn
		library 'S:\Core\Library': fnbuildkey$
		on error goto Ertn
	end if
fnend
include: ertn