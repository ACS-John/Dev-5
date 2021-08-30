fn_setup
fntop(program$)
print newpage
! exe 'con gui off'
! for repeat=1 to 5
	print 'Bitcoin Price as of '&date$("MM/DD/CCYY")&' - '&time$&' is '&str$(fn_getPrice)
! 	sleep(1)
! next repeat

! fn_download('http://planetacs.net/acs5update/Release_Notes.txt','[temp]\Release_Notes.txt')

! r: functional proof I can read the release notes off the web
!	open #hC=fnH: "name=http://planetacs.net/acs5update/Release_Notes.txt, http=client",display,outin
!	do
!		dim line$*2048
!		linput #hC: line$ eof EoC
!		lineCount+=1
!		pr rtrm$(line$)
!		if lineCount/24=int(lineCount/24) then pause
!	loop
!	EoC: !
!	close #hC:
!	pause
! /r

! exe 'con gui on'
end

def fn_download(url$*2048,destination$*512; ___,hOut,hIn,line$*2048,lineCount)
	
	! fnCopyFile(url$&', http=client',destination$)
	fn_copyFile(url$&', http=client',destination$)
	
	! open #hIn=fnH: 'name='&url$&', http=client',display,outin
	! open #hOut=fnH: 'name='&destination$&',replace',d,o
	! do
	! 	dim line$*2048
	! 	linput #hIn: line$ eof EoDi
	! 	lineCount+=1
	! 	print #hOut: line$
	! loop
	! EoDi: !
	! close #hIn:
	! close #hOut:
	
	

	
	
fnend
def fn_copyFile(fromFile$*255,toFile$*255;disableProgressBar,___,hFrom,hTo,written,cx,chunk$*10000,enableProgressBar)
	! enableProgressBar=~disableProgressBar
	open #hFrom=fnH: 'name='&fromFile$&',recl=10000,shr',external,outin ! was input
	open #hTo=fnH: 'name='&toFile$&',recl=10000,replace',external,output
	do ! Read the next record and process it
			written=0
			read #hFrom, using FcopyFile : chunk$ eof DoneCopying ioerr ShortRec
			if ~written then write #hTo, using FcopyFile : chunk$
			! if enableProgressBar and lrec(hFrom) then fnUpdateProgressBar(rec(hFrom)/lrec(hFrom),'',0,0,0,'Transferring ''&fromFile$&'' File')
	loop

	DoneCopying: ! Done Copying the file here
	! if enableProgressBar then fnCloseProgressBar
	close #hFrom:
	close #hTo:
fnend
FcopyFile: form C 10000
ShortRec: ! r: Process short record at EOF
	cx=Cnt
	if err><4271 then msgbox('Error during copying. Please try again.') : goto DoneCopying
	reread #hFrom, using FcopyFile : chunk$
	! chunk$=rtrm$(chunk$,chr$(0))
	rln(hTo,cx)
	write #hTo, using 'form C '&str$(cx) : chunk$(1:cx)
	written=1
continue ! /r

def fn_getPrice(;___,hConnection,line$*999)
	open #hConnection=fnH: "name= https://min-api.cryptocompare.com/data/price?fsym=BTC&tsyms=USD&extraParams=SageLive, http=client",display,outin
	linput #hConnection: line$ eof ignore
	line$(1:pos(line$,'USD')+4)=""
	line$=srep$(line$,"}","")
	fn_getPrice=val(line$) conv ignore
	close #hConnection:
fnend
Xit: fnXit
include: fn_setup
