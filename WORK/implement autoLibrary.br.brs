autoLibrary
on error goto Ertn
fnTop(program$)
pr b: env$('program_caption')
tab$=chr$(9)
fn_getList
fileCount=udim(mat fileName$)
countlinesRemoved=countAutoLibraryAdded= _
	countProgramWithLineNoSkipped=countProgramWithLineNoFixed= _
	countLibraryOfferingProgram= 0
for item=1 to fileCount
	fnStatus(str$(item)&'/'&str$(fileCount))
	! fnStatusPause
	ProcessThisFile: !
	if exists(fileName$(item)) then
		fnStatus(fileName$(item))
		open #hIn :=fnH: 'name='&fileName$(item),d,i
		dim tmpFile$*512
		tmpFile$=env$('temp')&'\acs\tmpSourceFile.br.brs'
		open #hOut:=fnH: 'name='&tmpFile$&',RecL=1024,Replace',d,o
		encounteredAutoLibrary=lineCount=enableSkipThisFile=0
		do
			dim line$*2048
			linput #hIn: line$ eof EoIn
			dim ln$*2048 ! modified version of line$ for testing purposes
			dim tab$*1
			tab$=chr$(9)
			tabLevel=fn_tabLevel(line$)
	
			ln$=srep$(line$,tab$,'')
			ln$=trim$(ln$)
			fn_stripComment(ln$)
			lineCount+=1
			! r: check for and if necessary remove line numbers and restart
			if lineCount=1 then
				lineVal=0
				lineVal=val(ln$(1:5)) conv ignore
				if lineVal then
					close #hIn:
					close #hOut:
					if fn_removeLineNumbers(fileName$(item)) then
						countProgramWithLineNoSkipped+=1
						fnStatus(' . line numbers removed - restarting this files processing.')
						goto ProcessThisFile
					else 
						countProgramWithLineNoFixed+=1
						fnStatus(' X failed removing line numbers - skipping it.')
						enableSkipThisFile=1
						goto EoIn
					end if
				end if
			en if
			! /r
			
			ln$=lwrc$(ln$)
			
			! pr ln$ : pause
			
			if ln$(1:11)='autolibrary' then
				! existing autoLibrary encountered
				encounteredAutoLibrary=1
				pr #hOut: line$
			else if ln$(1:14)='def library fn' and ln$(15:15)<>'_' then
				! r: library offering program - SKIPPED
				countLibraryOfferingProgram+=1
				fnStatus(' X program offers a library lets just skip these for now and see how many they are')
				enableSkipThisFile=1
				! /r
			else if ln$(1:6)='def fn' and ln$(7:7)<>'_' then 
				! r: illegal local function name
				encounterBadFn=1
				fnStatus(' Y Illegally named function: '&ln$)
				fnStatus(' Y Make your changes to the program and then continue to restart on this program.')
				close #hIn:
				close #hOut:
				execute 'sy ""C:\ACS\Program\Notepad++\notepad++.exe" "'&filename$(item)&'" -n'&str$(lineCount)&'"'
				! pause
				fnStatusPause
				! enableSkipThisFile=1
				goto ProcessThisFile
				! /r
			else if ln$(1:26)="library 's:\core\library':" then
				! r: autoLibrary magic
				if encounteredAutoLibrary then
					countlinesRemoved+=1
					! not even a blank line ! pr #hOut: ''
				else ! if ~encounteredAutoLibrary then
					encounteredAutoLibrary=1
					pr #hOut: rpt$(tab$,tablevel)&'autoLibrary'
					countAutoLibraryAdded+=1
				end if
				! /r
			else
				! fn_acsToAcs2(line$)
				pr #hOut: line$
			end if
		loop until enableSkipThisFile
		EoIn: !
		close #hIn:  ioerr ignore
		close #hOut: ioerr ignore
		if ~enableSkipThisFile then
			dim backupFile$*1024
			backupFile$
			if fn_makeBackup(fileName$(item),'.beforeAutoLibrary') then
				fnCopy(tmpFile$,fileName$(item))
			else
				fnStatus(' X Backup failed so new program not put into place.')
				pr bell;
				pause
			end if
		end if
	else 
		fnStatus(fileName$(item)&' : NOT FOUND - SKIPPED.')
	end if

nex item
! r: Totals Screen
fnStatus('*files processed: '&str$(item))
fnStatus('*lines removed: '&str$(countlinesRemoved))
fnStatus('*autoLibrary added: '&str$(countAutoLibraryAdded))
fnStatus('*Library Offering Programs (skipped): '&str$(countLibraryOfferingProgram))
fnStatus('*Programs with Line Numbers (fixed): '&str$(countProgramWithLineNoFixed))
fnStatus('*Programs with Line Numbers (skipped): '&str$(countProgramWithLineNoSkipped))
fnStatusPause
! /r
end
Xit: fnXit
def fn_removeLineNumbers(rFile$*1024; ___,returnN,hIn,hOut,rlnFile$*1024,line$*2048,lineLen)
	!  DO NOT PASS FILES THAT DO NOT CONTAIN LINE NUBERS
	fn_makeBackup(rFile$,'.beforeRemoveLineNumbers')
	
	rlnFile$=env$('temp')&'\acs\tmpSourceFile.br.brs'
	open #hIn :=fnH: 'name='&rFile$,d,i
	open #hOut:=fnH: 'name='&rlnFile$&',RecL=1024,Replace',d,o
	do
		linput #hIn: line$ eof R_eoIn
		line$=rtrm$(line$)
		line$=rtrm$(line$)
		lineLen=len(line$)
		if line$(lineLen-1:lineLen)='!:' then line$(lineLen-1:lineLen)=': _'
		line$(1:6)=''
		if trim$(line$)='!' then line$=''
		if lwrc$(trim$(line$)(1:4))='def '   then line$=ltrm$(line$)
		if lwrc$(trim$(line$)(1:5))='fnend'  then line$=ltrm$(line$)
		if lwrc$(trim$(line$)(1:6))='return' then line$=ltrm$(line$)
		line$=rtrm$(line$)
		! r: convert '  ' to '[tab]'
		! tabConvertCount=0
		! pr line$
		for x=10 to 2 step -2
			! pr x
			if line$(1:x)=rpt$(' ',x) then 
				! tabConvertCount+=1
				line$(1:x)=rpt$(chr$(9),x/2)
			end if
		nex x
		! if tabConvertCount>0 then
		! 	pr srep$(line$,chr$(9),'>') : pause
		! end if
		! /r
		pr #hOut: rtrm$(line$)
	loop
	R_eoIn: !
	close #hIn:
	close #hOut:
	returnN=fnCopy(rlnFile$,rFile$)
	! pr 'fn_removeLineNumbers returning ';returnN;' after processing '&rFile$ : pause
	fn_removeLineNumbers=returnN
fnend
def fn_makeBackup(of$*1024,newExt$*128; ___,returnN,backupFile$*1024)	
	backupFile$=of$&newExt$
	if exists(backupFile$) then
		returnN=1
	else
		returnN=fnCopy(of$,backupFile$)
	end if
	fn_makeBackup=returnN
fnend
def fn_tabLevel(&line$; ___,it,returnN)
	it=5 ! highest tested for level
	do until returnN or ~it
		it-=1
		if line$(1:(it))=rpt$(tab$,it) then 
			returnN=it
		end if
	loop 
	fn_tabLevel=returnN
fnend
def fn_stripComment(&ln$; ___,ePos)
	ePos=pos(ln$,'!')
	if ePos>0 then
		ln$(ePos:inf)=''
	en if
fnend
def fn_getList
	dim fileName$(0)*512
	mat fileName$(0)
	pr 'inside this function you should build a list of file names to process' :  pause
	! fnAddOneC(mat fileName$,'C:\ACS\Dev-5\acsCL\Conversion\apmstr-cnv.br.brs')
fnend
def fn_acsToAcs2(&line$; ___,posX,removing$*12,adding$*7)
	removing$=lwrc$('fnAcsOld(sn$,0,') ! must be lwrc$
	adding$='fnAcs('
	removingLen=len(removing$)
	posX=pos(lwrc$(line$),removing$)
	if posX>0 then
		! pr 'before line$: '&line$ ! pause
		line$(posX:posX+removingLen-1)=adding$
		! pr ' after line$: '&line$
		! pause
	end if
fnend
include: ertn
fn_removeLineNumbers