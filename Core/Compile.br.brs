pr border: 'Import Source'
execute 'con gui off'
library program$: fnReCompile
fnReCompile
def library fnReCompile(; disableRebuildCache)
	autoLibrary
	dim filename$*255,msr_file$*255
	if ~exists("S:\(import)") then execute "sy -M md "&os_filename$("S:\(import)")
	open #proc_file:=1: 'Name=S:\(import)\compile.prc,RecL=1024,Replace',display,output

	if ~disableRebuildCache then
		execute "sy -M sortfiles -D . -C "".br.brs|.br""" ! ioerr RcDone
	end if

		dim sourceFile$(0)*256
		mat sourceFile$(0)
		fn_getFileList('S:\(import)\brsfiles',mat sourceFile$)
		sourceCount=udim(mat sourceFile$)
	! RcDone:!
	
	open #dirfile:=fngethandle: "Name=S:\(import)\brsfiles",display,input
	pr #proc_file: 'Scr_Freeze'
	for sourceItem=1 to sourceCount
		if fn_hasLineNumbers(sourceFile$(sourceItem)) then
			pr #proc_file: 'Load "'&sourceFile$(sourceItem)&'",Source'
			parameter$=fn_buildParameter$(sourceFile$(sourceItem))
			if exists(sourceFile$(sourceItem)(1:len(sourceFile$(sourceItem))-4)) then
				pr #proc_file: 'Replace "'&sourceFile$(sourceItem)(1:len(sourceFile$(sourceItem))-4)&'"'&parameter$
			else
				pr #proc_file: 'Save "'&sourceFile$(sourceItem)(1:len(sourceFile$(sourceItem))-4)&'"'&parameter$
			end if
		else
			pr #proc_file:  'sy ""C:\ACS\Dev-5\Sad Panda\Compile.cmd" "'&sourceFile$(sourceItem)&'""'
			! exe  'sy ""C:\ACS\Util\Lexi\ConvStoO.cmd" "'&sourceFile$(sourceItem)&'""'
		end if
		pr #proc_file: ''
	nex sourceItem
	pr #proc_file: 'Scr_Thaw'

	if env$("AfterRecompile")="" then
		pr #proc_file: "Sy"
	else
		pr #proc_file: 'chain "'&env$("AfterRecompile")&'"'
	end if
	close #dirfile: ioerr ignore
	msr_file$=file$(proc_file)
	close #proc_file:
	execute "subproc "&msr_file$
fnend
def fn_getFileList(filename$,mat list$; ___,dirfile,returnN)
	returnN=0
	mat list$(0)
	open #dirfile:=fngethandle: 'Name='&filename$,display,input
	do
		linput #dirfile: line$ eof Gfl_Done
		returnN=fnAddOneC(mat list$,line$)
	loop
	Gfl_Done: ! 
	fn_getFileList=returnN
fnend
def fn_buildParameter$(filename$*256)
	bp_gets_object=0
	filename$=lwrc$(filename$)
	if pos(filename$,'_s1.brs')>0 then   ! only files that end with _s1.brs can not be distributed as object
		bp_gets_object=0
	else if pos(filename$,'Core\client.br')>0 then
		bp_gets_object=1
	else if pos(filename$,'Core\programs\update.br')>0 then
		bp_gets_object=1
	else if pos(filename$,'Collection-Master Add-On')>0 then
		bp_gets_object=1
	end if
	if bp_gets_object then
		fn_buildParameter$=',object'
	end if
fnend
def fn_lexiLineNum(lextFileIn$*256,lextFileOut$*256)
	dim string$*4000
	dim const$(1)*800
	dim constantName$(1)*30
	dim currentSelect$*400
	dim currentCase$(1)*400
	dim afterString$*4000
	increment=1
	labelIncrement=10
	mat constantName$(0)
	mat const$(0)
	open #1: "name="&lextFileIn$, display, input
	open #2: "name="&lextFileOut$&",recl=800,replace",display,output
	do
		linput #1: string$ eof LexiDoneReading
		if ~skipNextOne and (ltrm$(string$)(1:1)="!" and pos(string$,"!")>3) then string$(1:4)=" ! ."&ltrm$(string$(1:4))
		for constIndex=1 to udim(Mat const$)
			if (constantPosition:=pos(Uprc$(string$),Uprc$(constantName$(constIndex)))) then
				string$=string$(1:constantPosition-1)&const$(constIndex)&string$(constantPosition+len(constantName$(constIndex)):len(string$))
			end if
		next constIndex
		if (constantPosition:=pos(Uprc$(string$),"#DEFINE#")) then
			constantPosition+=8
			if (constNameStartPos:=pos(string$,"[[",constantPosition)) then
				if (constNameEndPos:=pos(string$,"]]",constNameStartPos)) then
					constNameEndPos+=1
					mat const$(constIndex:=(udim(Mat const$)+1))
					mat constantName$(constIndex)
					constantName$(constIndex)=string$(constNameStartPos:constNameEndPos)
					const$(constIndex)=trim$(string$(constNameEndPos+2:len(string$)))
					if const$(constIndex)(1:1)="=" then ! If Equals, Then Ignore It
						const$(constIndex)=trim$(const$(constIndex)(2:len(const$(constIndex))))
					end if
					if const$(constIndex)(1:1)='"' and const$(constIndex)(len(const$(constIndex)):len(const$(constIndex)))='"' then
						const$(constIndex)=const$(constIndex)(2:len(const$(constIndex))-1) ! Remove Quotes If Both Are Present
					end if
				end if
			end if
		end if
		if (selectPosition:=pos(lwrc$(string$),"#select#")) then
			if (casePosition:=pos(lwrc$(string$),"#case#",selectPosition)) then
				currentSelect$=string$(selectPosition+8:casePosition-1)
				caseIndex=0
				currentCaseChunk=casePosition+6
				do
					caseIndex+=1
					mat currentCase$(caseIndex)
					if (nextCaseChunk:=pos(string$,"#",currentCaseChunk)) then
						currentCase$(caseIndex)=string$(currentCaseChunk:nextCaseChunk-1)
						currentCaseChunk=nextCaseChunk+1
					else
						currentCase$(caseIndex)=string$(currentCaseChunk:len(string$))
					end if
				loop while nextCaseChunk
				afterString$=" then  ! "&string$(selectPosition:len(string$))
				string$=string$(1:selectPosition-1)&"if "
				for caseIndex=1 to udim(Mat currentCase$)
					if caseIndex>1 then
						string$=string$&" or "
					end if
					string$=string$&trim$(currentSelect$)&"="&trim$(currentCase$(caseIndex))
				next caseIndex
				string$=string$&afterString$
			end if
		else if (casePosition:=pos(lwrc$(string$),"#case#")) then
			if len(trim$(currentSelect$)) then
					caseIndex=0
					currentCaseChunk=casePosition+6
					do
						caseIndex+=1
						mat currentCase$(caseIndex)
						if (nextCaseChunk:=pos(string$,"#",currentCaseChunk)) then
								currentCase$(caseIndex)=string$(currentCaseChunk:nextCaseChunk-1)
								currentCaseChunk=nextCaseChunk+1
						else
								currentCase$(caseIndex)=string$(currentCaseChunk:len(string$))
						end if
					loop while nextCaseChunk
					afterString$=" then  ! "&string$(casePosition:len(string$))
					string$=string$(1:casePosition-1)&"else if "
					for caseIndex=1 to udim(Mat currentCase$)
						if caseIndex>1 then
								string$=string$&" or "
						end if
						string$=string$&trim$(currentSelect$)&"="&trim$(currentCase$(caseIndex))
					next caseIndex
					string$=string$&afterString$
			end if
		else if (casePosition:=pos(lwrc$(string$),"#case else#")) then
			if len(trim$(currentSelect$)) then
					string$=string$(1:casePosition-1)&"else "&string$(casePosition+11:len(string$))&" ! "&string$(casePosition:len(string$))
			end if
		else if (Endposition:=pos(lwrc$(string$),"#end select#")) then
			string$=string$(1:EndPosition-1)&"end if"&string$(EndPosition+12:len(string$))&"  ! "&string$(EndPosition:len(string$))
			currentSelect$=""
		end if
		if (newNumber:=pos(lwrc$(string$),"#autonumber#")) then
			temp=0
			temp=val(string$(newNumber+12:newIncrement:=pos(string$,",",newNumber+12))) conv LexiBadAutoNumber
			if temp=0 then goto LexiBadAutoNumber
			newLineCount=temp
			if newLineCount<=lineCount then print "autonumber error in "&Str$(lastLineCount)&" to "&Str$(newLineCount)&" autonumber section" : close #1: : close #2: : execute ("*Free "&lextFileOut$) : print Bell : pause : execute ("System")
			lastLineCount=lineCount=newLineCount
			increment=val(string$(newIncrement+1:4000)) conv LexiBadAutoNumber
			lineCount-=increment ! Decrement So Next increment Is Correct
		end if
		if (ltrm$(lwrc$(string$))(1:1)="l") and (newNumber:=pos(ltrm$(string$)(1:7),":")) then
			newLineCount=val(ltrm$(string$)(2:newNumber-1)) conv LexiBadAutoNumber
			if (newLineCount>lineCount) then
					lineCount=newLineCount
					if mod(lineCount,labelIncrement)=0 then increment=labelIncrement
					lineCount-=increment ! Decrement So Next Num Is Correct
			else
					increment=max(int(increment/2),2) ! Cut Incr In Half To Catch Up
			end if
		end if
		LexiBadAutoNumber: ! Ignore Line Number Information
		x=0
		x=val(string$(1:5)) conv LexiAddLineNumber
		if x>0 then goto LexiPrintLine
		LexiAddLineNumber: !
		if ~skipNextOne then
			if trim$(string$)="" then
				string$=cnvrt$("pic(#####)",(lineCount:=lineCount+increment))&"  !"
			else
				string$=cnvrt$("pic(#####)",(lineCount:=lineCount+increment))&" "&string$
			end if
		else
			string$="      "&string$
			skipNextOne=0
		end if
		LexiPrintLine: !
		if trim$(string$)(len(trim$(string$))-1:len(trim$(string$)))="!:" then skipNextOne=1
		print #2: string$
	loop
	LexiDoneReading: !
	close #2:
	close #1:
fnend
def fn_hasLineNumbers(filename$*256)
	filename$=trim$(filename$)
	ext$=filename$(pos(filename$,'.',-1):len(filename$)) !  get file identifier
	if lwrc$(ext$)='.brs' or lwrc$(ext$)='.wbs' then
		open #hTmp:=3: 'name='&filename$,display,input
		dim hlnLine$*2048
		linput #hTmp: hlnLine$ eof HlnEof
		close #hTmp:
		
		hlnTest1=0
		hlnTest1=val(hlnLine$(1:5)) conv ignore
		if hlnTest1>0 then goto HlnYesLineNumbers
		
		hlnPosSpace=pos(hlnLine$,' ')
		if hlnPosSpace>0 then
			hlnTest1=val(hlnLine$(1:hlnPosSpace-1)) conv ignore
			if hlnTest1>0 then goto HlnYesLineNumbers
		end if
		
		goto HlnNoLineNumbers
		HlnYesLineNumbers: !
			hasLineNumbersReturn=1
		goto HlnFinis
		HlnNoLineNumbers: !
			hasLineNumbersReturn=0
		goto HlnFinis
		HlnEof: !
		pr bell;'HlnEof file analyzed (filename$='&filename$&') had no lines (EoF).'
		pause
		goto HlnFinis
	end if
		HlnFinis: !
	fn_hasLineNumbers=hasLineNumbersReturn
fnend
def library fnCheckCompiled(; ___,filename$*256)
	autoLibrary
	if env$('acsEnableComplier')='Yes' then

		! pr 'entered into fncheckcompiled' : pause
		execute 'CD S:'
		if ~exists('S:\(import)') then execute 'mkdir S:\(import)'
		execute 'sy -M '&os_filename$('S:\sortfiles.exe')&' -D . -C ".br.brs|.br"' ioerr CcDone

		open #hBrsFileList:=fngethandle: "Name=S:\(import)\brsfiles",display,input ioerr CC_ERR
		linput #hBrsFileList: filename$ eof CcDone
		! pr 'there is at least one source file to be recompiled.' : pr  filename$ : pause
		close #hBrsFileList:
		if env$('compile_without_asking')='Yes' then
			docompile=2
			setenv('compile_without_asking','')
		else
			docompile=msgbox("You have uncompiled source files!  Recompile?", "ACS 5 - "&os_filename$(program$), "Yn", "Qst")
		end if


		! if fnFileContainsMultipleEntries then
		! pause
		! end if


		if docompile=2 then
			setenv("AfterRecompile", "S:\Core\Start")
			chain 'S:\Core\Compile.br' ! execute "Proc S:\ReCompile.prc" ioerr ignore
		end if
		CcDone: !

	end if
fnend
CC_ERR: ! r:
	mb_response=msgbox(program$&' encountered an error '&str$(err) _
	&' on line '&str$(line)&'.'&chr$(13)&'Close ACS?'&chr$(13)& _
	'(Choose Cancel for developer pause.)','ACS 5 - S:\Core\CheckCompiled - Error' _
	,'OKc','Excl')
	if mb_response=1 then execute 'system'
	pause
retry  ! /r