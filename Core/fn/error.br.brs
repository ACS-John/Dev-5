autoLibrary
on error goto Ertn
pr 'test fnError'
gos SubB
end
SubA: ! r:
	aa+=1
	pr 'SubA';aa
	if aa<5 th gos SubB
return ! /r
SubB: ! r:
	bb+=1
	pr 'SubB';bb
	if bb<5 th
		gos SubA
	else
		xx=1/0
		end
	en if
return ! /r



Xit: fnXit

def library fnError(callingprogram$*256, errornumber, linenumber, &ertnAct$, stopable$; ___,sourceFile$*300,line$*512)
	autoLibrary
	on error goto NEVER
	xcnt=cnt

	! ! r: ertnAct$ Size Test  ( sets enableBigErtnAct=0 or 1 )
	! enableBigErtnAct=0 ! ertnAct$ is dimmed less than 256
	! dim ertnActHold$*256
	! pause
	! ertnActHold$=ertnAct$
	! ertnAct$=rpt$(' ',256) soflow ErtnActSizeTestFinis
	! enableBigErtnAct=1 ! ertnAct$ is dimmed at least 256
	! ErtnActSizeTestFinis: !
	! ertnAct$=ertnActHold$
	! ! /r

	dim response$(5)*80
	! ertnAct$  =  the returning action to be executed
	! stopable$ =  if it is 'no' or 'NO' etc it'll not allow them to stop
	!              otherwise it should be the paragraph heading of where to goto on
	!              a selected exit (must be 15 characters or less)

	!   pr border: 'Error '&str$(errornumber)&' on line '&str$(linenumber)
	pr fields '1,73,C 7,N': bell$
	if callingprogram$=uprC$(callingprogram$) then callingprogram$=lwrc$(callingprogram$)
	fnLog('Error '&str$(errornumber)&' at line '&str$(linenumber)&' of '&callingprogram$(1:100),2)
	ertnAct$='Go '&str$(linenumber)
	! dim acshelp$*200
	! acshelp$='Help\co\error\br\Err'&cnvrt$('Pic(####)',errornumber)&'.html'
	MENU1: !
		! r: draw screen
		open #win=fnH: 'SRow=3,SCol=4,Rows=19,Cols=72,Border=S:[screen],N=[screen],Caption=<Error',display,outin  ! ,border=Sr   ...
		if errornumber=61 then
			pr #win,f '1,1,Cc 69,N': 'A record is needed but is locked by another user.'
			pr #win,f '2,1,Cc 69,N': 'Please ask other users to return to the Menu and'
			pr #win,f '3,1,Cc 69,N': 'then you may select Retry.'
		else if errornumber=4148 then
			pr #win,f '1,1,Cc 69,N': 'A file lock is needed but the file is already locked by someone else.'
			pr #win,f '2,1,Cc 69,N': 'Please ask other users to return to the Menu and'
			pr #win,f '3,1,Cc 69,N': 'then you may select Retry.'
		else if errornumber=632 then
			pr #win,f '1,1,Cc 69,N': 'Problem with Index File'
		end if

		pr #win,f ' 7, 2,Cr 13,N': 'Program:'
		pr #win,f ' 7,16,Cl 53,P[textboxes]': env$('Core_Program_Current')(1:53)
		pr #win,f ' 8, 2,Cr 13,N': 'File:'
		pr #win,f ' 8,16,Cl 53,P[textboxes]': callingprogram$(1:53)
		pr #win,f '10, 2,Cr 13,[screen]': 'Error:'
		pr #win,f '10,16,Cl 17,,B08': str$(errornumber)&'  (F8 BRWiki)'
		pr #win,f '12, 2,Cr 13,[screen]': 'Line:'
		if env$('acsDeveloper')<>'' then
		  pr #win,f '12,16,Cl 17,,B11': str$(linenumber)&' (F11 N++)'
		else
			pr #win,f '12,16,Cl 5,P[textboxes]': str$(linenumber)
		end if



		pr #win,f '13, 2,Cr 13,N': 'Count+1:'
		pr #win,f '13,16,C 5,P[textboxes]': str$(xcnt+1)
		pr #win,f '14, 2,Cr 13,N': 'Session:'
		pr #win,f '14,16,C 5,P[textboxes]': session$
		button_pos$='47'
		pr #win,f '10,47,Cc 22,,B12': 'Command Console (F12)' ! 1,19,12/CC 12,,B1000
		pr #win,f '11,47,Cc 22,,B09': 'Stacks           (F9)'
	!   pr #win,f '6,47,C 22,B,10': 'WB Help (F10)'
		! pr #win,f '11,47,Cc 22,,B08': 'BRWiki Help (F8)'
	!   if exists(acshelp$)<>0 then
	!     pr #win,f '12,47,Cc 22,,B09': 'ACS Help (F19)'
	!   end if
		pr #win,f '13,47,Cc 22,,B01': 'Retry (Enter)'
		pr #win,f '14,47,Cc 22,,B99': 'Exit (Esc)'


		if env$('acsDeveloper')<>'' then ! enableBigErtnAct and
		  ! pr #win,f '16,20,Cc 35,,B11': 'N++ .brs  on Line  (F11)'
			pr #win,f '17,20,Cc 35,,B21': 'Recompile, Reload and Run (Ctrl+F1)'
			pr #win,f '18,20,Cc 35,,B22': 'Reload and Run            (Ctrl+F2)'
			! pr #win,f '19,20,Cc 35,,B23': 'Edit (Ctrl+F3)'
			! pr #win,f '16,18,Cc 38,,B120': 'Recompile, Reload and Run (Ctrl+Alt+1)'
		end if
		! /r


	ERR_INP: !
		in #0,f '4,4,C 1,AE,N': pause$
		if cmdkey=0 then
			fnLog('action taken = Retry',2)
			ertnAct$='Go '&str$(linenumber)
			goto ERROR_XIT
		else if cmdkey=5 or cmdkey=99 then
			fnLog('action taken = Quit',2)
			goto ERR_QUITER
		else if cmdkey=8 then
			fnLog('action taken = BRWiki Help',2)
			gosub BRWIKIHELP

		else if cmdkey=9 then

			ShowStacks: ! ! r:
			! dim stackColumn$(6)*128
			dim stackColumn$(5)*128
			stackColumn$(1)='sequence'
			stackColumn$(2)='line'
			stackColumn$(3)='clause'
			stackColumn$(4)='container'
			stackColumn$(5)='program file'
			! stackColumn$(6)='origional line'
			mat clientSelectMask$(udim(mat stackColumn$))
			mat clientSelectMask$=('')
			clientSelectMask$(1)='20'  ! 0 decimals, commas
			clientSelectMask$(2)='20'
			clientSelectMask$(3)='81' ! cr
			fnFlexInit1('stacks',2,1,10,10,mat stackColumn$,mat clientSelectMask$)

			! r: read stacks into array

			execute 'St st >"[temp]\stacks[session].tmp"'
			open #hStacks=fnH: 'Name=[temp]\stacks[session].tmp',display,input
			line_count=stackCount=0
			do
				dim tmp$*128
				linput #hStacks: tmp$ eof EO_hStacks
				line_count+=1

				if tmp$(11:14)='line' then
					stackColumn$(1)=str$(stackCount+=1)
					! stackColumn$(6)=tmp$
					tmp$=rtrm$(tmp$)
					lineLen=len(tmp$)
					stackColumn$(2)=tmp$(16:20)
					stackColumn$(3)=tmp$(22:23)
					posLastSpace=pos(rtrm$(tmp$),' ',-1)
					posPentultimateSpace=pos(tmp$(1:posLastSpace-1),' ',-1)
					posPentultimateSpace2=pos(tmp$(1:posPentultimateSpace-1),' ',-1)
					stackColumn$(4)=tmp$(34:posPentultimateSpace2)
					posInFunction=pos(tmp$,'in function')
					if posInFunction then
						stackColumn$(4)=tmp$(posInFunction+12:inf)
					else if pos(tmp$,'in a GOSUB routine')>0 then
						stackColumn$(4)='gosub'
					else
						stackColumn$(4)='origin'
					end if
					tmp$(1:33)=''
					tmp$=srep$(tmp$,' in a GOSUB routine','')
					tmp$=srep$(tmp$,' in function','')
					posLastSpace=pos(rtrm$(tmp$),' ',-1)
					if posLastSpace>0 then
						tmp$(posLastSpace:inf)=''
					end if
					! pr tmp$ : pause
					stackColumn$(5)=tmp$
					! pr 'tmp$='&tmp$
					! pr 'lineLen='&str$(lineLen)
					! pr 'posLastSpace='&str$(posLastSpace)
					! pr 'posPentultimateSpace='&str$(posPentultimateSpace)
					! pr 'posPentultimateSpace2='&str$(posPentultimateSpace2)
					! pr 'program='&stackColumn$(4)
					! pr 'container='&stackColumn$(5)
					! pause
										
					dim col1$(0)*256
					dim col2$(0)*256
					dim col3$(0)*256
					dim col4$(0)*256
					dim col5$(0)*256
					dim col6$(0)*256
					mat col1$(stackCount) : col1$(stackCount)=stackColumn$(1)
					mat col2$(stackCount) : col2$(stackCount)=stackColumn$(2)
					mat col3$(stackCount) : col3$(stackCount)=stackColumn$(3)
					mat col4$(stackCount) : col4$(stackCount)=stackColumn$(4)
					mat col5$(stackCount) : col5$(stackCount)=stackColumn$(5)
					! mat col6$(stackCount) : col6$(stackCount)=stackColumn$(6)

					fnFlexAdd1(mat stackColumn$)
				end if
			loop
			EO_hStacks: !
			close #hStacks:
			! /r
			fnCmdKey('Edit',1,1)
			fnCmdKey('Back',5,0,1)
			dim resp$(40)*256
			ckey=fnAcs(mat resp$)
			if ckey=5 then ! r:
				setenv('current_grid_row','')
				goto MENU1 ! /r
			else if ckey=1 then ! r:
				! pr mat resp$ : pause
				dim selectedFile$*256
				whichLine=val(resp$(1))
				setenv('current_grid_row',resp$(1))
				selectedLine=val(col2$(whichLine))
				selectedFile$=col5$(whichLine)
				selectedFile$=srep$(os_filename$(selectedFile$),'F:\CLSINC\','C:\ACS\Dev-5\')
				selectedFile$=srep$(selectedFile$,'COLLECTION-MASTER ADD-ON','Collection-Master Add-On')
				! exe 'dir "'&selectedFile$&'" -l -b >acsErrTmp[session].txt'
				! open #hTmp=fnH: 'name=acsErrTmp[session].txt',display,input
				! linput #hTmp: line$ !  consume "Directory of" line
				! linput #hTmp: line$ !  read line that contains selectedFile$
				! ! pr 'line$=';line$ : pause
				! selectedFile$=srep$(selectedFile$,uprc$(line$(1:len(line$))),line$(1:len(line$)))
				selectedFile$&='.brs'
				! pr 'selectedFile$=';selectedFile$ : pause
				! close #hTmp:
				! exe '*Free acsErrTmp[session].txt -n'
				! execute '"'&os_filename$('S:\brEdit.cmd')&'"'
				execute 'sy ""C:\ACS\Program\Notepad++\notepad++.exe" "'&selectedFile$&'" -n'&str$(selectedLine)&'"'

			
			
			
				goto ShowStacks
			end if
			! /r
		! else if cmdkey=10 then ! r:
		! 	fnLog('action taken = WB Help',2)
		! 	gosub ERR_WBHELP
		! /r
		else if cmdkey=12 then  ! r:
			ertnAct$='PAUSE'
			fnLog('action taken = Program Pause',2)
			goto ERROR_XIT ! /r
		else if fkey=11 then ! r: goto that line in n++ (source file)
			sourceFile$=srep$(os_filename$(callingprogram$),'F:\CLSINC\','C:\ACS\Dev-5\')
			sourceFile$=srep$(sourceFile$,'COLLECTION-MASTER ADD-ON','Collection-Master Add-On')
			exe 'dir "'&sourceFile$&'" -l -b >acsErrTmp[session].txt'
			open #hTmp=fnH: 'name=acsErrTmp[session].txt',display,input
			linput #hTmp: line$ !  consume "Directory of" line
			linput #hTmp: line$ !  read line that contains sourceFile$
			! pr 'line$=';line$ : pause
			sourceFile$=srep$(sourceFile$,uprc$(line$(1:len(line$))),line$(1:len(line$)))
			sourceFile$&='.brs'
			! pr 'sourceFile$=';sourceFile$ : pause
			close #hTmp:
			exe '*Free acsErrTmp[session].txt -n'
			execute '"'&os_filename$('S:\brEdit.cmd')&'"'
			execute 'sy ""C:\ACS\Program\Notepad++\notepad++.exe" "'&sourceFile$&'" -n'&str$(linenumber)&'"'
			! /r
		! else if cmdkey=19 then ! r:
		! 	fnLog('action taken = ACS Help',2)
		! 	gosub ACSHELP
		! /r
		else if cmdkey=21 or fkey=120 then ! r:
			ertnAct$='Proc r3.prc'
			fnLog('action taken = '&ertnAct$,2)
			if ~exists('in') then
				fnWriteProc('in','end')
				fnWriteProc(''  ,"setenv('source',program$&program$(pos(program$,'.',-1):inf)&'s')")
				fnWriteProc(''  ,"setenv('source',os_filename$(env$('source')))")
				fnWriteProc(''  ,"exec 'sy """"C:\ACS\Dev-5\Sad Panda\Compile.cmd"" ""'&env$('source')&'""""'")
				fnWriteProc(''  ,'execute ''load "''&program$&''"''')
			end if
			! fnWriteProc('Recompile Reload and Run.prc','subproc in')
			fnWriteProc('r3.prc','subproc in')
			fnWriteProc(''      ,'run')
			goto ERROR_XIT
			! /r
		else if cmdkey=22 then ! r:
			ertnAct$='Proc rr'
			fnLog('action taken = '&ertnAct$,2)
			fnWriteProc('rr','end')
			fnWriteProc(''      ,'execute ''load "''&program$&''"''')
			fnWriteProc(''      ,'run')
			goto ERROR_XIT
			! /r
		! else if cmdkey=23 then ! r:
		! 	ertnAct$='Proc errEdit.prc'
		! 	fnLog('action taken = '&ertnAct$,2)
		! 	fnWriteProc('errEdit' ,'end')
		! 	fnWriteProc(''   ,"exec 'sy "&os_filename$('S:\brEdit.cmd')&' "''&os_filename$(program$)&''"''')
		! 	goto ERROR_XIT
		! /r
		end if
	goto ERR_INP

	! ERR_WBHELP: ! r:
	!     on error goto NO_ERR_WBHELP
	!     help$('ERR'&cnvrt$('PIC(####)',errornumber)&',WBCmd')
	!     goto ERR_WBHELP_RETURN ! /r
	! NO_ERR_WBHELP: ! r:
	!     mat msgline$(2)
	!     msgline$(1)='Sorry, No Workstation Basic Help '
	!     msgline$(2)='is available for this Error Number.'
	!     fnMsgBox(mat msgline$,response$(1),env$('program_caption'),0)
	!     response$(1)=response$(1)(1:1)
	! ERR_WBHELP_RETURN: !
	!     return  ! /r
	BRWIKIHELP: ! r:
		execute 'Sy -c -m Start /MIN http://brwiki2.brulescorp.com/index.php?search='&cnvrt$('pic(####)',errornumber)
	return  ! /r
! ACSHELP: ! r:
!     if exists(acshelp$)<>0 then
!       execute 'Sy -c -m '&acshelp$
!     end if
!     return  ! /r
	ERR_QUITER: ! r:
		if uprc$(rtrm$(stopable$))='NO' then
			setenv('ExitNow','no')
			dim msgline$(0)*128
			mat msgline$(2)
			msgline$(1)='Do not stop the processing of this program.'
			msgline$(2)='Please contact ACS Technical Support.'
			fnMsgBox(mat msgline$,response$(1),env$('program_caption'),0)
			response$(1)=response$(1)(1:1)
			goto MENU1
		end if
		ertnAct$='go '&stopable$
	goto ERROR_XIT ! /r
	NEVER: ! r:
		pr 'The Error routine had an error!'
		pr 'error '&str$(err)&' on line '&str$(line)
		pr 'Please call ACS support.'
		pause  ! input fields '1,1,C 1,AEN': pause$
	retry  ! /r
	ERROR_XIT: !
		if file(win)<>-1 then close #win:
fnend

include: Ertn
