on erorr goto ErrorHandler
library env$('pandaPath')&'\BambooForest.br': fnAddOneC,fnAddOneN,fnGetHandle,fnGetPp,fnOpenInAndOut,fnCloseInAndOut,fnStatusClose,fnStatus,fnHasInitialScan,fnStatusPause,fnhas,fnReadFileIntoArray
pr ' in plugin: Include '
pr "  fnHas('[lineStartsWith]'&env$('keyword-include'))  returns  "&str$(fnHas('[lineStartsWith]'&env$('keyword-include')))
! if fnHas('[lineStartsWith]'&env$('keyword-include')) t
  fn_applyLineContinuation(env$('pandaSourceFile')) ! process any include: [filename] lines (returns same file passed)
! en if
en
 
en
def fn_applyLineContinuation(fileName$*256)
	! env$('path-include') - if the (include:) file included can't be found the include routine will attempt to prepend( append this to the beginning of) the parsed filename and try again.
	! skipNextOne - means the prior one ended with an (exclamation mark)(colon)
	includedFileCount=0
	do
		imbededincludeCount=0
		fnOpenInAndOut(fileName$,hIn,hOut, 'applyInclude')
		! pau
		dim line$*4000
		lineCount=0
		do
			lin #hIn: line$ eof EoAiIn
			! if trim$(line$)='!pausenow' then pr line$ : pause
			lineCount+=1
			if lastLineContinued then line$=trim$(line$)
			lineLastChr=len(rtrm$(line$))
			if line$(lineLastChr-1:lineLastChr)=' _' t
				pr #hOut: line$(1:lineLastChr-1);   !  this leaves the space - this is sometimes necessary  I don't think it ever hurts
				lastLineContinued+=1
			else
				if lastLineContinued then
					pr #hOut: trim$(line$) ! the very end of the line
					for x=1 to lastLineContinued
						pr #hOut: 
					nex x
					lastLineContinued=0
				else
					! pr 'ccc'&gline$ : pause
					pr #hOut: line$ ! every regular line
				en if
			en if
		loop
		EoAiIn: !
		pr #hOut:
		cl #hOut:
		cl #hIn,free:
	loop while imbededIncludeCount
	fnStatus('Line Continustions applied.')
fn
 
ErrorHandler: ! r:
	! exec 'list -'&str$(line)
	pr bell;'~o~   ~o~   ~o~   ~o~   ~o~   ~o~   ~o~   ~o~   Error   ~o~   ~o~   ~o~   ~o~   ~o~   ~o~   ~o~   ~o~'
	exe 'st st'
	pr '        program: '&program$
	pr '      variable$: '&variable$
	pr '          error: '&str$(err)
	pr '          line: '&str$(line)
	exec 'list '&str$(line)
	pr bell;'~o~   ~o~   ~o~   ~o~   ~o~   ~o~   ~o~   ~o~   o-_-o   ~o~   ~o~   ~o~   ~o~   ~o~   ~o~   ~o~   ~o~'
	pau
retry ! /r
