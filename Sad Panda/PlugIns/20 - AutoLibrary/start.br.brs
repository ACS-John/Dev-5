on erorr goto ErrorHandler
library env$('pandaPath')&'\BambooForest.br': fnHas
library env$('pandaPath')&'\BambooForest.br': fnOpenInAndOut,fnCloseInAndOut
library env$('pandaPath')&'\BambooForest.br': fnStripComments$
library env$('pandaPath')&'\BambooForest.br': fnAddOneC
library env$('pandaPath')&'\BambooForest.br': fnStatus,fnStatusPause,fnStatusClose
pr ' in plugin: autoLibrary '
has=fnHas('autoLibrary')
pr 'passed fnHas returned ';has;' (fnhas does not work so it''s logic is disabled here)' ! pause
! if has t
	! fnStatus('Applying autoLibrary.')
  fn_applyAutoLibrary(env$('pandaSourceFile')) ! process any autoLibrary lines (returns same file passed)
! en if
en
 
en
def fn_applyAutoLibrary(fileName$*256)
	autoLibrariedFileCount=0
	dim line$*4000
	dim countAddGosub
	countAddGosub=0
	imbededautoLibraryCount=0
	dim fmAll$(0)*32
	mat fmAll$(0)
	fnOpenInAndOut(fileName$,hIn,hOut, 'applyAutoLibrary')
	do
		lin #hIn: line$ eof EoAiIn
		if srep$(srep$(line$,chr$(9),''),' ','')(1:11)='autoLibrary' then ! is an autoLibrary trigger line
			autoLibrariedFileCount+=1
			line$='gosub AutoLibrary ! Sad Panda ('&trim$(line$)&')'
			countAddGosub+=1
		else
			if fn_gatherFnCalls(line$,mat fmAll$) th

			en if
		end if
		pr #hOut: line$
	loop
	EoAiIn: !
	if countAddGosub>0 then
		
		! pr '*pr mat fmAll$*'
		! pr mat fmAll$
		! pr 'count=';udim(mat fmAll$)
		! pause
		
		pr #hOut: 'AutoLibrary: ! r: region dynamically built in Sad Panda'
		for item=1 to udim(mat fmAll$)
			pr #hOut: '  library "'&env$('path-autoLibrary')&'": '&fmAll$(item)
		nex item
		pr #hOut: 'return ! /r'
	end if
	fnCloseInAndOut(hIn,hOut)
fn
def fn_gatherFnCalls(line$*4000,mat fmAll$; ___,returnN)
	dim item$(0)*256
	line$=fnStripComments$(line$)
	! if pos(line$,'_')>0 and pos(line$,'fn_')<=0 th 
	! 	pr '_ encountered: '&line$ 
	! 	pause
	! en if
	line$=xlate$(line$,rpt$(' ', 36)&'$           0123456789       abcdefghijklmnopqrstuvwxyz    _ abcdefghijklmnopqrstuvwxyz'&rpt$(' ', 133))
	do 
		line$=srep$(line$,'  ',' ')
	loop until pos(line$,'  ')<=0
	str2mat(line$,mat item$,' ')
	for it=1 to udim(mat item$)
		if item$(it)(1:2)='fn' and item$(it)(3:3)<>'_' and (item$(it)<>'fn' and item$(it)<>'fne' and item$(it)<>'fnen' and item$(it)<>'fnend') th
			fnAddOneC(mat fmAll$,item$(it), 0,1)
			returnN+=1
		en if
	nex it
	fn_gatherFnCalls=returnN
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
