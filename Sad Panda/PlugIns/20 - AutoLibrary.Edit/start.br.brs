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
	dim fmLibrary$(0)*32
	mat fmLibrary$(0)
	for it=1 to udim(mat item$)
		if item$(it)(1:2)='fn' and item$(it)(3:3)<>'_' and (item$(it)<>'fn' and item$(it)<>'fne' and item$(it)<>'fnen' and item$(it)<>'fnend') th
			if it>1 and item$(it-1)='def' then goto Gfc_PastAdd
			if it>2 and item$(it-1)='library' then
				fnAddOneC(mat fmLibrary$,item$(it), 0,1)
				goto Gfc_PastAdd
			end if
				fnAddOneC(mat fmAll$,item$(it), 0,1)
				returnN+=1
			Gfc_PastAdd: !
		en if
	nex it
	for li=1 to udim(mat fmLibrary$)
		liWhich=fn_srch_case_insensitive(mat fmAll$,fmLibrary$(li))
		if liWhich>0 then
			dim fmNew$(0)*32
			liLen=udim(mat fmAll$)-1
			fn_arrayItemRemoveC(mat fmAll$,liWhich)
		end if
	nex li
	fn_gatherFnCalls=returnN
fn
def fn_arrayItemRemoveC(mat array$,itemToRemove)
	if itemToRemove=udim(mat array$) then
		mat array$(itemToRemove-1)
	else
		mat array$(itemToRemove:udim(mat array$)-1)=array$(itemToRemove+1:udim(mat array$))
		mat array$(udim(mat array$)-1)
	end if
fnend
def fn_srch_case_insensitive(mat srch_array$,srch_for$*256; srch_start_ele)
  srch_array_count=udim(mat srch_array$)
  srch_return=0
  do
    srch_found=srch(mat srch_array$,'^'&srch_for$,srch_start_ele)
    if srch_found>0 and lwrc$(srch_for$)=lwrc$(srch_array$(srch_found)) then
      srch_return=srch_found
    else if srch_found>0 then
      srch_start_ele=srch_found+1
    else if srch_found<=0 then ! it's not there, anywhere - get outta here.
      srch_start_ele=srch_array_count+1
    end if
  loop until srch_start_ele>srch_array_count or srch_return
  fn_srch_case_insensitive=srch_return
fnend
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
