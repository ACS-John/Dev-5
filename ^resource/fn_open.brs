! r: doNotInclude
	pr 'This clip is not intended to be compiled directly nor run directly.'
	pr 'This clip replaces "include: fn_open" when processed with lexi.'
	pr 'The area inside the doNotInsert region could be used for testing or documentation.'
	end
! /r doNotInclude
! openDataN_open (supressprompt:=2)

dim form$(0)*2048

def fn_openFio(table$*255,mat openData$, mat openDataN; inputOnly,indexNumber)
	fn_openFio=fn_open(table$,mat openData$,mat openDataN,mat form$, inputOnly,indexNumber)
fnend

def fn_open(openTableName$*255, mat openData$, mat openDataN, mat openForm$; openInputOnly,openKeyNumber,openDisableEnumSort,openPath$*255,mat openDescription$,mat openFieldWidth, ___,openLetEnumItem,openReturn,openSupressPrompt)
	library 'S:\Core\Library': fnFioOpenFile
	dim openLetEnum$(0)*800
	dim openEnumLoaded$(0)*32
	openSupressPrompt=2
	openReturn=fnFioOpenFile(openTableName$,mat openData$,mat openDataN,mat openForm$, openInputOnly,openKeyNumber,openDisableEnumSort,openPath$,mat openDescription$,mat openFieldWidth,mat openLetEnum$,openSupressPrompt)
	openTableName$=lwrc$(openTableName$)
	if srch(mat openEnumLoaded$,openTableName$)<=0 then
		mat openEnumLoaded$(udim(mat openEnumLoaded$)+1)
		openEnumLoaded$(udim(mat openEnumLoaded$))=openTableName$
		for openLetEnumItem=1 to udim(mat openLetEnum$)
			exe openLetEnum$(openLetEnumItem)
		nex openLetEnumItem
	en if
	fn_open=openReturn
fn
