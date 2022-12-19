! replace S:\Core\chain.br

def library fnChain(prg$*255; disableCoreProgramSetting,noLog)
	autoLibrary
	on error goto Ertn

	dim path$*256
	dim prog$*256
	fnGetPp(prg$,path$,prog$,ext$)
	if ext$='' then ext$='.br'
	! display menu : '','',''
	! pr 'fnChain to '&PATH$&PROG$
	prg$=path$&prog$
	if ~disableCoreProgramSetting then fnSetCoreProgramCurrent(prg$,put=2)
	Test: !
	resp$=''
	if exists(path$&prog$&ext$)=0 then
		dim mss$(4)*128
		mss$(1)='The program ('&path$&prog$&ext$&') could not be found.'
		mss$(2)=''
		mss$(3)='Retry will look again.'
		mss$(4)='Cancel will return you to the Menu.'
		fnMsgBox(mat mss$,resp$,'',21)
	end if
	if resp$='Cancel' then goto Xit else if resp$='Retry' then goto Test
	if ~noLog then fnLog('fnChain to '&prg$,1)
	chain prg$
	Xit: !
	chain 'S:\Core\Menu.br' ! fnXit
fnend
include: Ertn
