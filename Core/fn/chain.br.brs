! replace S:\Core\chain.br
 
def library fnChain(prg$*255; no_fnprg_setting,noLog)
	autoLibrary
	on error goto Ertn
 
	dim cap$*128,mss$(4)*128
	dim path$*256,prog$*256
 
	fnGetPp(prg$,path$,prog$,ext$)
	if ext$='' then ext$='.br'
	! display menu : '','',''
	! pr 'fnChain to '&PATH$&PROG$
	prg$=path$&prog$
	if ~no_fnprg_setting then fnprg(prg$,put=2)
	Test: !
	resp$=''
	if exists(path$&prog$&ext$)=0 then
		mss$(1)='The program ('&path$&prog$&ext$&') could not be found.'
		mss$(2)=''
		mss$(3)='Retry will look again.'
		mss$(4)='Cancel will return you to the Menu.'
		fnmsgbox(mat mss$,resp$,cap$,21)
	end if 
	if resp$='Cancel' then goto Xit else if resp$='Retry' then goto Test
	if ~noLog then fnlog('fnChain to '&prg$,1)
	chain prg$
	Xit: !
	chain 'S:\Core\Menu.br' ! fnXit
fnend
include: Ertn
