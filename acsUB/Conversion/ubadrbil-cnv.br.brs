! Replace S:\acsUB\conversion\ubadrbil-cnv
! converts adrbil as well as ask cno and write it so other conversion programs don't have to ask it.
def library fnub_cnv_adrbil
	autoLibrary
	on error goto Ertn
	fnStatus('Converting Alternate Billing Address file (S:\acsUB\conversion\ubadrbil-cnv)')
	dim a$(4)*30,cap$*128,z$*10,ab$(4)*30
 
	
 
	if ~exists("[Q]\UBmstr\UBAdrBil.h[cno]") then ! move old files to "env$('temp')&"\temp."&session$&"orary file before creating new file with same name
		fnCopy("[Q]\UBmstr\UBAdrBil.h[cno]",env$('temp')&"\temp."&session$)
	end if
	if exists("[Q]\UBmstr\UBAdrBil.h[cno]")=0 then goto BUILD_FILES
	open #3: "Name=[Q]\UBmstr\ubMaster.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed ioerr OPN_CUST
	goto L160
	
	OPN_CUST: !
		open #3: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed
	goto L160
	
	L160: !
	open #1: "Name="&env$('temp')&"\temp."&session$&"",internal,input,relative
	open #2: "Name="&env$('temp')&"\tmp_x_"&session$&",RecL=130,Replace",internal,outIn,relative
	L180: !
		read #3,using "form pos 1,c 10,pos 385,pd 3": z$,bra eof END1
	if bra=0 then goto L180
	if rln(1)=100 then goto L210 else goto L230
	L210: !
		! note!!!!   may have to change a$(3) to a$(4), etc and form on next line
		read #1,using 'Form POS 11,3*C 30',rec=bra: a$(1),a$(2),a$(3) noRec L180
	goto L240
	L230: !
	read #1,using 'Form POS 11,4*C 30',rec=bra: mat a$ noRec L180
	L240: !
		if trim$(a$(1))="" and trim$(a$(2))="" and trim$(a$(3))="" and trim$(a$(4))="" then goto L180
		write #2,using 'Form POS 1,C 10,4*C 30,': z$,mat a$
	goto L180
 
	END1: !
		close #1:
		close #2:
		fnCopy(env$('temp')&"\tmp_x_"&session$,"[Q]\UBmstr\UBAdrBil.h[cno]")
		if ~fnIndex("[Q]\UBmstr\UBAdrBil.h[cno]","[Q]\UBmstr\adrIndex.h[cno]","1 10") then goto Xit ! ,Replace,DupKeys -n" ioerr Xit
		execute "Free "&env$('temp')&"\temp."&session$
		close #3: ioerr ignore
		execute "Free [Q]\UBmstr\ubMaster.h[cno]" ioerr ignore
	goto Xit
	BUILD_FILES: ! build files if don't exist
		open #2: "Name=[Q]\UBmstr\UBAdrBil.h[cno],RecL=130,Replace",internal,outIn,relative
		close #2:
		fnIndex("[Q]\UBmstr\UBAdrBil.h[cno]","[Q]\UBmstr\adrIndex.h[cno]","1 10") ! execute "Index [Q]\UBmstr\UBAdrBil.h[cno]"&' '&"[Q]\UBmstr\adrIndex.h[cno] 1,10,Replace,DupKeys -n"
	return
Xit: fnend
include: Ertn
