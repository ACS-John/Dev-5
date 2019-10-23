! Replace S:\Core\AddCNo
! add a new company program for [cursys] (copies it from 99999) then it chains to Company Information
! ______________________________________________________________________
library 'S:\Core\Library': fntop
library 'S:\Core\Library': fnTos,fnLbl,fnAcs2,fnTxt,fnCmdKey,fncmbcno,fnCmdSet
library 'S:\Core\Library': fnCopy
library 'S:\Core\Library': fnSystemName$
library 'S:\Core\Library': fnchain
library 'S:\Core\Library': fnmsgbox
library 'S:\Core\Library': fnindex_sys
library 'S:\Core\Library': fncreg_write
on error goto ERTN
dim ml$(10)*80,resp$(40)*128,cap$*128
! ______________________________________________________________________
fntop(program$,cap$="Add New "&env$('cursys')&" Company [cno]")
if exists('S:\'&fnSystemName$&'\mstr\*.h99999') then
	fnCopy('S:\'&fnSystemName$&'\mstr\*.h99999','[Q]\'&env$('cursys')&'mstr\*.h[cno]')
else if exists('S:\acs'&env$('cursys')&'\mstr\*.h99999') then
	fnCopy('S:\acs'&env$('cursys')&'\mstr\*.h99999','[Q]\'&env$('cursys')&'mstr\*.h[cno]')
end if
! 
if env$('cursys')='CL' then ! r:
	mat ml$(5)
	ml$(1)='Would you like to import data from an old'
	ml$(2)='ACS Accounts Payable system?'
	ml$(3)='This is only chance.'
	fnmsgbox(mat ml$,resp$,cap$,36)
	if resp$='Yes' then let fnchain("S:\acsCL\Conversion\APmstr-Cnv")
! /r
else if env$('cursys')='UB' then ! r:
	! the following block of logic is removed in favor of logic located in "S:\Utility Billing\Type of Service.br"
	! fnTos
	! mylen=32 : mypos=mylen+2 : lc=0
	! fnLbl(lc+=1,1,"Copy Service Types from Company:",mylen,1)
	! fncmbcno(lc,mypos)
	! resp$(1)=''
	! fnCmdKey("&Next",1,1,1)
	! fnAcs2(mat resp$,ckey)
	! if ck=5 then goto XIT
	! copytoscno=val(resp$(1)(43:47))
	! if copytoscno=0 then
	! 	fnCopy("S:\Utility Billing\mstr\UBData\*.h99999","[Q]\UBmstr\UBData\*.h[cno]")
	! else
	! 	fnCopy("[Q]\UBmstr\UBData\*.h"&str$(copytoscno),"[Q]\UBmstr\UBData\*.h[cno]")
	! end if
	open #1: "Name=[Q]\UBmstr\Company.h[cno],RecL=129,Replace,Shr",internal,outIn,relative 
	write #1,using "Form POS 1,C 40",rec=1: empty$
	close #1: 
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],Size=0,RecL=2067,Replace",internal,output 
	close #1: 
	open #2: "Name=[Q]\UBmstr\ubTransVB.h[cno],KFName=[Q]\UBmstr\ubTrIndx.h[cno],Replace,RecL=102,KPs=1,KLn=19",internal,outIn,keyed 
	close #2: 
	open #3: "Name=[Q]\UBmstr\ubAdrBil.h[cno],KFName=[Q]\UBmstr\AdrIndex.h[cno],Use,RecL=130,KPs=1,KLn=10",internal,outIn,keyed 
	close #3: 
	open #1: "Name=[Q]\UBmstr\Cass1.h[cno],RecL=111,Replace",internal,output 
	close #1: 
	! open #1: "Name=[Q]\UBmstr\Deposit1.h[cno],KFName=[Q]\UBmstr\DepIdx1.h[cno],Replace,RecL=16,KPs=1,KLn=10",internal,outIn,keyed 
	! close #1: 
	open #1: "Name=[Q]\UBmstr\Deposit2.h[cno],Replace,RecL=73",internal,outIn,relative 
	close #1: 
	open #1: "Name=[Q]\UBmstr\BudMstr.h[cno],KFName=[Q]\UBmstr\BudIdx1.h[cno],Replace,RecL=80,KPs=1,KLn=10",internal,outIn,keyed 
	close #1: 
	open #1: "Name=[Q]\UBmstr\BudTrans.h[cno],Replace,RecL=149",internal,outIn,relative 
	close #1: 
	open #1: "Name=[Q]\UBmstr\UBAdrBil.h[cno],KFName=[Q]\UBmstr\NoteIdx1.h[cno],Replace,RecL=130,KPs=1,KLn=10",internal,outIn,keyed 
	close #1: 
	fncreg_write('Route Low',str$(bkno1)) ! Route Number Range Low
	fncreg_write('Route High',str$(bkno2)) ! Route Number Range High
! ______________________________________________________________________
	fnindex_sys(val(env$('cno')),'UB')
! /r
else if env$('cursys')='GL' then ! r:
	dim zer(57)
MENU1: ! 
	fnTos
	mylen=37 : mypos=mylen+2
	fnLbl(1,1,"Copy Chart of Accounts from Company:",mylen,1)
	fncmbcno(1,mypos)
	fnCmdSet(2)
	fnAcs2(mat resp$,ckey)
	if ckey=5 then fro_cno=99999: goto L210 ! use company #99999 if no company to copy from
	fro_cno=val(resp$(1)(43:47))
	if fro_cno=0 then fro_cno=99999
L210: ! 
	if cno<1 or cno=fro_cno then goto MENU1
! ___________________________
	execute "Copy [Q]\GLmstr\*.h"&str$(fro_cno)&' '&"[Q]\GLmstr\*.h[cno] -n"
	open #20: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno],NoShr",internal,outIn,keyed 
	do 
		read #20,using 'Form POS 87,PD 6.2': cb eof EO_GLMSTR
		rewrite #20,using 'Form POS 81,42*PD 6.2,POS 333,2*PD 3,13*pd 6.2': mat zer
	loop 
EO_GLMSTR: close #20: 
! ___________________________
	execute "drop [Q]\GLmstr\GLTrans.H[cno]"
	open #1: "Name=[Q]\GLmstr\ACTrans.h[cno],Size=0,RecL=72,Replace,NoShr",internal,output 
	close #1: 
end if  ! /r
if exists('S:\'&fnSystemName$&'\Company.br') then
	fnchain('S:\'&fnSystemName$&'\Company.br') 
else
	fnchain('S:\acs'&env$('cursys')&'\Company')
end if

XIT: fnchain("S:\Core\Programs\Select Company")
include: Ertn
