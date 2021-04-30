! Replace S:\Core\AddCNo
! add a new company program for [cursys] (copies it from 99999) then it chains to Company Information

autoLibrary
on error goto Ertn
dim ml$(10)*80
dim resp$(40)*128

fnTop(program$,"Add New "&env$('cursys')&" Company [cno]")
if exists('S:\'&fnSystemNameFromAbbr$&'\mstr\*.h99999') then
	fnCopy('S:\'&fnSystemNameFromAbbr$&'\mstr\*.h99999','[Q]\[cursys]mstr\*.h[cno]')
else if exists('S:\acs[cursys]\mstr\*.h99999') then
	fnCopy('S:\acs[cursys]\mstr\*.h99999','[Q]\[cursys]mstr\*.h[cno]')
end if

if env$('cursys')='CL' then ! r:
	mat ml$(5)
	ml$(1)='Would you like to import data from an old'
	ml$(2)='ACS Accounts Payable system?'
	ml$(3)='This is only chance.'
	fnmsgbox(mat ml$,resp$,'',36)
	if resp$='Yes' then fnchain("S:\acsCL\Conversion\APmstr-Cnv")
	! /r
else if env$('cursys')='UB' then ! r:
	open #1: "Name=[Q]\UBmstr\Company.h[cno],RecL=129,Replace,Shr",internal,outIn,relative
	write #1,using "Form POS 1,C 40",rec=1: empty$
	close #1:
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],Size=0,RecL=2067,Replace",internal,output
	close #1:
	open #2: "Name=[Q]\UBmstr\ubTransVB.h[cno],KFName=[Q]\UBmstr\ubTrIndx.h[cno],Replace,RecL=102,KPs=1,KLn=19",internal,outIn,keyed
	close #2:
	open #2: "Name=[Q]\UBmstr\ubTransVB.h[cno],KFName=[Q]\UBmstr\UBTrdt.h[cno],KPs=11/1,KLn=8/10",internal,outIn,keyed
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
	ckey=fnAcs(mat resp$)
	if ckey=5 then 
		fro_cno=99999 ! use company #99999 if no company to copy from
	else
		fro_cno=val(resp$(1)(43:47))
		if fro_cno=0 then fro_cno=99999
	end if
	if cno<1 or cno=fro_cno then goto MENU1

	fnCopy('[Q]\GLmstr\*.h'&str$(fro_cno),'[Q]\GLmstr\*.h[cno]')
	open #20: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno],NoShr",internal,outIn,keyed
	do
		read #20,using 'Form POS 87,PD 6.2': cb eof EO_GLMSTR
		rewrite #20,using 'Form POS 81,42*PD 6.2,POS 333,2*PD 3,13*pd 6.2': mat zer
	loop
	EO_GLMSTR: close #20:

	execute "drop [Q]\GLmstr\GLTrans.h[cno]"
	open #1: "Name=[Q]\GLmstr\ACTrans.h[cno],Size=0,RecL=72,Replace,NoShr",internal,output
	close #1:
end if  ! /r
if exists('S:\'&fnSystemNameFromAbbr$&'\Company.br') then
	fnchain('S:\'&fnSystemNameFromAbbr$&'\Company.br')
else
	fnchain('S:\acs'&env$('cursys')&'\Company')
end if
 
Xit: fnchain("S:\Core\Programs\Select Company")
include: ertn
