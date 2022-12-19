! Replace S:\Core\AddCNo
! add a new company program for [cursys] (copies it from 99999) then it chains to Company Information

autoLibrary
on error goto Ertn
dim ml$(10)*80
dim resp$(40)*128

fnTop(program$,'Add New '&env$('cursys')&' Company '&env$('cno'))
! r: copy h99999 into place
if exists('S:\[cursystem]\mstr\*.h99999') then
	fnCopy('S:\[cursystem]\mstr\*.h99999','[Q]\[cursys]mstr\*.h[cno]')
else if exists('S:\acs[cursys]\mstr\*.h99999') then
	fnCopy('S:\acs[cursys]\mstr\*.h99999','[Q]\[cursys]mstr\*.h[cno]')
end if
! /r

if env$('cursys')='CL' then ! r:
	mat ml$(5)
	ml$(1)='Would you like to import data from an old'
	ml$(2)='ACS Accounts Payable system?'
	ml$(3)='This is only chance.'
	fnMsgBox(mat ml$,resp$,'',36)
	if resp$='Yes' then fnImportCLfromAP
	! /r
else if env$('cursys')='UB' then ! r:
	open #hTmp=fnH: 'Name=[Q]\UBmstr\Company.h[cno],Replace,RecL=129',i,outIn,r
	write #hTmp,using 'form pos 1,C 40',rec=1: empty$
	close #hTmp:
	open #hTmp=fnH: 'Name=[Q]\UBmstr\Customer.h[cno],Replace,RecL=2067',i,outIn,r
	close #hTmp:
	open #hTmp=fnH: 'Name=[Q]\UBmstr\ubTransVB.h[cno],Replace,RecL=102',i,outIn,r
	close #hTmp:
	open #hTmp=fnH: 'Name=[Q]\UBmstr\ubAdrBil.h[cno],Replace,RecL=130',i,outIn,r
	close #hTmp:
	open #hTmp=fnH: 'Name=[Q]\UBmstr\Cass1.h[cno],Replace,RecL=111',i,outIn,r
	close #hTmp:
	open #hTmp=fnH: 'Name=[Q]\UBmstr\Deposit2.h[cno],Replace,RecL=73',i,outIn,r
	close #hTmp:
	fnFree('[Q]\UBmstr\BudMstr.h[cno]')  ! do not enable budget by default.  instead delete any existing old files in this company 10/6/22 John
	fnFree('[Q]\UBmstr\BudTrans.h[cno]')
	open #hTmp=fnH: 'Name=[Q]\UBmstr\UBAdrBil.h[cno],Replace,RecL=130',i,outIn,r
	close #hTmp:
	bkno1=1 : bkno2=9
	fncreg_write('Route Low',str$(bkno1)) ! Route Number Range Low
	fncreg_write('Route High',str$(bkno2)) ! Route Number Range High

	fnindex_sys(val(env$('cno')),'UB')
! /r
else if env$('cursys')='GL' then ! r:
	GlMenu1: !
	fnTos
	mylen=37 : mypos=mylen+2
	fnLbl(1,1,'Copy Chart of Accounts from Company:',mylen,1)
	fncmbcno(1,mypos)
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then 
		fro_cno=99999 ! use company #99999 if no company to copy from
	else
		fro_cno=val(resp$(1)(43:47))
		if fro_cno=0 then fro_cno=99999
	end if
	if cno<1 or cno=fro_cno then goto GlMenu1

	fnCopy('[Q]\GLmstr\*.h'&str$(fro_cno),'[Q]\GLmstr\*.h[cno]')
	open #hGlm=fnH: 'Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno],NoShr',i,outIn,k
	do
		read #hGlm,using 'form pos 87,PD 6.2': cb eof EO_GLMSTR
		dim zer(57)
		rewrite #hGlm,using 'form pos 81,42*PD 6.2,pos 333,2*PD 3,13*pd 6.2': mat zer
	loop
	EO_GLMSTR: !
	close #hGlm:

	execute 'drop [Q]\GLmstr\GLTrans.h[cno]'
	! fnDel('[Q]\GLmstr\glTrans-IdxAcct.h[cno]')    maybe a good idea??
	open #1: 'Name=[Q]\GLmstr\ACTrans.h[cno],Size=0,RecL=72,Replace,NoShr',internal,output
	close #1:
end if  ! /r
fnChain('S:\[cursystem]\Company.br')

 
Xit: fnChain('S:\Core\Programs\Select Company')
include: ertn
