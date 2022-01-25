! Formerly S:\acsGL\CloseMonth
! GL Month End Closing
autoLibrary
on error goto Ertn
fnTop(program$)
if fnprocess=1 then goto MainLoop
Screen1: ! r:
	fnTos
	fnLbl(1,1,"Closing Period Number:",22,right)
	fnTxt(1,24,2,0,0,'number')
	resp$(1)=str$(fnActPd)
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	actpd=val(resp$(1))
	if actpd<1 or actpd>13 then goto Screen1
goto MainLoop ! /r
MainLoop: ! r:
	fnAutomatedSavePoint('before')
	open #1: "Name=[Q]\GLmstr\Company.h[cno],Shr",i,outi,r
	read #1,using 'form pos 384,n 2,pos 417,N 1',rec=1: nap,reccode
	close #1:
	fn_currentToAccumlatedTrans
	OPEN_GLMSTR: !
	open #hAcct=fnH: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLINDEX.h[cno]",i,outIn,k
	fnStatus("Closing Month...")
	do
		dim bc(13)
		read #hAcct,using 'form pos 87,14*PD 6.2': cb,mat bc eof EoAcct
		bc(actpd)=cb
		bb=cb
		rewrite #hAcct,using 'form pos 81,PD 6.2,pos 93,13*PD 6.2,pos 333,2*PD 3': bb,mat bc,0,0
	loop
	EoAcct: !
	close #hAcct:
	fnLastAccountingPeriodClosed(actpd)
	actpd+=1
	if actpd>nap then actpd=1
	fnActPd(actpd)

	open #1: "Name=[Q]\GLmstr\GLTrans.h[cno],Size=0,RecL=73,Replace",internal,output
	write #1,using 'form pos 1,N 3,N 6,N 3,N 6,PD 6.2,2*N 2,C 12,C 30,PD 3': 0,0,0,0,0,0,0," "," ",1
	close #1:
	fnIndex('[Q]\GLmstr\GLTrans.h[cno]','[Q]\GLmstr\glTrans-IdxAcct.h[cno]','1 12')
	if reccode=0 then 
		FileDrop: !
		open #hBankRec=fnH: "Name=[Q]\GLmstr\GLBRec.h[cno],SIZE=0,RecL=68,Replace",internal,outIn
	else 
		open #hBankRec=fnH: "Name=[Q]\GLmstr\GLBRec.h[cno],KFName=[Q]\GLmstr\GLRecIdx.h[cno]",i,outIn,k ioerr FileDrop
		do
			read #hBankRec,using 'form pos 63,PD 5.2,pos 68,N 1': a2,a3 eof EoGlBrec
			if a3=1 or a2=0 then delete #hBankRec:
		loop
	end if
	EoGlBrec: !
	close #hBankRec:
	fnRemoveDeletedRecords("[Q]\GLmstr\GLBRec.h[cno]")
	fnIndex("[Q]\GLmstr\GLBREC.h[cno]","[Q]\GLmstr\GLRecIdx.h[cno]","1 24")
goto Xit ! /r

def fn_currentToAccumlatedTrans(; ___,hTransAccumulated,hTransCurrent,tr$*12,td$*30)
	fnStatus("Transferring Current Transactions to Accumulated Trans...")
	open #hTransAccumulated=fnH: "Name=[Q]\GLmstr\AcTrans.h[cno],RecL=72,use",internal,outin
	open #hTransCurrent=fnH: "Name=[Q]\GLmstr\GLTrans.h[cno]",i,i
	do
		dim tr(7)
		read #hTransCurrent,using 'form pos 1,N 3,N 6,N 3,N 6,PD 6.2,2*N 2,C 12,C 30,N 2': mat tr,tr$,td$ eof CTAT_EoTransCurrent
		if tr(1)+tr(2)+tr(3)<>0 then
			write #hTransAccumulated,using 'form pos 1,N 3,N 6,N 3,N 6,PD 6.2,2*N 2,C 12,C 30,N 2': mat tr,tr$,td$,actpd
		end if
	loop
	CTAT_EoTransCurrent: !
	close #hTransAccumulated:
	close #hTransCurrent:
	fnIndex("[Q]\GLmstr\AcTrans.h[cno]","[Q]\GLmstr\ACTRIDX.h[cno]","1/71/17/13 12/2/2/4")
fnend

Xit: fnXit
include: ertn
