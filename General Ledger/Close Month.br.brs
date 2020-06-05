! Formerly S:\acsGL\CloseMonth
! GL Month End Closing
autoLibrary
on error goto Ertn
dim bc(13)
dim tr(7)
dim tr$*12
dim td$*30
fnTop(program$)
if fnprocess=1 then goto GET_GOING
SCREEN1: ! r:
	fnTos(sn$='Close_Month')
	lc=0 : mylen=22 : mypos=mylen+2
	fnLbl(lc+=1,1,"Closing Period Number:",mylen,right)
	fnTxt(lc,mypos,2,0,0,'number')
	resp$(1)=str$(fnactpd)
	fnCmdSet(2)
	fnAcs2(mat resp$,ckey)
	if ckey=5 then goto Xit
	actpd=val(resp$(1))
	if actpd<1 or actpd>13 then goto SCREEN1
goto GET_GOING ! /r
GET_GOING: !
	fnAutomatedSavePoint('before')
	open #1: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,outIn,relative
	read #1,using 'Form pos 384,n 2,POS 417,N 1',rec=1: nap,reccode
	close #1:
	fn_current_to_accumlated_trans
	OPEN_GLMSTR: !
	open #h_glmstr:=fngethandle: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLINDEX.h[cno]",internal,outIn,keyed
	! fnwait - "Closing Month..."
	do
		read #h_glmstr,using 'Form POS 87,14*PD 6.2': cb,mat bc eof EO_GLMSTR
		bc(actpd)=cb
		bb=cb
		rewrite #h_glmstr,using 'Form POS 81,PD 6.2,POS 93,13*PD 6.2,POS 333,2*PD 3': bb,mat bc,0,0
	loop
	EO_GLMSTR: !
	close #h_glmstr:
 
	open #21: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,outIn,relative
	rewrite #21,using 'Form pos 296,n 2',rec=1: actpd
	close #21:
 
	actpd+=1
	if actpd>nap then actpd=1
	fnactpd(actpd)
 
	open #1: "Name=[Q]\GLmstr\GLTrans.h[cno],Size=0,RecL=73,Replace",internal,output
	write #1,using 'Form POS 1,N 3,N 6,N 3,N 6,PD 6.2,2*N 2,C 12,C 30,PD 3': 0,0,0,0,0,0,0," "," ",1
	close #1:
 
	if reccode=0 then goto GLBREC_DROP
	open #h_glbrec:=1: "Name=[Q]\GLmstr\GLBRec.h[cno],KFName=[Q]\GLmstr\GLRecIdx.h[cno]",internal,outIn,keyed ioerr GLBREC_DROP
	do
		read #h_glbrec,using 'Form POS 63,PD 5.2,POS 68,N 1': a2,a3 eof FINIS
		if a3=1 or a2=0 then delete #h_glbrec:
	loop
	FINIS: !
	close #h_glbrec:
	fnRemoveDeletedRecords("[Q]\GLmstr\GLBRec.h[cno]")
	fnIndex("[Q]\GLmstr\GLBREC.h[cno]","[Q]\GLmstr\GLRecIdx.h[cno]","1 24")
goto Xit
GLBREC_DROP: !
	open #h_glbrec:=1: "Name=[Q]\GLmstr\GLBRec.h[cno],SIZE=0,RecL=68,Replace",internal,outIn
goto FINIS
!
def fn_current_to_accumlated_trans
	! fnwait - "Transferring Current Transactions to Accumulated Trans..."
	open #hTransAccumulated:=fngethandle: "Name=[Q]\GLmstr\AcTrans.h[cno],RecL=72,use",internal,outin
	open #hTransCurrent:=fngethandle: "Name=[Q]\GLmstr\GLTrans.h[cno]",internal,input
	do
		read #hTransCurrent,using 'Form POS 1,N 3,N 6,N 3,N 6,PD 6.2,2*N 2,C 12,C 30,N 2': mat tr,tr$,td$ eof CTAT_EoTransCurrent
		if tr(1)+tr(2)+tr(3)<>0 then
			write #hTransAccumulated,using 'Form POS 1,N 3,N 6,N 3,N 6,PD 6.2,2*N 2,C 12,C 30,N 2': mat tr,tr$,td$,actpd
		end if
	loop
	CTAT_EoTransCurrent: !
	close #hTransAccumulated:
	close #hTransCurrent:
	fnIndex("[Q]\GLmstr\ACTRANS.h[cno]","[Q]\GLmstr\ACTRIDX.h[cno]","1/71/17/13 12/2/2/4")
fnend
!
Xit: fnXit
include: Ertn
