! formerly S:\acsGL\glreass
autoLibrary
fnTop(program$)
on error goto Ertn
fn_reassignTrA(val(env$('cno')))

dim txt$(4)*128
txt$(1)='Transaction addresses have successfully been reassigned'
txt$(2)='to their correct General Ledger accounts.'
txt$(3)=''
txt$(4)='(This process is also completed when indexes are recreated.)'
fnMsgBox(mat txt$,resp$,'',64)
goto Xit

Xit: fnXit

def library fnReassignTransactionAddresses(cno)
	autoLibrary
	fnReassignTransactionAddresses=fn_reassignTrA(cno)
fnend
def fn_reassignTrA(cno)

	open #hAcct=fnH: 'Name=[Q]\GLmstr\GLmstr.h'&str$(cno)&',KFName=[Q]\GLmstr\GLIndex.H'&str$(cno)&',Shr',i,outIn,k
	dim ta(2)
	Facct: form pos 333,2*pd 3
	open #hTrans=fnH: 'Name=[Q]\GLmstr\GLTrans.H'&str$(cno)&',Shr',i,outi,r
	Ftrans1: form pos 71,pd 3
	Ftrans2: form pos 1,c 12,pos 71,pd 3

	do
		read #hAcct,using Facct: mat ta eof EoTrans
		rewrite #hAcct,using Facct: 0,0
	loop
	EoTrans: !

	lr2=lrec(hTrans)
	rewrite #hTrans,using Ftrans1,rec=1: lr2 norec AddRecordOne
	for j=1 to lr2
		read #hTrans,using Ftrans2,rec=j: k$,nta noRec NextTrans
		read #hAcct,using Facct,key=k$: mat ta nokey NextTrans
		if ta(1)=0 then ta(1)=j
		if ta(2)>0 then rewrite #hTrans,using Ftrans1,rec=ta(2): j
		ta(2)=j
		rewrite #hAcct,using Facct,key=k$: mat ta
		rewrite #hTrans,using Ftrans1,rec=j: 0
		NextTrans: !
	next j

	close #hAcct: ioerr ignore
	close #hTrans: ioerr ignore

fnend
AddRecordOne: !
write #hTrans,using Ftrans1,rec=1: lr2
continue
include: ertn no

