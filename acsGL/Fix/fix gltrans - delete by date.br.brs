! this program deletes transactions matching a given month from the unclosed transaction file.  it makes a backup copy of the file first
! NOTICE: You should probably be using "Remove Entries Posted by Mistake" instead of this.
enableDelete=1
monthToDelete=8
 
autoLibrary
 
if enableDelete then
	fnCopy('[Q]\GLmstr\GLTrans.h[cno]','[Q]\GLmstr\GLTrans backup at '&fnSafeFilename$(date$&' - '&time$)&'.h[cno]')
end if
open #hTrans=fnH: 'Name=[Q]\GLmstr\GLTrans.h[cno],kfname=[Q]\GLmstr\glTrans-IdxAcct.h[cno],Shr',internal,outIn,keyed
do
	dim tr(7)
	dim tr$*12
	dim td$*30
	read #hTrans,using 'Form POS 1,N 3,N 6,N 3,N 6,PD 6.2,2*N 2,C 12,C 30,N 2': mat tr,tr$,td$ eof EO_hTrans
	! bad from unpost.br.brs read #hTrans,using 'Form POS 1,C 12,N 6,PD 6.2,N 2,N 2,C 12,C 30': acct$,_s,_k,mat n,l$,p$ eof EO_hTrans
	transMonth=date(days(tr(4),'mmddyy'),'mm')
	if tr4prior<>tr(4) then
		
		pr tr(4),transMonth
		tr4prior=tr(4)
	en if
	if enableDelete and transMonth=monthToDelete then
		delete #hTrans:
		deleteCount+=1
	end if
loop
EO_hTrans: !
pr 'lrec(hTrans)=';lrec(hTrans)
close #hTrans:
pr 'deleteCount=';deleteCount
end
