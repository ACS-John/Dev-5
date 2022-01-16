! reorganize the unpaid file.  For years the allocations on the unpaid invoices have not been cleared from the allocaiton file.  If he same invoice is used again, it will add the old allocation to the check in history.
autoLibrary
fnTop(program$)
on error goto Ertn

open #paytrans=fnH: 'Name=[Q]\CLmstr\PayTrans.h[cno],KFName=[Q]\CLmstr\UnPdIdx1.h[cno],Shr',internal,outIn,keyed
open #unpdaloc=fnH: 'Name=[Q]\CLmstr\UnPdAloc.h[cno],KFName=[Q]\CLmstr\Uaidx2.h[cno],Shr',internal,outIn,keyed
open #newunpdaloc=fnH: 'Name=[Q]\CLmstr\NewUnPdAloc.h[cno],RecL=67,replace',internal,outIn
ReadUnpaidInvoice: ! read unpaid invoice file
read #paytrans,using 'form pos 1,C 8,c 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8': vn$,iv$,mat up$,upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate eof L260
restore #unpdaloc,key>=lpad$(rtrm$(vn$),8)&lpad$(rtrm$(iv$),12): nokey ReadUnpaidInvoice
ReadUnPdAlloc: !
dim gldesc$*30
read #unpdaloc,using 'form pos 1,C 8,C 12,c 12,PD 5.2,C 30': hvn$,hiv$,gl$,percent,gldesc$ eof ReadUnpaidInvoice
if vn$=hvn$ and iv$<>hiv$ then goto ReadUnPdAlloc
if vn$<>hvn$ or iv$<>hiv$ then goto ReadUnpaidInvoice
write #newunpdaloc,using 'form pos 1,C 8,C 12,c 12,PD 5.2,C 30': hvn$,hiv$,gl$,percent,gldesc$
goto ReadUnPdAlloc
L260: !
close #paytrans:
close #unpdaloc:
close #newunpdaloc:
fnCopy('[Q]\CLmstr\newunpdaloc.h[cno]','[Q]\CLmstr\unpdaloc.h[cno]')
fnIndex('[Q]\CLmstr\PayTrans.h[cno]','[Q]\CLmstr\UnPdIdx1.h[cno]','1,20')
fnIndex('[Q]\CLmstr\unpdaloc.h[cno]','[Q]\CLmstr\Uaidx2.h[cno]','1,20')
fnIndex('[Q]\CLmstr\unpdaloc.h[cno]','[Q]\CLmstr\Uaidx1.h[cno]','9,12')
Xit: fnXit

include: ertn
