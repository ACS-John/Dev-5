! Replace S:\acsCL\ReorgUnpaid
! reorganize the unpaid file.  For years the allocations on the unpaid invoices have not been cleared from the allocaiton file.  If he same invoice is used again, it will add the old allocation to the check in history.
 
	autoLibrary
	fnTop(program$,cap$="Reorganize Unpaid File")
	on error goto Ertn
 
	dim cap$*128
	dim gldesc$*30
 
	open #paytrans=4: "Name=[Q]\CLmstr\PayTrans.h[cno],KFName=[Q]\CLmstr\UnPdIdx1.h[cno],Shr",internal,outIn,keyed
	open #unpdaloc=5: "Name=[Q]\CLmstr\UnPdAloc.h[cno],KFName=[Q]\CLmstr\Uaidx2.h[cno],Shr",internal,outIn,keyed
	open #newunpdaloc=6: "Name=[Q]\CLmstr\NewUnPdAloc.h[cno],RecL=67,replace",internal,outIn
READ_UNPAID_INVOICES: ! read unpaid invoice file
	read #paytrans,using 'form pos 1,C 8,c 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8': vn$,iv$,mat up$,upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate eof L260
	restore #unpdaloc,key>=lpad$(rtrm$(vn$),8)&lpad$(rtrm$(iv$),12): nokey READ_UNPAID_INVOICES
L210: !
	read #unpdaloc,using 'form pos 1,C 8,C 12,c 12,PD 5.2,C 30': hvn$,hiv$,gl$,percent,gldesc$ eof READ_UNPAID_INVOICES
	if vn$=hvn$ and iv$<>hiv$ then goto L210
	if vn$<>hvn$ or iv$<>hiv$ then goto READ_UNPAID_INVOICES
	write #newunpdaloc,using 'form pos 1,C 8,C 12,c 12,PD 5.2,C 30': hvn$,hiv$,gl$,percent,gldesc$
	goto L210
L260: close #paytrans:
	close #unpdaloc:
	close #newunpdaloc:
	fnCopy('[Q]\CLmstr\newunpdaloc.h[cno]',"[Q]\CLmstr\unpdaloc.h[cno]")
	fnIndex('[Q]\CLmstr\PayTrans.h[cno]','[Q]\CLmstr\UnPdIdx1.h[cno]',"1,20")
	fnIndex('[Q]\CLmstr\unpdaloc.h[cno]','[Q]\CLmstr\Uaidx2.h[cno]',"1,20")
	fnIndex('[Q]\CLmstr\unpdaloc.h[cno]','[Q]\CLmstr\Uaidx1.h[cno]',"9,12")
Xit: fnXit
 
include: ertn
