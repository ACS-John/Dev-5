00010 ! Replace S:\acsCL\ReorgUnpaid
00020 ! reorganize the unpaid file.  For years the allocations on the unpaid invoices have not been cleared from the allocaiton file.  If he same invoice is used again, it will add the old allocation to the check in history.
00030 !
00040   library 'S:\Core\Library': fntop,fnxit,fnerror,fnCopy,fnindex_it
00050   fntop(program$,cap$="Reorganize Unpaid File")
00070   on error goto Ertn
00080 !
00090   dim cap$*128
00130   dim gldesc$*30
00140 !
00150   open #paytrans=4: "Name=[Q]\CLmstr\PayTrans.H[cno],KFName=[Q]\CLmstr\UnPdIdx1.H[cno],Shr",internal,outIn,keyed 
00160   open #unpdaloc=5: "Name=[Q]\CLmstr\UnPdAloc.H[cno],KFName=[Q]\CLmstr\Uaidx2.H[cno],Shr",internal,outIn,keyed 
00170   open #newunpdaloc=6: "Name=[Q]\CLmstr\NewUnPdAloc.H[cno],RecL=67,replace",internal,outIn 
00180 READ_UNPAID_INVOICES: ! read unpaid invoice file
00190   read #paytrans,using 'Form POS 1,C 8,c 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8': vn$,iv$,mat up$,upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate eof L260
00200   restore #unpdaloc,key>=lpad$(rtrm$(vn$),8)&lpad$(rtrm$(iv$),12): nokey READ_UNPAID_INVOICES
00210 L210: !
00212   read #unpdaloc,using 'Form POS 1,C 8,C 12,c 12,PD 5.2,C 30': hvn$,hiv$,gl$,percent,gldesc$ eof READ_UNPAID_INVOICES
00220   if vn$=hvn$ and iv$<>hiv$ then goto L210
00230   if vn$<>hvn$ or iv$<>hiv$ then goto READ_UNPAID_INVOICES
00240   write #newunpdaloc,using 'Form POS 1,C 8,C 12,c 12,PD 5.2,C 30': hvn$,hiv$,gl$,percent,gldesc$
00250   goto L210
00260 L260: close #paytrans: 
00261   close #unpdaloc: 
00262   close #newunpdaloc: 
00270   fnCopy('[Q]\CLmstr\newunpdaloc.H[cno]',"[Q]\CLmstr\unpdaloc.H[cno]")
00280   fnIndex_it('[Q]\CLmstr\PayTrans.H[cno]','[Q]\CLmstr\UnPdIdx1.H[cno]',"1,20")
00290   fnIndex_it('[Q]\CLmstr\unpdaloc.H[cno]','[Q]\CLmstr\Uaidx2.H[cno]',"1,20")
00300   fnIndex_it('[Q]\CLmstr\unpdaloc.H[cno]','[Q]\CLmstr\Uaidx1.H[cno]',"9,12")
00310 XIT: fnxit
00320 !
00330 ! <Updateable Region: ERTN>
00340 ERTN: fnerror(program$,err,line,act$,"xit")
00350   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00360   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00370   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00380 ERTN_EXEC_ACT: execute act$ : goto ERTN
00390 ! /region
00400 !
