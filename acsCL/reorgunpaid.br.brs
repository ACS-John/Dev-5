00010 ! Replace S:\acsCL\ReorgUnpaid
00020 ! reorganize the unpaid file.  For years the allocations on the unpaid invoices have not been cleared from the allocaiton file.  If he same invoice is used again, it will add the old allocation to the check in history.
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit,fnerror,fnCopy,fnindex_it
00050   let fntop(program$,cap$="Reorganize Unpaid File")
00070   on error goto ERTN
00080 ! ______________________________________________________________________
00090   dim cap$*128
00130   dim gldesc$*30
00140 ! ______________________________________________________________________
00150   open #paytrans=4: "Name="&env$('Q')&"\CLmstr\PayTrans.H"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\UnPdIdx1.H"&env$('cno')&",Shr",internal,outin,keyed 
00160   open #unpdaloc=5: "Name="&env$('Q')&"\CLmstr\UnPdAloc.H"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\Uaidx2.H"&env$('cno')&",Shr",internal,outin,keyed 
00170   open #newunpdaloc=6: "Name="&env$('Q')&"\CLmstr\NewUnPdAloc.H"&env$('cno')&",RecL=67,replace",internal,outin 
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
00270   fnCopy(env$('Q')&'\CLmstr\newunpdaloc.H'&env$('cno'),env$('Q')&"\CLmstr\unpdaloc.H"&env$('cno'))
00280   fnIndex_it(env$('Q')&'\CLmstr\PayTrans.H'&env$('cno'),env$('Q')&'\CLmstr\UnPdIdx1.H'&env$('cno'),"1,20")
00290   fnIndex_it(env$('Q')&'\CLmstr\unpdaloc.H'&env$('cno'),env$('Q')&'\CLmstr\Uaidx2.H'&env$('cno'),"1,20")
00300   fnIndex_it(env$('Q')&'\CLmstr\unpdaloc.H'&env$('cno'),env$('Q')&'\CLmstr\Uaidx1.H'&env$('cno'),"9,12")
00310 XIT: let fnxit
00320 ! ______________________________________________________________________
00330 ! <Updateable Region: ERTN>
00340 ERTN: let fnerror(program$,err,line,act$,"xit")
00350   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00360   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00370   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00380 ERTN_EXEC_ACT: execute act$ : goto ERTN
00390 ! /region
00400 ! ______________________________________________________________________
