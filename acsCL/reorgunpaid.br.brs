00010 ! Replace R:\acsCL\ReorgUnpaid
00020 ! reorganize the unpaid file.  For years the allocations on the unpaid invoices have not been cleared from the allocaiton file.  If he same invoice is used again, it will add the old allocation to the check in history.
00030 ! ______________________________________________________________________
00040   library 'R:\Core\Library': fntop,fnxit, fncno,fndat,fnopenprn,fncloseprn,fnchain,fnerror
00050   let fntop(program$,cap$="Reorganize Unpaid File")
00060   let fncno(cno,cnam$)
00070   on error goto ERTN
00080 ! ______________________________________________________________________
00090   dim n$*40,a$(3)*30,b(4),k$*20,hk$*20,k4$*20,cap$*128
00100   dim bn$*30,bcn$*30,in1$(9),de$*30
00110   dim nam$*50,cnam$*40,d(2),sn$*50,egl(10)
00120   dim bk$(20)*28,nam$*28,cnt$*25,jci$(100)*30
00130   dim contact$*30,ph$*12,email$*50,fax$*12,myact$*20,up$(4)*18,gldesc$*30
00140 ! ______________________________________________________________________
00150   open #paytrans=4: "Name=Q:\CLmstr\PayTrans.H"&str$(cno)&",KFName=Q:\CLmstr\UnPdIdx1.H"&str$(cno)&",Shr",internal,outin,keyed 
00160   open #unpdaloc=5: "Name=Q:\CLmstr\UnPdAloc.H"&str$(cno)&",KFName=Q:\CLmstr\Uaidx2.H"&str$(cno)&",Shr",internal,outin,keyed 
00170   open #newunpdaloc=6: "Name=Q:\CLmstr\NewUnPdAloc.H"&str$(cno)&",RecL=67,replace",internal,outin 
00180 READ_UNPAID_INVOICES: ! read unpaid invoice file
00190   read #paytrans,using 'Form POS 1,C 8,c 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8': vn$,iv$,mat up$,upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate eof L260
00200   restore #unpdaloc,key>=lpad$(rtrm$(vn$),8)&lpad$(rtrm$(iv$),12): nokey READ_UNPAID_INVOICES
00210 L210: read #unpdaloc,using 'Form POS 1,C 8,C 12,c 12,PD 5.2,C 30': hvn$,hiv$,gl$,percent,gldesc$ eof READ_UNPAID_INVOICES
00220   if vn$=hvn$ and iv$<>hiv$ then goto L210
00230   if vn$<>hvn$ or iv$<>hiv$ then goto READ_UNPAID_INVOICES
00240   write #newunpdaloc,using 'Form POS 1,C 8,C 12,c 12,PD 5.2,C 30': hvn$,hiv$,gl$,percent,gldesc$
00250   goto L210
00260 L260: close #4: 
00261   close #5: 
00262   close #6: 
00270   execute "copy Q:\CLmstr\newunpdaloc.H"&str$(cno)&",Q:\CLmstr\unpdaloc.H"&str$(cno)&" -n"
00280   execute "INDEX Q:\CLmstr\PayTrans.H"&str$(cno)&",Q:\CLmstr\UnPdIdx1.H"&str$(cno)&",1,20,Replace,DupKeys -n"
00290   execute "INDEX Q:\CLmstr\unpdaloc.H"&str$(cno)&",Q:\CLmstr\Uaidx2.H"&str$(cno)&",1,20,Replace,DupKeys -n"
00300   execute "INDEX Q:\CLmstr\unpdaloc.H"&str$(cno)&",Q:\CLmstr\Uaidx1.H"&str$(cno)&",9,12,Replace,DupKeys -n"
00310 XIT: let fnxit
00320 ! ______________________________________________________________________
00330 ! <Updateable Region: ERTN>
00340 ERTN: let fnerror(cap$,err,line,act$,"xit")
00350   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00360   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00370   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00380 ERTN_EXEC_ACT: execute act$ : goto ERTN
00390 ! /region
00400 ! ______________________________________________________________________
