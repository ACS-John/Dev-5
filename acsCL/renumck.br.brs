00010 ! Replace S:\acsCL\RENUMCK
00020 ! ReNUMBER A SERIES OF CHECKS
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnerror,fncno,fnAcs,fnTos,fnTxt,fndate_mmddyy_to_ccyymmdd,fnCmdSet,fnLbl
00050   on error goto Ertn
00060 ! ______________________________________________________________________
00070   dim de$*30,cap$*128,tr$(5)*35
00080 ! ______________________________________________________________________
00090   fncno(cno)
00100   fntop(program$,"Renumber Checks")
00110   cancel=99 : right=1 : center=2 : on=1 : off=0 !:
        left=0
00120   open #20: "Name=[Q]\CLmstr\Company.h[cno],Shr",internal,input  !:
        read #20,using 'Form POS 417,N 1': rcn !:
        close #20: 
00130   open #trmstr:=1: "Name=[Q]\CLmstr\TrMstr.H[cno],KFName=[Q]\CLmstr\TrIdx1.H[cno]",internal,outIn,keyed 
00140   open #tralloc:=3: "Name=[Q]\CLmstr\TrAlloc.H[cno],KFName=[Q]\CLmstr\TrAlloc-idx.h[cno]",internal,outIn,keyed 
00150 L150: fnTos(sn$='RmTrans-'&str$(rcn)) !:
        mylen=30 : mypos=mylen+3 : lc=0
00160   fnLbl(lc+=1,1,"First Check Number to Renumber:",mylen,right)
00170   fnTxt(lc,mypos,10,0,0,'30') !:
        resp$(1)=""
00180   fnLbl(lc+=1,1,"Last Check Number to Renumber:",mylen,right)
00190   fnTxt(lc,mypos,10,0,0,'30') !:
        resp$(2)=""
00200   fnLbl(lc+=1,1,"First New Check Number to Use:",mylen,right)
00210   fnTxt(lc,mypos,10,0,0,'30') !:
        resp$(3)=""
00220   fnLbl(lc+=1,1,"Bank Account Number:",mylen,right)
00230   fnTxt(lc,mypos,2,0,0,'30') !:
        resp$(4)=""
00240   fnCmdSet(2)
00250   fnAcs(sn$,0,mat resp$,ckey)
00260   if ckey=5 or ckey=cancel then goto XIT else !:
          firstold=val(resp$(1)) !:
          lastold=val(resp$(2)) !:
          newnumber=firstnew=val(resp$(3)) !:
          bankaccount=val(resp$(4))
00270   if firstold=0 or lastold=0 or newnumber=0 or bankaccount=0 then goto L150
00280 READ_TRMSTR: ! 
00290   restore #trmstr,key>=cnvrt$("pic(zz)",bankaccount)&"1"&cnvrt$("pic(zzzzzzzz",firstold): nokey L150
00300 L300: read #trmstr,using 'Form POS 1,G 2,G 1,C 8,G 6,PD 10.2,C 8,C 35,G 1,G 6,G 1': bank_code,tcde,tr$(1),tr$(2),tr3,tr$(4),tr$(5),pcde,clr,scd eof END1
00310   x=val(tr$(1)) conv L300
00320   if x<firstold or x>lastold then goto END1
00330   if bank_code<>bankaccount then goto L300
00340   if tcde<>1 then goto L300
00350   rewrite #trmstr,using 'Form POS 1,G 2,G 1,n 8': bank_code,tcde,newnumber
00360   restore #tralloc: 
00370   key$=cnvrt$('Pic(ZZ)',bank_code)&str$(tcde)&tr$(1) !:
        restore #tralloc,key>=key$: nokey EO_TRALLOC
00380 READ_TRALLOC: ! 
00390   read #tralloc,using 'Form POS 1,C 11,C 12,PD 5.2,C 30,G 6,X 3,C 12,N 1': newkey$,gl$,amt,de$,ivd,po$,postd eof EO_TRALLOC
00400   if newkey$=key$ then goto L410 else goto EO_TRALLOC
00410 L410: rewrite #tralloc,using 'Form POS 4,n 8': newnumber
00420   goto READ_TRALLOC
00430 EO_TRALLOC: ! 
00440   newnumber+=1
00450   goto READ_TRMSTR
00460 ! ______________________________________________________________________
00470 END1: ! 
00480   close #trmstr: 
00490   close #tralloc: 
00500   execute "Index [Q]\CLmstr\TrMstr.H[cno]"&' '&"[Q]\CLmstr\TrIdx1.H[cno] 1 11 Replace DupKeys -n"
00510   execute "Index [Q]\CLmstr\TrMstr.H[cno]"&' '&"[Q]\CLmstr\TrIdx2.H[cno] 28/1 8/11 Replace DupKeys -n"
00520   execute "Index [Q]\CLmstr\TrMstr.H[cno]"&' '&"[Q]\CLmstr\TrIdx3.H[cno] 16/12/4 2/4/8 Replace DupKeys -n"
00530   execute "Index [Q]\CLmstr\TrAlloc.H[cno]"&' '&"[Q]\CLmstr\TrAlloc-idx.H[cno] 1 11 Replace DupKeys -n"
00540   goto XIT
00550 ! ______________________________________________________________________
00560 XIT: fnxit
00570 ! ______________________________________________________________________
00580 ! <Updateable Region: ERTN>
00590 ERTN: fnerror(program$,err,line,act$,"xit")
00600   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00610   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00620   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00630 ERTN_EXEC_ACT: execute act$ : goto ERTN
00640 ! /region
00650 ! ______________________________________________________________________
