00010 ! Replace S:\acsCL\RemovePaidInvoices
00020 ! Remove Paid Invoices
00030 !
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fndate_mmddyy_to_ccyymmdd,fnTos,fnLbl,fnTxt,fnAcs,fnCmdSet,fnfree,fnrename
00050   on error goto Ertn
00060 !
00070   dim cap$*128
00080 !
00090   fntop(program$,cap$="Remove Paid Invoices")
00100   cancel=99 : right=1 : center=2 : on=1 : off=0 !:
        left=0
00110   fncno(cno)
00120 !
00130   fnTos(sn$='RmvPdInv') !:
        lc=0 : mylen=13 : mypos=mylen+2 !:
        mywidth=42
00140   fnLbl (lc+=1,1,"Removal Date:",mylen,right)
00150   fnTxt(lc,mypos,10,0,left,'1003') !:
        resp$(1)=""
00160   lc+=1
00170   fnLbl(lc+=1,1,"All transactions with a date equal to",mywidth,center)
00180   fnLbl(lc+=1,1,"or older than this date will be removed.",mywidth,center)
00190   fnCmdSet(2)
00200   fnAcs(sn$,0,mat resp$,ckey)
00210   if ckey=5 or ckey=cancel then goto XIT else !:
          rd1=val(resp$(1))
00220 ! fnwait
00230   open #ivpaid=1: "Name=[Q]\CLmstr\IvPaid.H[cno],KFName=[Q]\CLmstr\IVIndex.H[cno],Shr",internal,outIn,keyed 
00240   open #work=2: "Name=[Q]\CLmstr\Work."&session$&",Size=0,RecL=34,Replace",internal,output 
00250 READ_IVPAID: ! 
00260   read #ivpaid,using 'Form POS 1,C 8,C 12,G 6,G 8': vn$,iv$,dp,ckn eof EO_IVPAID
00270   if fndate_mmddyy_to_ccyymmdd(dp)<=rd1 then goto READ_IVPAID
00280   write #work,using 'Form POS 1,C 8,C 12,G 6,G 8': vn$,iv$,dp,ckn
00290   goto READ_IVPAID
00300 !
00310 EO_IVPAID: ! 
00320   close #ivpaid: 
00330   close #work: 
00340   fnFree("[Q]\CLmstr\IvPaid.H[cno]")
00350   fnRename("[Q]\CLmstr\Work."&session$,"[Q]\CLmstr\IvPaid.H[cno]")
00360   goto XIT
00370 !
00380 XIT: fnxit
00390 !
00400 ! <Updateable Region: ERTN>
00410 ERTN: fnerror(program$,err,line,act$,"xit")
00420   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00430   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00440   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00450 ERTN_EXEC_ACT: execute act$ : goto ERTN
00460 ! /region
00470 !
