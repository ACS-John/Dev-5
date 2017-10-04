00010 ! Replace S:\acsCL\RemovePaidInvoices
00020 ! Remove Paid Invoices
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fndate_mmddyy_to_ccyymmdd,fntos,fnlbl,fntxt,fnacs,fncmdset
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cap$*128
00080 ! ______________________________________________________________________
00090   fntop(program$,cap$="Remove Paid Invoices")
00100   cancel=99 : let right=1 : center=2 : on=1 : off=0 !:
        left=0
00110   fncno(cno)
00120 ! ______________________________________________________________________
00130   fntos(sn$='RmvPdInv') !:
        lc=0 : let mylen=13 : let mypos=mylen+2 !:
        let mywidth=42
00140   fnlbl (lc+=1,1,"Removal Date:",mylen,right)
00150   fntxt(lc,mypos,10,0,left,'1003') !:
        let resp$(1)=""
00160   lc+=1
00170   fnlbl(lc+=1,1,"All transactions with a date equal to",mywidth,center)
00180   fnlbl(lc+=1,1,"or older than this date will be removed.",mywidth,center)
00190   fncmdset(2)
00200   fnacs(sn$,0,mat resp$,ckey)
00210   if ckey=5 or ckey=cancel then goto XIT else !:
          let rd1=val(resp$(1))
00220 ! fnwait
00230   open #ivpaid=1: "Name="&env$('Q')&"\CLmstr\IvPaid.H"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\IVIndex.H"&str$(cno)&",Shr",internal,outin,keyed 
00240   open #work=2: "Name="&env$('Q')&"\CLmstr\Work."&session$&",Size=0,RecL=34,Replace",internal,output 
00250 READ_IVPAID: ! 
00260   read #ivpaid,using 'Form POS 1,C 8,C 12,G 6,G 8': vn$,iv$,dp,ckn eof EO_IVPAID
00270   if fndate_mmddyy_to_ccyymmdd(dp)<=rd1 then goto READ_IVPAID
00280   write #work,using 'Form POS 1,C 8,C 12,G 6,G 8': vn$,iv$,dp,ckn
00290   goto READ_IVPAID
00300 ! ______________________________________________________________________
00310 EO_IVPAID: ! 
00320   close #ivpaid: 
00330   close #work: 
00340   execute "Free "&env$('Q')&"\CLmstr\IvPaid.H"&str$(cno)&" -n"
00350   execute "Rename "&env$('Q')&"\CLmstr\Work."&session$&' '&env$('Q')&"\CLmstr\IvPaid.H"&str$(cno)&" -n"
00360   goto XIT
00370 ! ______________________________________________________________________
00380 XIT: let fnxit
00390 ! ______________________________________________________________________
00400 ! <Updateable Region: ERTN>
00410 ERTN: let fnerror(program$,err,line,act$,"xit")
00420   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00430   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00440   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00450 ERTN_EXEC_ACT: execute act$ : goto ERTN
00460 ! /region
00470 ! ______________________________________________________________________
