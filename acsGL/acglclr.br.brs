00010 ! Replace S:\acsGL\AcGlClr
00020 ! -- Clear Accumulated Transactions
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fndate_mmddyy_to_ccyymmdd, fnTos,fnLbl,fnCmdSet,fnTxt,fnAcs, fnconsole
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cnam$*40,tr(7),tr$*12,td$*30,cap$*128
00080 ! ______________________________________________________________________
00090   right=1 : center=2 : left=0
00100   fntop(program$,cap$="Clear Accumulated Transactions")
00110   fnconsole(off=0)
00120   fncno(cno,cnam$)
00130   fnTos(sn$='ClrAccTrans') !:
        lc=0 !:
        mylen=50 : mypos=mylen+2 : width=80
00140   fnLbl(lc+=1,1,"* * *   Warning   * * *",width,center)
00150   fnLbl(lc+=1,1,"This selection will remove all records from the",width,center)
00160   fnLbl(lc+=1,1,"General Ledger Accumulated Transactions File ",width,center)
00170   fnLbl(lc+=1,1,"older than the date entered.",width,center)
00180   fnLbl(lc+=1,1,"Enter the Removal Date:",mylen,right)
00190   fnTxt(lc,mypos,8,0,0,'ccyymmdd') !:
        resp$=''
00200   fnCmdSet(2)
00210   fnAcs(sn$,0,mat resp$,ckey)
00220   if ckey=5 then goto XIT
00230   rd1=val(resp$(1))
00240 ! 
00250   open #2: "Name="&env$('temp')&"\Work."&session$&",RecL=72,Replace",internal,output 
00260   open #1: "Name="&env$('Q')&"\GLmstr\ACTRANS.H"&env$('cno'),internal,input 
00270 L270: read #1,using L280: mat tr,tr$,td$,pcde eof END1
00280 L280: form pos 1,n 3,n 6,n 3,n 6,pd 6.2,2*n 2,c 12,c 30,n 2
00290   if fndate_mmddyy_to_ccyymmdd(tr(4))<rd1 then goto L270
00300   write #2,using L280: mat tr,tr$,td$,pcde
00310   goto L270
00320 ! ______________________________________________________________________
00330 END1: close #2: 
00340   close #1: 
00350   execute "COPY "&env$('temp')&"\Work."&session$&' '&env$('Q')&"\GLmstr\ACTRANS.H"&env$('cno')&" -n"
00360   execute "Index "&env$('Q')&"\GLmstr\ACTRANS.H"&env$('cno')&' '&env$('Q')&"\GLmstr\ACTRIDX.H"&env$('cno')&" 1/71/17/13 12/2/2/4 Replace DupKeys"
00370   execute "free "&env$('temp')&"\Work."&session$
00380   goto XIT
00390 ! ______________________________________________________________________
00400 XIT: fnxit
00410 ! ______________________________________________________________________
00420 ! <updateable region: ertn>
00430 ERTN: fnerror(program$,err,line,act$,"xit")
00440   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00450   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00460   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00470 ERTN_EXEC_ACT: execute act$ : goto ERTN
00480 ! /region
00490 ! ______________________________________________________________________
