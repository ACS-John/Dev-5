00010 ! Replace S:\acsGL\AcGlClr
00020 ! -- Clear Accumulated Transactions
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fndate_mmddyy_to_ccyymmdd, fntos,fnlbl,fncmdset,fntxt,fnacs, fnconsole
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cnam$*40,tr(7),tr$*12,td$*30,cap$*128
00080 ! ______________________________________________________________________
00090   let right=1 : center=2 : let left=0
00100   let fntop(program$,cap$="Clear Accumulated Transactions")
00110   let fnconsole(off=0)
00120   let fncno(cno,cnam$)
00130   let fntos(sn$='ClrAccTrans') !:
        let lc=0 !:
        let mylen=50 : let mypos=mylen+2 : let width=80
00140   let fnlbl(lc+=1,1,"* * *   Warning   * * *",width,center)
00150   let fnlbl(lc+=1,1,"This selection will remove all records from the",width,center)
00160   let fnlbl(lc+=1,1,"General Ledger Accumulated Transactions File ",width,center)
00170   let fnlbl(lc+=1,1,"older than the date entered.",width,center)
00180   let fnlbl(lc+=1,1,"Enter the Removal Date:",mylen,right)
00190   let fntxt(lc,mypos,8,0,0,'ccyymmdd') !:
        let resp$=''
00200   let fncmdset(2)
00210   let fnacs(sn$,0,mat resp$,ckey)
00220   if ckey=5 then goto XIT
00230   let rd1=val(resp$(1))
00240 ! 
00250   open #2: "Name="&env$('temp')&"\Work."&session$&",RecL=72,Replace",internal,output 
00260   open #1: "Name="&env$('Q')&"\GLmstr\ACTRANS.H"&str$(cno),internal,input 
00270 L270: read #1,using L280: mat tr,tr$,td$,pcde eof END1
00280 L280: form pos 1,n 3,n 6,n 3,n 6,pd 6.2,2*n 2,c 12,c 30,n 2
00290   if fndate_mmddyy_to_ccyymmdd(tr(4))<rd1 then goto L270
00300   write #2,using L280: mat tr,tr$,td$,pcde
00310   goto L270
00320 ! ______________________________________________________________________
00330 END1: close #2: 
00340   close #1: 
00350   execute "COPY "&env$('temp')&"\Work."&session$&' '&env$('Q')&"\GLmstr\ACTRANS.H"&str$(cno)&" -n"
00360   execute "Index "&env$('Q')&"\GLmstr\ACTRANS.H"&str$(cno)&' '&env$('Q')&"\GLmstr\ACTRIDX.H"&str$(cno)&" 1/71/17/13 12/2/2/4 Replace DupKeys"
00370   execute "free "&env$('temp')&"\Work."&session$
00380   goto XIT
00390 ! ______________________________________________________________________
00400 XIT: let fnxit
00410 ! ______________________________________________________________________
00420 ! <updateable region: ertn>
00430 ERTN: let fnerror(program$,err,line,act$,"xit")
00440   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00450   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00460   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00470 ERTN_EXEC_ACT: execute act$ : goto ERTN
00480 ! /region
00490 ! ______________________________________________________________________
