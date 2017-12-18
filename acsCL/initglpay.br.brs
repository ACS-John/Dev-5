00010 ! Replace S:\acsCL\InitGLPay
00020 ! Import General Ledger Payee Records
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fndat,fnerror,fntos,fnlbl,fntxt,fncomboa,fncmdset,fnacs,fnmsgbox,fngethandle
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cnam$*40,dat$*20,cap$*128,item1$(2)*45,resp$(10)*25,ml$(3)*70,de$*50
00080 ! ______________________________________________________________________
00090   fntop(program$,cap$="Import GL Payee Records")
00100   cancel=99 : right=1 : left=0 : center=2 : number$='30'
00110   fncno(cno,cnam$) !:
        fndat(dat$)
00120 MENU1: ! 
00130   fntos(sn$="InitGLPay") !:
        mylen=45 : mypos=mylen+2 : lc=0
00140   fnlbl(lc+=1,1,"Extract Payee Information from general ledger:",45,right)
00150   item1$(1)="ACS G/L system" !:
        item1$(2)="Accountant's Diskette"
00160   fncomboa("claims-srt",lc,mypos,mat item1$,tt$) !:
        resp$(1)=item1$(1)
00170   fnlbl(lc+=1,1,"General Ledger Company Number:",mylen,right)
00180   fntxt(lc,mypos,5,0,left,number$) !:
        resp$(2)=env$('cno')
00190   fncmdset(2) !:
        fnacs(sn$,0,mat resp$,ck)
00200   if ck=5 then goto XIT else !:
          if resp$(1)=item1$(1) then pas$="BUILD" else !:
            if resp$(1)=item1$(2) then pas$="COPY"
00210   glcno=val(resp$(2))
00220   execute "COPY A:paymstr.H"&str$(glcno)&' '&env$('Q')&"\CLmstr\*.*" ioerr MSGBOX2
00230   execute "COPY A:payeeglbreakdown.H"&str$(glcno)&' '&env$('Q')&"\CLmstr\*.*" ioerr MSGBOX2
00240   execute "Index "&env$('Q')&"\CLmstr\paymstr.H"&str$(glcno)&' '&env$('Q')&"\CLmstr\payidx1.H"&str$(glcno)&",1,8,replace,DupKeys"
00250   execute "Index "&env$('Q')&"\CLmstr\payeeglbreakdown.H"&str$(glcno)&' '&env$('Q')&"\CLmstr\Payeeglbkdidx.H"&str$(glcno)&",1,8,replace,DupKeys"
00252   open #paymstr:=fngethandle: "Name="&env$('Q')&"\CLmstr\PayMstr.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\PayIdx1.h"&env$('cno')&",Shr",internal,outin,keyed 
00254   open #payeegl:=fngethandle: "Name="&env$('Q')&"\CLmstr\PayeeGLBreakdown.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\Payeeglbkdidx.h"&env$('cno')&",Use,RecL=56,KPs=1,KLn=8,Shr",internal,outin,keyed 
00255   version(payeegl,1)
00256   version(paymstr,1)
00257   close #paymstr: 
00258   close #payeegl: 
00260 XIT: fnxit
00270 ! ______________________________________________________________________
00280 ! <Updateable Region: ERTN>
00290 ERTN: fnerror(program$,err,line,act$,"xit")
00300   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00310   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00320   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00330 ERTN_EXEC_ACT: execute act$ : goto ERTN
00340 ! /region
00350 ! ______________________________________________________________________
00360 MSGBOX1: ! 
00370   mat ml$(2) !:
        ml$(1)="A general ledger chart of accounts has not been set up" !:
        ml$(2)="for this company.  You must choose a different option" !:
        fnmsgbox(mat ml$,resp$,cap$,16) !:
        goto MENU1
00380 MSGBOX2: ! 
00390   mat ml$(1) !:
        ml$(1)="Be sure the diskette is properly inserted and try again" !:
        fnmsgbox(mat ml$,resp$,cap$,16) !:
        goto MENU1
