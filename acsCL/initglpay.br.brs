00010 ! Replace R:\acsCL\InitGLPay
00020 ! Import General Ledger Payee Records
00030 ! ______________________________________________________________________
00040   library 'R:\Core\Library': fntop,fnxit, fncno,fndat,fnerror,fntos,fnlbl,fntxt,fncomboa,fncmdset,fnacs,fnmsgbox,fngethandle
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cnam$*40,dat$*20,cap$*128,item1$(2)*45,resp$(10)*25,ml$(3)*70,de$*50
00080 ! ______________________________________________________________________
00090   let fntop(program$,cap$="Import GL Payee Records")
00100   let cancel=99 : let right=1 : let left=0 : let center=2 : let number$='30'
00110   let fncno(cno,cnam$) !:
        let fndat(dat$)
00120 MENU1: ! 
00130   let fntos(sn$="InitGLPay") !:
        let mylen=45 : let mypos=mylen+2 : let lc=0
00140   let fnlbl(lc+=1,1,"Extract Payee Information from general ledger:",45,right)
00150   let item1$(1)="ACS G/L system" !:
        let item1$(2)="Accountant's Diskette"
00160   let fncomboa("claims-srt",lc,mypos,mat item1$,tt$) !:
        let resp$(1)=item1$(1)
00170   let fnlbl(lc+=1,1,"General Ledger Company Number:",mylen,right)
00180   let fntxt(lc,mypos,5,0,left,number$) !:
        let resp$(2)=str$(cno)
00190   let fncmdset(2) !:
        let fnacs(sn$,0,mat resp$,ck)
00200   if ck=5 then goto XIT else !:
          if resp$(1)=item1$(1) then let pas$="BUILD" else !:
            if resp$(1)=item1$(2) then let pas$="COPY"
00210   let glcno=val(resp$(2))
00220   execute "COPY A:paymstr.H"&str$(glcno)&" Q:\CLmstr\*.*" ioerr MSGBOX2
00230   execute "COPY A:payeeglbreakdown.H"&str$(glcno)&" Q:\CLmstr\*.*" ioerr MSGBOX2
00240   execute "index Q:\CLmstr\paymstr.H"&str$(glcno)&" Q:\CLmstr\payidx1.H"&str$(glcno)&",1,8,replace,DupKeys"
00250   execute "index Q:\CLmstr\payeeglbreakdown.H"&str$(glcno)&" Q:\CLmstr\Payeeglbkdidx.H"&str$(glcno)&",1,8,replace,DupKeys"
00252   open #paymstr:=fngethandle: "Name=Q:\CLmstr\PayMstr.h"&str$(cno)&",KFName=Q:\CLmstr\PayIdx1.h"&str$(cno)&",Shr",internal,outin,keyed 
00254   open #payeegl:=fngethandle: "Name=Q:\CLmstr\PayeeGLBreakdown.h"&str$(cno)&",KFName=Q:\CLmstr\Payeeglbkdidx.h"&str$(cno)&",Use,RecL=56,KPs=1,KLn=8,Shr",internal,outin,keyed 
00255   let version(payeegl,1)
00256   let version(paymstr,1)
00257   close #paymstr: 
00258   close #payeegl: 
00260 XIT: let fnxit
00270 ! ______________________________________________________________________
00280 ! <Updateable Region: ERTN>
00290 ERTN: let fnerror(cap$,err,line,act$,"xit")
00300   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00310   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00320   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00330 ERTN_EXEC_ACT: execute act$ : goto ERTN
00340 ! /region
00350 ! ______________________________________________________________________
00360 MSGBOX1: ! 
00370   mat ml$(2) !:
        let ml$(1)="A general ledger chart of accounts has not been set up" !:
        let ml$(2)="for this company.  You must choose a different option" !:
        let fnmsgbox(mat ml$,resp$,cap$,16) !:
        goto MENU1
00380 MSGBOX2: ! 
00390   mat ml$(1) !:
        let ml$(1)="Be sure the diskette is properly inserted and try again" !:
        let fnmsgbox(mat ml$,resp$,cap$,16) !:
        goto MENU1
