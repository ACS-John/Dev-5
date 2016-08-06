00010 ! Replace R:\acsCL\InitGLCoA
00020 ! Import General Ledger Chart of Accounts
00030 ! ______________________________________________________________________
00040   library 'R:\Core\Library': fntop,fnxit, fncno,fndat,fnerror,fntos,fnlbl,fntxt,fncomboa,fncmdset,fnacs,fnmsgbox
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cnam$*40,dat$*20,cap$*128,item1$(2)*45,resp$(10)*25,ml$(3)*70,de$*50
00080 ! ______________________________________________________________________
00090   let fntop(program$,cap$="Import GL Chart of Accounts")
00100   let cancel=99 : let right=1 : let left=0 : let center=2 : let number$='30'
00110   let fncno(cno,cnam$) !:
        let fndat(dat$)
00120 L120: open #1: "Name=Q:\CLmstr\GLmstr.H"&str$(cno)&",KFName=Q:\CLmstr\GLINDEX.H"&str$(cno)&",Shr",internal,outin,keyed 
00130 MENU1: ! 
00140   let fntos(sn$="InitGLCoA") !:
        let mylen=38 : let mypos=mylen+2 : let lc=0
00150   let fnlbl(lc+=1,1,"Extract general ledger accounts from:",38,right)
00160   let item1$(1)="ACS G/L system" !:
        let item1$(2)="Accountant's Diskette"
00170   let fncomboa("claims-srt",lc,mypos,mat item1$,tt$) !:
        let resp$(1)=item1$(1)
00180   let fnlbl(lc+=1,1,"General Ledger Company Number:",mylen,right)
00190   let fntxt(lc,mypos,5,0,left,number$) !:
        let resp$(2)=str$(cno)
00200   let fncmdset(2) !:
        let fnacs(sn$,0,mat resp$,ck)
00210   if ck=5 then goto XIT else !:
          if resp$(1)=item1$(1) then let pas$="BUILD" else !:
            if resp$(1)=item1$(2) then let pas$="COPY"
00220   let glcno=val(resp$(2))
00230   if pas$><"COPY" then goto L270
00240   close #1: ioerr L250
00250 L250: execute "COPY A:Q:\GLmstr.H"&str$(glcno)&" Q:\CLmstr\*.*" ioerr MSGBOX2
00260   goto XIT
00270 L270: if trim$(pas$)><"BUILD" then goto MENU1
00280   close #1: ioerr L290
00290 L290: open #2: "Name=Q:\GLmstr\GLmstr.h"&str$(glcno)&",KFName=Q:\GLmstr\GLINDEX.h"&str$(glcno)&",Shr",internal,input,keyed ioerr MSGBOX1
00300   open #1: "Name=Q:\CLmstr\GLmstr.H"&str$(cno)&",Size=0,RecL=62,Replace",internal,output 
00310 L310: read #2,using 'Form POS 1,C 12,C 50': gl$,de$ eof END1
00320   write #1,using 'Form POS 1,C 12,C 50': gl$,de$
00330   goto L310
00340 END1: close #1: 
00350   close #2: 
00355   execute "INDEX Q:\CLmstr\GLmstr.H"&str$(cno)&" Q:\CLmstr\GLINDEX.H"&str$(cno)&" 1 12 Replace DupKeys"
00360   goto XIT
00370 ! ______________________________________________________________________
00380   execute "INDEX Q:\CLmstr\GLmstr.H"&str$(cno)&" Q:\CLmstr\GLINDEX.H"&str$(cno)&" 1 12 Replace DupKeys"
00390   goto XIT
00400 ! ______________________________________________________________________
00410   restore #1,key>="            ": nokey MENU1
00420 L420: let ln=eof1=0
00430   print newpage
00440   if ck=5 or eof1=1 then goto MENU1
00450   goto L420
00460 ! ______________________________________________________________________
00470 XIT: let fnxit
00480 ! ______________________________________________________________________
00490   close #1: ioerr L500
00500 L500: execute "COPY Q:\CLmstr\GLmstr.H"&str$(cno)&" "&env$('Temp')&"\WORK -D"
00510   execute "FREE Q:\CLmstr\GLmstr.h"&str$(cno)
00520   execute "RENAME "&env$('Temp')&"\WORK Q:\CLmstr\GLmstr.h"&str$(cno)
00530   execute "INDEX Q:\CLmstr\GLmstr.H"&str$(cno)&" Q:\CLmstr\GLINDEX.H"&str$(cno)&" 1 12 Replace DupKeys"
00540   goto L120
00550 ! ______________________________________________________________________
00560 ! <Updateable Region: ERTN>
00570 ERTN: let fnerror(cap$,err,line,act$,"xit")
00580   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00590   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00600   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00610 ERTN_EXEC_ACT: execute act$ : goto ERTN
00620 ! /region
00630 ! ______________________________________________________________________
00640 MSGBOX1: ! 
00650   mat ml$(2) !:
        let ml$(1)="A general ledger chart of accounts has not been set up" !:
        let ml$(2)="for this company.  You must choose a different option" !:
        let fnmsgbox(mat ml$,resp$,cap$,16) !:
        goto MENU1
00660 MSGBOX2: ! 
00670   mat ml$(1) !:
        let ml$(1)="Be sure the diskette is properly inserted and try again" !:
        let fnmsgbox(mat ml$,resp$,cap$,16) !:
        goto MENU1
