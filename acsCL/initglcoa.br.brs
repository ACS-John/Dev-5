00010 ! Replace S:\acsCL\InitGLCoA
00020 ! Import General Ledger Chart of Accounts
00030 !
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fndat,fnerror,fnTos,fnLbl,fnTxt,fncomboa,fnCmdSet,fnAcs,fnmsgbox,fnFree
00042   library 'S:\Core\Library': fncopy,fnrename,fnindex_it
00050   on error goto Ertn
00060 !
00070   dim cap$*128,item1$(2)*45,resp$(10)*25,ml$(3)*70,de$*50
00080 !
00090   fntop(program$,cap$="Import GL Chart of Accounts")
00100   cancel=99 : right=1 : left=0 : center=2 : number$='30'
00120 L120: open #1: "Name=[Q]\CLmstr\GLmstr.H[cno],KFName=[Q]\CLmstr\GLINDEX.H[cno],Shr",internal,outIn,keyed 
00130 MENU1: ! 
00140   fnTos(sn$="InitGLCoA") !:
        mylen=38 : mypos=mylen+2 : lc=0
00150   fnLbl(lc+=1,1,"Extract general ledger accounts from:",38,right)
00160   item1$(1)="ACS G/L system" !:
        item1$(2)="Accountant's Diskette"
00170   fncomboa("claims-srt",lc,mypos,mat item1$,tt$) !:
        resp$(1)=item1$(1)
00180   fnLbl(lc+=1,1,"General Ledger Company Number:",mylen,right)
00190   fnTxt(lc,mypos,5,0,left,number$) !:
        resp$(2)=env$('cno')
00200   fnCmdSet(2) !:
        fnAcs(sn$,0,mat resp$,ck)
00210   if ck=5 then goto XIT else !:
          if resp$(1)=item1$(1) then pas$="BUILD" else !:
            if resp$(1)=item1$(2) then pas$="COPY"
00220   glcno=val(resp$(2))
00230   if pas$><"COPY" then goto L270
00240   close #1: ioerr L250
00250 L250: execute "COPY A:GLmstr.H"&str$(glcno)&' '&"[Q]\CLmstr\*.*" ioerr MSGBOX2
00260   goto XIT
00270 L270: if trim$(pas$)><"BUILD" then goto MENU1
00280   close #1: ioerr L290
00290 L290: open #2: "Name=[Q]\GLmstr\GLmstr.h"&str$(glcno)&",KFName=[Q]\GLmstr\GLINDEX.h"&str$(glcno)&",Shr",internal,input,keyed ioerr MSGBOX1
00300   open #1: "Name=[Q]\CLmstr\GLmstr.H[cno],Size=0,RecL=62,Replace",internal,output 
00310 L310: read #2,using 'Form POS 1,C 12,C 50': gl$,de$ eof END1
00320   write #1,using 'Form POS 1,C 12,C 50': gl$,de$
00330   goto L310
00340 END1: close #1: 
00350   close #2: 
00355   execute "Index [Q]\CLmstr\GLmstr.H[cno]"&' '&"[Q]\CLmstr\GLINDEX.H[cno] 1 12 Replace DupKeys"
00360   goto XIT
00370 !
00380   execute "Index [Q]\CLmstr\GLmstr.H[cno]"&' '&"[Q]\CLmstr\GLINDEX.H[cno] 1 12 Replace DupKeys"
00390   goto XIT
00400 !
00410   restore #1,key>="            ": nokey MENU1
00420 L420: ln=eof1=0
00430   pr newpage
00440   if ck=5 or eof1=1 then goto MENU1
00450   goto L420
00460 !
00470 XIT: fnxit
00480 ! r: unreferenced stuff
00490   close #1: ioerr ignore
00500   fnCopy("[Q]\CLmstr\GLmstr.H[cno]",env$('Temp')&"\WORK",0,"-D")
00510   fnFree("[Q]\CLmstr\GLmstr.h[cno]")
00520   fnRename(env$('Temp')&"\WORK","[Q]\CLmstr\GLmstr.h[cno]")
00530   fnIndex_it("[Q]\CLmstr\GLmstr.H[cno]","[Q]\CLmstr\GLINDEX.H[cno]","1 12")
00540   goto L120
00550 ! /r
00560 ! <Updateable Region: ERTN>
00570 ERTN: fnerror(program$,err,line,act$,"xit")
00580   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00590   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00600   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00610 ERTN_EXEC_ACT: execute act$ : goto ERTN
00620 ! /region
00630 !
00640 MSGBOX1: ! 
00650   mat ml$(2) !:
        ml$(1)="A general ledger chart of accounts has not been set up" !:
        ml$(2)="for this company.  You must choose a different option" !:
        fnmsgbox(mat ml$,resp$,cap$,16) !:
        goto MENU1
00660 MSGBOX2: ! 
00670   mat ml$(1) !:
        ml$(1)="Be sure the diskette is properly inserted and try again" !:
        fnmsgbox(mat ml$,resp$,cap$,16) !:
        goto MENU1
