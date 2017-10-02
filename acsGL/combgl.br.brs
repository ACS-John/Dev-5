00010 !  Replace S:\acsGL\CombGL
00020 ! Consolidate Companies
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnerror,fnputcno,fntos,fnlbl,fncmbcno,fntxt,fncmdset,fnacs,fncmdkey
00050   let fntop(program$,cap$="Consolidate Master Files")
00060   on error goto ERTN
00070 ! ______________________________________________________________________
00080   dim a$*416,n$*40,cap$*128,resp$(2)*80
00090 ! ______________________________________________________________________
00100   cap$="Consolidate Companies"
00110   let dcno=99
00120 MAIN: ! 
00130   let fntos(sn$='Combgl') !:
        let lc=rc=0 !:
        let mylen=29 : let mypos=mylen+2
00140   let fnlbl(lc+=1,1,"&Source Company Number:",mylen,1)
00150   let fncmbcno(lc,mypos) !:
        let resp$(rc+=1)=''
00160   let fnlbl(lc+=1,1,"&Destination Company Number:",mylen,1)
00170   let fntxt(lc,mypos,5,0,0,'30') !:
        let resp$(rc+=1)=str$(dcno)
00180   if hcno>0 then let fnlbl(lc+=1,1,"Last Company Selected: "&str$(hcno),mylen,1)
00190   let lc+=1
00200   let fnlbl(lc+=1,1,"Warning",80,2,1)
00210   let fnlbl(lc+=1,1,"Please make sure no one else is",80,2) !:
        let fnlbl(lc+=1,1,"using either company. If the destination",80,2) !:
        let fnlbl(lc+=1,1,"company exists, it will be over written",80,2) !:
        let fnlbl(lc+=1,1,"by the first company selected.  All others",80,2) !:
        let fnlbl(lc+=1,1,"will be combined with the first company selected. ",80,2)
00220   let fncmdkey("&Next",1,1,0,"Allows you to combine this company and select more if desired.")
00230   let fncmdkey("C&omplete",2,0,0,"All companies have been combined.  Return to the menu.")
00240   let fncmdkey("Cancel",5,0,1,"Stop without combining any companies.")
00250   let fnacs(sn$,0,mat resp$,ck)
00260 ! 
00270   if ck=5 then goto XIT
00280   if ck=2 then goto END1
00290   cno=val(resp$(1)(43:47)) !:
        let dcno=val(resp$(2)) !:
        let hcno=cno
00300   if cno=0 or ckey=5 then goto END1
00310   ctr+=1
00320   if ctr>1 then goto L390
00330   cno1=cno
00340   execute "Copy "&env$('Q')&"\GLmstr\*.H"&str$(cno1)&' '&env$('Q')&"\GLmstr\*.H"&str$(dcno)&" -n" ioerr MAIN
00350   open #1: "Name="&env$('Q')&"\GLmstr\Company.h"&str$(dcno)&"",internal,outin  !:
        read #1,using ' Form POS 1,C 40': n$ !:
        let n$(25:40)=" (Consolidated)" !:
        rewrite #1,using ' Form POS 1,C 40': n$ !:
        close #1: 
00360   open #1: "Name="&env$('Q')&"\GLmstr\GLmstr.H"&str$(dcno)&"",internal,output 
00370   goto MAIN
00380 ! ______________________________________________________________________
00390 L390: open #2: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\GLIndex.h"&str$(cno),internal,input,keyed ioerr MAIN
00400 L400: read #2,using 'Form POS 1,C 416': a$ eof L430
00410   write #1,using 'Form POS 1,C 416': a$
00420   goto L400
00430 L430: close #2: 
00440   goto MAIN
00450 ! ______________________________________________________________________
00460 END1: close #1: ioerr L470
00470 L470: execute "Index "&env$('Q')&"\GLmstr\GLmstr.H"&str$(dcno)&' '&env$('Q')&"\GLmstr\GLIndex.H"&str$(dcno)&" 1 12 Replace DupKeys" ioerr XIT
00480   execute "Index "&env$('Q')&"\GLmstr\GLmstr.H"&str$(dcno)&' '&env$('Q')&"\GLmstr\glIndx2.H"&str$(dcno)&" 13 30 Replace DupKeys"
00490   let fnputcno(cno=dcno)
00500 XIT: let fnxit
00510 ! ______________________________________________________________________
00520 ! <Updateable Region: ERTN>
00530 ERTN: let fnerror(program$,err,line,act$,"xit")
00540   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00550   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00560   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00570 ERTN_EXEC_ACT: execute act$ : goto ERTN
00580 ! /region
00590 ! ______________________________________________________________________
