00010 ! Replace S:\acsUB\accessdate
00020 ! -- Check for last date customers records were accessed
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnacs,fnlbl,fntxt,fnwait,fntos,fncno,fnxit,fnerror,fncmdset,fntop,fngetdir
00050   on errror goto ERTN
00060 ! ______________________________________________________________________
00070   dim text$*40,cap$*128,txt$*40,filter$*60,optionfile$*20,empty$*10,programfolder$*60
00071   dim ln$*128,resp$(100)*128
00080 ! ______________________________________________________________________
00090   let fncno(cno)
00100 ! 
00110   let fntop("S:\acsUB\ACCESSDATE",cap$="Display Last Date Customer File Accessed")
00130   execute "Dir "&env$('Q')&"\UBmstr\CUSTOMER.H"&str$(cno)&" >"&env$('Q')&"\UBmstr\DATEWork."&session$ ioerr MENU1
00140   open #12: "Name="&env$('Q')&"\UBmstr\DATEwork2.tmp,RecL=30,Replace",internal,outin 
00150   open #13: "Name="&env$('Q')&"\UBmstr\DATEWork."&session$,display,input ioerr MENU1
00160   linput #13: ln$ eof MENU1
00165   linput #13: ln$ eof MENU1
00170   access_date$=ln$(26:42)
00180   goto MENU1
00190 ! ______________________________________________________________________
00200 MENU1: ! 
00210   let fntos(sn$="AccessDate") !:
        let mylen=34 : let mypos=mylen+2
00220   let fnlbl(1,1,"Last Date Customer Files Accessed:",34,1)
00230   let fntxt(1,mypos,20,0,0,"",1) !:
        let resp$(1)=access_date$
00240   let fncmdset(41)
00250   let fnacs(sn$,0,mat resp$,ck)
00260 XIT: let fnxit
00270 ! ______________________________________________________________________
00280 ! <Updateable Region: ERTN>
00290 ERTN: let fnerror(program$,err,line,act$,"xit")
00300   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00310   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00320   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00330 ERTN_EXEC_ACT: execute act$ : goto ERTN
00340 ! /region
