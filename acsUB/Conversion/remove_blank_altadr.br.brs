00010 ! Replace S:\acsUB\conversion\remove_blank_altadr
00020 ! remove blank alternate billing addresses (still get key= after conversion and causes blank addresses  ( on old system the addresses were just set to blank when deleted, but not the customer #
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnAcs,fnLbl,fnTxt,fnwait,fncmbrt2,fncombof,fnChk,fnerror,fnOpt,fnTos,fncmbact,fncno,fnxit,fnCmdSet,fntop,fnformnumb$,fnpause,fnopenprn,fncloseprn,fnCmdKey
00050   on error goto Ertn
00060 ! ______________________________________________________________________
00070   fncno(cno)
00080 ! 
00090   dim ba$(4)*30
00100   open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed  ! open in account order
00110   open #3: "Name=[Q]\UBmstr\UBAdrBil.h[cno],KFName=[Q]\UBmstr\adrIndex.h[cno],Shr",internal,outIn,keyed 
00120   form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1859,pd 5.2,pos 1750,2*n 6
00130 L130: ! read alternate billing address
00140   read #3,using L180: z$,mat ba$ eof XIT
00150   pr z$,mat ba$
00160   pause 
00170   if trim$(ba$(1))="" and trim$(ba$(2))="" and trim$(ba$(3))="" and trim$(ba$(4))="" then goto L220
00180 L180: form pos 1,c 10,4*c 30
00190   read #1,using L200,key=z$: holdz$ nokey L220
00200 L200: form pos 1,c 10
00210   goto L130
00220 L220: delete #3,key=z$: 
00230   goto L130
00240 XIT: fnxit
00250 ! ______________________________________________________________________
00260 ERTN: fnerror(program$,err,line,act$,"xit")
00270   if uprc$(act$)<>"PAUSE" then goto L300
00280   execute "list -"&str$(line) !:
        pause  !:
        goto L300
00290   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause 
00300 L300: execute act$
00310   goto ERTN
00320 ! ______________________________________________________________________
