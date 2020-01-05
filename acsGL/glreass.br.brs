00010 ! Replace S:\acsGL\glreass
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnwin3,fnerror,fnTos,fnLbl,fnCmdKey,fnAcs
00050   fntop(program$,"Reassign Transaction Addresses")
00060   on error goto Ertn
00080   dim ta(2),tr1$*70,wrd1$(2)*35,fil$(12,4)
00150 ! ______________________________________________________________________
00160 MENU1: ! 
00170   fnTos(sn$="glreorg") !:
        mylen=20: mypos=mylen+3 : right=1
00180   fnLbl(1,1,"If you get errors trying to access general ledger")
00190   fnLbl(2,1,"transaction, this option might help.  It")
00200   fnLbl(3,1,"will reassign all transactions back to the ")
00210   fnLbl(4,1,"correct general ledger accounts.")
00220   fnLbl(5,1,"Take Next to continue with the reassign process.")
00230   fnCmdKey("&Next",1,1,0,"Reassigns the general ledger transactions.")
00240   fnCmdKey("&Cancel",5,0,1,"Returns to menu without processing.")
00250   fnAcs(sn$,0,mat resp$,ckey)
00260   if ckey=5 then goto XIT
00270   if ckey=1 then gosub REORG
00280   goto XIT
00290 REORG: ! r:
00300   open #1: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.H[cno],Shr",internal,outIn,keyed 
00310   open #2: "Name=[Q]\GLmstr\GLTRANS.H[cno],Shr",internal,outIn,relative 
00320   L320: read #1,using L330: mat ta eof L360
00330   L330: form pos 333,2*pd 3
00340   rewrite #1,using L330: 0,0
00350   goto L320
00360   L360: lr2=lrec(2)
00370   rewrite #2,using L470,rec=1: lr2
00380   for j=1 to lr2
00390     read #2,using L400,rec=j: k$,nta noRec L480
00400     L400: form pos 1,c 12,pos 71,pd 3
00410     read #1,using L330,key=k$: mat ta nokey L480
00420     if ta(1)=0 then ta(1)=j
00430     if ta(2)>0 then rewrite #2,using L470,rec=ta(2): j
00440     ta(2)=j
00450     rewrite #1,using L330,key=k$: mat ta
00460     rewrite #2,using L470,rec=j: 0
00470     L470: form pos 71,pd 3
00480     L480: !
00485   next j
00490 return ! /r
00500 ! ______________________________________________________________________
00510 XIT: fnxit
00520 ! ______________________________________________________________________
00530 ! 
00540 ERTN: fnerror(program$,err,line,act$,er_out$)
00550   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00560   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00570   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00580 ERTN_EXEC_ACT: execute act$ : goto ERTN
