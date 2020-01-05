00010 ! formerly S:\acsCL\UnPdInv2
00020 ! Unpaid Invoice Summary
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fndat,fnerror,fntop,fnxit,fnwait
00050   on error goto Ertn
00060 ! ______________________________________________________________________
00070   dim dat$*20,vnam$*30,de$*31,gl(3),ade$*50
00080   dim gl$*12,gd$*50
00090 ! ______________________________________________________________________
00100   fntop(program$)
00110   fndat(dat$)
00120   fnwait
00140   open #paytrans=4: "Name=[Q]\CLmstr\PayTrans.h[cno],KFName=[Q]\CLmstr\unpdidx1.h[cno],Shr",internal,input,keyed 
00150   open #glmstr=7: "Name=[Q]\CLmstr\GLmstr.H[cno],KFName=[Q]\CLmstr\GLINDEX.H[cno],Shr",internal,input,keyed 
00160   open #unpdaloc=8: "Name=[Q]\CLmstr\UnPdAloc.H[cno],KFName=[Q]\CLmstr\UAIdx2.h[cno],Shr",internal,input,keyed 
00170   open #work=9: "Name="&env$('temp')&'\work.tmp,KFName='&env$('temp')&'\addr.tmp,KPS=1,KLN=12,RecL=68,Replace',internal,outIn,keyed 
00180   open #paymstr=13: "Name=[Q]\CLmstr\PayMstr.H[cno],KFName=[Q]\CLmstr\PayIdx1.H[cno],Shr",internal,outIn,keyed 
00190   gosub HDR
00200 READ_PAYTRANS: ! 
00210   read #paytrans,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,G 1': vn$,iv$,ivd,dd,po$,de$,upa,cde eof END1
00220   read #paymstr,using 'Form POS 9,C 30',key=vn$: vnam$ nokey L230
00230 L230: pr #255,using 'Form POS 1,2*C 31,N 10.2': vnam$,de$,upa !:
        ! this line would love to be a skip 0 one day
00240   t1+=upa
00250   restore #unpdaloc,key>=vn$&iv$: nokey READ_PAYTRANS
00260 READ_UNPDALOC: ! 
00270   read #unpdaloc,using 'Form Pos 1,C 8,c 12,N 3,N 6,N 3,PD 5.2,C 30': unvn$,univ$,mat gl,amt,ade$ eof READ_PAYTRANS !:
        if vn$<>unvn$ or iv$<>univ$ then goto READ_PAYTRANS
00280   if amt=0 then goto READ_UNPDALOC
00290   pr #255,using 'FORM POS 74,N 10.2,N 4,N 6,N 3': amt,mat gl pageoflow NEWPGE
00300   gl$=cnvrt$("N 3",gl(1))&cnvrt$("N 6",gl(2))&cnvrt$("N 3",gl(3))
00310   read #work,using 'FORM POS 63,PD 6.2',key=gl$: ga nokey L360
00320   ga+=amt
00330   rewrite #work,using 'FORM POS 63,PD 6.2',key=gl$: ga
00340   goto READ_UNPDALOC
00350 ! ______________________________________________________________________
00360 L360: gd$="" !:
        read #glmstr,using 'Form Pos 13,C 50',key=gl$: gd$ nokey L370
00370 L370: write #work,using 'FORM POS 1,C 12,C 50,PD 6.2': gl$,gd$,amt
00380   goto READ_UNPDALOC
00390 ! _______________________________________________________________
00400 NEWPGE: pr #255: newpage: gosub HDR : continue 
00410 HDR: ! 
00420   fnopenprn
00430   pr #255,using 'FORM POS 1,C 8,CC 86': date$,env$('cnam')
00440   pr #255,using 'FORM POS 1,C 8,POS 40,C 40': time$,"Unpaid Invoice Summary"
00450   pr #255,using 'Form POS 1,C 4,N 4,CC 86': "Page",pg+=1,dat$
00460   pr #255: "Payee Name                      Description                    Ck-Amount  GL-Amount    GL-Number"
00470 L470: pr #255: "______________________________ ______________________________ __________ __________ ____________"
00480   return 
00490 ! ______________________________________________________________________
00500 END1: ! 
00510   gosub L470
00520   pr #255,using 'FORM POS 50,C 10,N 12.2': "Total",t1 !:
        pr #255: ""
00530   pr #255: " GL-Number    Description                                            Amount  "
00540   pr #255: "____________  __________________________________________________  __________"
00550   restore #work,search>="": nokey L600
00560 READ_WORK: ! 
00570   read #work,using 'FORM POS 1,C 12,C 50,PD 6.2': gl$,gd$,ga eof L600
00580   pr #255,using 'FORM POS 1,C 14,C 50,N 12.2': gl$,gd$,ga
00590   goto READ_WORK
00600 L600: fncloseprn
00610   goto XIT
00620 ! ______________________________________________________________________
00630 XIT: fnxit
00640 ! ______________________________________________________________________
00650 ! <Updateable Region: ERTN>
00660 ERTN: fnerror(program$,err,line,act$,"xit")
00670   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00680   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00690   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00700 ERTN_EXEC_ACT: execute act$ : goto ERTN
00710 ! /region
00720 ! ______________________________________________________________________
