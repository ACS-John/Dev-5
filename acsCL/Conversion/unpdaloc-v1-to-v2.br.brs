00010 ! Replace S:\acsCL\Conversion\UnPdAloc-v1-to-v2
00020 ! converts the CL UnPdAloc file from version 0 or version 1 to Version 2
00030   def library fnunpdaloc_v1_to_v2
00040     library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnmsgbox,fnwait,fnCopy,fnindex_it,fnstatus
00050     on error goto ERTN
00060 ! ______________________________________________________________________
00070     dim cnam$*40,cap$*128,message$*40,msgline$(6)*48,response$(5)*1
00080 ! ______________________________________________________________________
00090     fncno(cno,cnam$)
00100     cap$="Checkbook update UnPdAloc from v1 to v2"
00120     fnstatus("Payment Allocation file until it is updating from v1 to v2")
00180     open #unpdaloc=1: "Name="&env$('Q')&"\CLmstr\UnPdAloc.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\UAIdx1.h"&env$('cno'),internal,outin,keyed 
00182 !      close #unpdaloc:
00190     if version(unpdaloc)=2 then let fnstatus("UnPdAloc is already version 2") : goto XIT
00200     version(unpdaloc,2)
00210     close #unpdaloc: 
00220 ! 
00230 ! change the record length
00240     fnCopy(env$('Q')&"\CLmstr\UnPdAloc.h"&env$('cno'),env$('Q')&"\CLmstr\UnPdAloc.h"&env$('cno'),67)
00270 ! 
00280 ! make sure the Key is right justified
00290     open #unpdaloc=1: "Name="&env$('Q')&"\CLmstr\UnPdAloc.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\UAIdx1.h"&env$('cno'),internal,outin,keyed 
00300     for j=1 to lrec(unpdaloc)
00310       read #unpdaloc,using 'Form Pos 1,C 8,c 12',rec=j: vn$,iv$ norec L330
00315       vn$=lpad$(rtrm$(vn$),8): iv$=lpad$(rtrm$(iv$),12)
00320       rewrite #unpdaloc,using 'Form Pos 1,Cr 8,c 12',rec=j: vn$,iv$
00330 L330: next j
00340     close #unpdaloc: 
00350     fnindex_it(env$('Q')&"\CLmstr\UnPdAloc.h"&env$('cno'),env$('Q')&"\CLmstr\UAIdx1.h"&env$('cno'),"9 12")
00360     fnindex_it(env$('Q')&"\CLmstr\UnPdAloc.h"&env$('cno'),env$('Q')&"\CLmstr\UAIdx2.h"&env$('cno'),"1 20")
00370     goto XIT
00380 ! ______________________________________________________________________
00390 ! <Updateable Region: ERTN>
00400 ERTN: fnerror(program$,err,line,act$,"xit")
00410     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00420     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00430     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00440 ERTN_EXEC_ACT: execute act$ : goto ERTN
00450 ! /region
00460 ! ______________________________________________________________________
00470 XIT: fnend 
00480 ! ______________________________________________________________________
