00010 ! Replace S:\acsUB\conversion\ubadrbil-cnv
00020 ! converts adrbil as well as ask cno and write it so other conversion programs don't have to ask it.
00030   def library fnub_cnv_adrbil
00040     library 'S:\Core\Library': fncno,fnerror,fnstatus,fnCopy,fnindex_it
00050     on error goto ERTN
00060     fnstatus('Converting Alternate Billing Address file (S:\acsUB\conversion\ubadrbil-cnv)')
00070     dim a$(4)*30,cap$*128,z$*10,ab$(4)*30
00080 ! ______________________________________________________________________
00090 ! fntop("S:\acsUB\conversion\ubadrbil-cnv",cap$="adrbil-cnv")
00100     fncno(cno)
00110 ! note!!!!   may have to change a$(3) to a$(4), etc and form on line 220
00120 ! 
00130     if ~exists(env$('Q')&"\UBmstr\UBAdrBil.h"&env$('cno')) then ! move old files to "env$('temp')&"\temp."&session$&"orary file before creating new file with same name
00132       fnCopy(env$('Q')&"\UBmstr\UBAdrBil.h"&env$('cno'),env$('temp')&"\temp."&session$)
00134     end if 
00140     if exists(env$('Q')&"\UBmstr\UBAdrBil.h"&env$('cno'))=0 then goto BUILD_FILES
00142     open #3: "Name="&env$('Q')&"\UBmstr\ubMaster.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,outin,keyed ioerr OPN_CUST
00144     goto L160
00150 OPN_CUST: ! 
00152     open #3: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,outin,keyed 
00160 L160: ! 
00162     open #1: "Name="&env$('temp')&"\temp."&session$&"",internal,input,relative 
00170     open #2: "Name="&env$('temp')&"\tmp_x_"&session$&",RecL=130,Replace",internal,outin,relative 
00180 L180: ! 
00182     read #3,using "form pos 1,c 10,pos 385,pd 3": z$,bra eof END1
00190     if bra=0 then goto L180
00200     if rln(1)=100 then goto L210 else goto L230
00210 L210: ! 
00212     read #1,using 'Form POS 11,3*C 30',rec=bra: a$(1),a$(2),a$(3) norec L180
00220     goto L240
00230 L230: read #1,using 'Form POS 11,4*C 30',rec=bra: mat a$ norec L180
00240 L240: if trim$(a$(1))="" and trim$(a$(2))="" and trim$(a$(3))="" and trim$(a$(4))="" then goto L180
00250     write #2,using 'Form POS 1,C 10,4*C 30,': z$,mat a$
00260     goto L180
00270 ! ______________________________________________________________________
00280 END1: close #1: 
00290     close #2: 
00300     fnCopy(env$('temp')&"\tmp_x_"&session$,env$('Q')&"\UBmstr\UBAdrBil.h"&env$('cno'))
00310     if ~fnindex_it(env$('Q')&"\UBmstr\UBAdrBil.h"&env$('cno'),env$('Q')&"\UBmstr\adrIndex.h"&env$('cno'),"1 10") then goto XIT ! ,Replace,DupKeys -n" ioerr XIT
00320     execute "Free "&env$('temp')&"\temp."&session$
00325     close #3: ioerr ignore
00330     execute "Free "&env$('Q')&"\UBmstr\ubMaster.h"&env$('cno') ioerr ignore
00340     goto XIT
00350 BUILD_FILES: ! build files if don't exist
00360     open #2: "Name="&env$('Q')&"\UBmstr\UBAdrBil.h"&env$('cno')&",RecL=130,Replace",internal,outin,relative 
00370     close #2: 
00380     fnindex_it(env$('Q')&"\UBmstr\UBAdrBil.h"&env$('cno'),env$('Q')&"\UBmstr\adrIndex.h"&env$('cno'),"1 10") ! execute "Index "&env$('Q')&"\UBmstr\UBAdrBil.h"&env$('cno')&' '&env$('Q')&"\UBmstr\adrIndex.h"&env$('cno')&" 1,10,Replace,DupKeys -n"
00390     return 
00410 XIT: fnend 
00418 IGNORE: continue 
76220 ! <updateable region: ertn>
76240 ERTN: fnerror(program$,err,line,act$,"xit")
76260   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
76280   if uprc$(act$)="PAUSE" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT ! if env$("ACSDeveloper")<>"" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
76300   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
76320 ERTN_EXEC_ACT: execute act$ : goto ERTN
76340 ! </updateable region: ertn>
