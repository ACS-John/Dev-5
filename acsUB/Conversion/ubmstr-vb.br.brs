00010 ! replace S:\acsUB\conversion\UBmstr-vb
00020 ! always start conversion process with bld_trans
00030   def library fnub_cnv_ubmstr_vb
00040     library 'S:\Core\Library': fnxit,fnerror,fnpause,fnCopy,fnindex_it,fnub_index_customer,fnstatus,fnFree
00050     on error goto ERTN
00060     dim b(11),a(7),d(15),alpha$*7,f2$*12,extra(23),extra$(11)*30,ba(12)
00070     dim custname$*30,badr(2)
00080     dim z$*10,e$(4)*30,f$(3)*12,c(4),g(12),adr(2),alp$*7,gb(10)
00090     dim x$*10,p$*10,rw4(22,13)
00100     fnstatus('Customer File Conversion (S:\acsUB\conversion\ubmstr-vb)')
00120 ! 
00122 ! fnCopy(env$('Q')&"\UBmstr\ubcoinfo.h"&env$('cno'),env$('Q')&"\UBmstr\Company.h"&env$('cno'),133) ! this should already be done.
00132     if exists(env$('Q')&"\UBmstr\ubMaster.h"&env$('cno')) then 
00133       fnFree(env$('Q')&"\UBmstr\customer.h"&env$('cno'))
00134       fnCopy(env$('Q')&"\UBmstr\ubMaster.h"&env$('cno'),env$('Q')&"\UBmstr\Customer.h"&env$('cno'))
00136       fnFree(env$('Q')&"\UBmstr\ubMaster.h"&env$('cno'))
00138     end if 
00150     fnCopy(env$('Q')&"\UBmstr\Customer.h"&env$('cno'),env$('Q')&"\UBmstr\Customer.h"&env$('cno'),2067)
00180     fnub_index_customer
00230     fnindex_it(env$('Q')&"\UBmstr\UBAdrBil.h"&env$('cno'),env$('Q')&"\UBmstr\adrIndex.h"&env$('cno'),"1 10")
00240 ! 
00250     open #h_customer:=1: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno'),internal,outin,keyed 
00260     if version(1)=1 then goto XIT
00270     open #81: "Name="&env$('Q')&"\UBmstr\BudMstr.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\BudIdx1.h"&env$('cno')&",Shr,Use,RecL=80,KPs=1,KLn=10",internal,outin,keyed 
00280     open #82: "Name="&env$('Q')&"\UBmstr\BudTrans.h"&env$('cno')&",Shr,Use,RecL=149",internal,outin,relative 
00290 L290: read #h_customer,using F_CUSTOMER: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,mat adr,alp$,f$(2),f$(3),bra,mat gb,mat rw4,df$,dr$,dc$,da$,mat extra,mat extra$ eof L590
00300     form pos 1,c 10,4*c 30,c 12,7*pd 2,11*pd 4.2,4*pd 4,15*pd 5,pd 4.2,pd 4,12*pd 4.2,2*pd 3,c 7,2*c 12,pd 3,10*pd 5.2,78*pd 5,13*pd 4.2,13*n 6,156*pd 4.2,13*n 6,13*pd 4.2,c 1,c 9,c 2,c 17,n 2,n 7,2*n 6,n 9,pd 5.2,n 3,3*n 9,3*n 2,3*n 3,n 1,3*n 9,3*pd 5.2,c 30,7*c 12,3*c 30
00310     extra(1)=val(z$(1:2)) conv L290 ! route
00320     extra(2)=val(z$(3:7)) conv L290 ! sequence
00330     if extra(2)=0 then extra(2)=1 ! don't allow zero sequence #
00340     extra$(6)=f$(3) : f$(3)="" ! bulk sort code
00350     extra$(3)=f$(2) : f$(2)="" ! meter module #  (water meter serial)
00360 ! eXTRA(11)=B(6): b(6)=0 ! note payable amount
00370 ! extra(17)=c(4) : c(4)=0 ! final billing code
00380 ! extra(3)=d(5) : d(5)=0 ! date meter read current
00390 ! extra(4)=d(6): d(6)=0 ! date meter read prior
00400 ! d(7)=0
00410 ! eXTRA$(2)=STR$(D(8)): d(8)=0 ! phone #
00420 ! d(11)=0 ! city bank account will be in bank draft program
00430 ! dA$=STR$(D(12)): d(12)=0 ! customer bank account
00440 ! eXTRA$(7)=STR$(D(15)): d(15)=0 ! test cycle code
00444     d(13)=0 ! set # units to 0 if not used xxx-prb 05/18/11
00450     d(14)=0 ! don't know but has value on old system
00460 ! gB(5)=GB(3)+GB(4): gB(3)=GB(4)=0 ! add rep parts and rep labor together
00470 ! If G(5)=9.25 Then g(6)=9.25: gB(6)=GB(5): g(5)=0: gB(5)=0 ! KINCAID ONLY
00480 ! gB(9)=GB(7): gB(7)=0 ! move sfc for service 7 to service 9
00490 ! eXTRA(18)=D(7): d(7)=0 ! average sewer usage
00495 ! If env$('client')="Monticello" AND A(2)>9 Then eXTRA(18)=D(7)=0 ! don't average sewer rate codes 10 or greater
00500 ! a(6)=A(7) ! penalty codes (was only 1 code but charges listed seperate
00510 ! If A(2)>0 Then eXTRA(14)=D(13) ! make sewer units same as water units if have sewer
00520     for j=1 to udim(extra)
00522       if extra(j)<-99999 then extra(j)=0
00523     next j
00528     rewrite #h_customer,using F_CUSTOMER: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,mat adr,alp$,f$(2),f$(3),bra,mat gb,mat rw4,df$,dr$,dc$,da$,mat extra,mat extra$
00530 F_CUSTOMER: form pos 1,c 10,4*c 30,c 12,7*pd 2,11*pd 4.2,4*pd 4,15*pd 5,pd 4.2,pd 4,12*pd 4.2,2*pd 3,c 7,2*c 12,pd 3,10*pd 5.2,78*pd 5,13*pd 4.2,13*n 6,156*pd 4.2,13*n 6,13*pd 4.2,c 1,c 9,c 2,c 17,n 2,n 7,2*n 6,n 9,pd 5.2,n 3,3*n 9,3*n 2,3*n 3,n 1,3*n 9,3*pd 5.2,c 30,7*c 12,3*c 30
00540     if b(10)=0 then goto L580 ! updating the budget billing file
00550     mat badr=(0): ba(12)=b(10)
00560     write #81,using L570: z$,mat ba,mat badr
00570 L570: form pos 1,c 10,pd 4,12*pd 5.2,2*pd 3
00580 L580: goto L290
00590 L590: version(1,1)
00592     version(81,1)
00594     version(82,1)
00600     close #1: 
00610     close #81: ioerr ignore
00620     close #82: ! ioerr L640
00630     fnindex_it(env$('Q')&"\UBmstr\BudMstr.h"&env$('cno'),env$('Q')&"\UBmstr\BudIdx1.h"&env$('cno'), '1 10')
00640 ! L640: ! Goto 70
00650 XIT: fnend  ! chain "S:\acsUB\conversion\note-cnv" ! fnxit
00660 IGNORE: continue 
76220 ! <updateable region: ertn>
76240 ERTN: fnerror(program$,err,line,act$,"xit")
76260   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
76280   if uprc$(act$)="PAUSE" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT ! if env$("ACSDeveloper")<>"" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
76300   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
76320 ERTN_EXEC_ACT: execute act$ : goto ERTN
76340 ! </updateable region: ertn>
