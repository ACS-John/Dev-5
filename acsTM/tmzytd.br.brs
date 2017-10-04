00020 ! 
00030   on error goto L410
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fncno,fnerror,fnpedat$,fnprocess, fntos,fnlbl,fntxt,fnchk,fnqgl,fncmdset,fnacs,fnagl$,fnconsole,fnd1
00050   fntop(program$,cap$="Zero Year to Date")
00060   fncno(cno,cnam$)
00070   fnconsole(1)
00080 ! 
00090 ! 
00100   dim eno$*9,cytdhrs(10),ncytdhrs(20),cytdamt(10),ncytdamt(20),prg$*20
00110   dim cap$*128
00120 L120: pr newpage
00130   pr fields "8,25,c 30,r,n": "********  WARNING  ********"
00140   pr fields "11,5,C 75": "THIS PROGRAM ZEROS ALL YEAR-TO-DATE FIELDS IN THE EMPLOYEE MASTER FILE"
00150   pr fields "12,5,c 75": "AND THE SERVICE CODE FILE.  IT ALSO CLEARS THE YTD BILLING SUMMARY FILE."
00160   pr fields "14,8,c 70": "BE SURE YOU HAVE RUN ALL OF THE YEAR-END REPORTS BEFORE CONTINUING!"
00170   pr fields "16,8,c 62": "ENTER 1 TO CONTINUE; ELSE ENTER 2 TO RETURN TO THE SYSTEM MENU"
00180 L180: input fields "16,75,n 1,uE,n": a conv L180
00190   on a goto L200,END1 none L120
00200 L200: pr newpage
00210   pr fields "10,20,C 60,H,N": "ZERO YEAR TO DATE INFORMATION IN PROCESS"
00220   open #1: "Name="&env$('Q')&"\TMmstr\EMmstr.h"&str$(cno)&",KFName="&env$('Q')&"\TMmstr\EMINDEX.h"&str$(cno)&",Shr",internal,outin,keyed ioerr L410
00230 L230: read #1,using L240: eno$ eof L280 ioerr L410
00240 L240: form pos 1,c 9
00250   rewrite #1,using L260: mat cytdhrs,mat ncytdhrs,mat cytdamt,mat ncytdamt
00260 L260: form pos 158,30*pd 4.2,pos 428,30*pd 5.2
00270   goto L230
00280 L280: close #1: 
00290   open #1: "Name="&env$('Q')&"\TMmstr\TMYTDTrn.h"&str$(cno),internal,output ioerr L310
00300   close #1,free: 
00310 L310: open #1: "Name="&env$('Q')&"\TMmstr\TMYTDTrn.h"&str$(cno)&",SIZE=0,RecL=56",internal,output ioerr L410
00320   close #1: 
00330   open #1: "Name="&env$('Q')&"\TMmstr\SCMSTR.H"&str$(cno)&",KFName="&env$('Q')&"\TMmstr\SCIndex.H"&str$(cno)&",Shr",internal,outin,keyed ioerr L410
00340 L340: read #1,using L360: a1 eof L380 ioerr L410
00350   rewrite #1,using L360: 0,0
00360 L360: form pos 35,pd 4.2,pd 5.2
00370   goto L340
00380 L380: close #1: 
00390 END1: if uprc$(rtrm$(prg$))="S:\acsTM\EMAINT" then chain prg$
00400 XIT: let fnxit
00410 L410: if err=61 then pr fields "23,3,C 75,N": "THIS PROGRAM IS TRYING TO ACCESS A RECORD THAT IS IN USE!" else goto L430
00420   goto L470
00430 L430: pr newpage
00440   if err=4148 then pr fields "23,3,C 78,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L460
00450   goto L470
00460 L460: pr fields "23,3,C 75,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
00470 L470: pr fields "24,3,C 70,N": "PRESS ENTER TO RETRY; ELSE ENTER  Q  TO QUIT"
00480   input fields "24,60,C 1,N": quitcode$
00490   if rtrm$(uprc$(quitcode$))="Q" then goto L530
00500   pr fields "23,3,C 78,N": ""
00510   pr fields "24,3,C 78,N": ""
00520   retry 
00530 L530: goto XIT
