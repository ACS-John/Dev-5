00020 ! 
00030   on error goto ERTN
00040   library 'S:\Core\Library': fntop,fnxit, fnerror
00050   fntop(program$,cap$="Build Files")
00070 ! 
00080   dim ta(25,2),fb(25),tr(6),a$*5
00090   pr newpage
00100   pr f "8,25,c 30,r,n": "********  WARNING  ********"
00110   pr f "11,10,c 70": "THIS PROGRAM WILL ERASE ALL CLIENT RECORDS FROM THIS FILE."
00120   pr f "13,5,C 75": "ENTER PASSWORD TO CONTINUE; ELSE PRESS ENTER TO RETURN TO CLIENT MENU."
00130 L130: input fields "15,10,C 5,IE,n": a$ conv L130
00140   if uprc$(a$)="THINK" then goto L150 else goto L610
00150 L150: pr newpage
00160   open #1: "Name=S:\Core\Data\acsllc\CLmstr.h[cno]",internal,input ioerr L180
00170   close #1,free: 
00180 L180: open #1: "Name=S:\Core\Data\acsllc\CLmstr.h[cno],SIZE=0,RecL=534",internal,output 
00190   close #1: 
00200   open #2: "Name=S:\Core\Data\acsllc\TMTrans.h[cno]",internal,output ioerr L220
00210   close #2,free: 
00220 L220: open #2: "Name=S:\Core\Data\acsllc\TMTrans.h[cno],SIZE=0,RecL=86",internal,output 
00230   write #2,using L240: " "," ",0,0,0,0,0,0,0,0,"","",1,""
00240 L240: form pos 1,c 5,c 9,2*pd 3.2,pd 4.2,n 6,n 2,pd 2,pd 1,n 2,c 4,c 12,pd 3,c 30
00250   close #2: 
00260   open #3: "Name=S:\Core\Data\acsllc\TMTRAddr.h[cno]",internal,output ioerr L280
00270   close #3,free: 
00280 L280: open #3: "Name=S:\Core\Data\acsllc\TMTRAddr.h[cno],SIZE=0,RecL=175",internal,output 
00290   ta(1,1)=1
00300   write #3,using L310: mat ta,mat fb
00310 L310: form pos 1,50*pd 3,25*n 1
00320   close #3: 
00330   open #4: "Name=S:\Core\Data\acsllc\TMYTDTrn.h[cno]",internal,output ioerr L350
00340   close #4,free: 
00350 L350: open #4: "Name=S:\Core\Data\acsllc\TMYTDTrn.h[cno],SIZE=0,RecL=56",internal,output 
00360   close #4: 
00370   open #5: "Name=S:\Core\Data\acsllc\TMINVREG.h[cno]",internal,output ioerr L390
00380   close #5,free: 
00390 L390: open #5: "Name=S:\Core\Data\acsllc\TMINVREG.h[cno],SIZE=0,RecL=45",internal,output 
00400   close #5: 
00410   open #6: "Name=S:\Core\Data\acsllc\ARTrans.h[cno]",internal,output ioerr L430
00420   close #6,free: 
00430 L430: open #6: "Name=S:\Core\Data\acsllc\ARTrans.h[cno],SIZE=0,RecL=60",internal,output 
00440   write #6,using L450: "",mat tr,"",1
00450 L450: form pos 1,c 17,n 6,2*pd 5.2,pd 2,2*n 1,c 20,pd 3
00460   close #6: 
00470 !   open #7: "Name=S:\Core\Data\acsllc\ARMoTran.h[cno]",internal,output ioerr L490
00480 !   close #7,free: 
00490 ! L490: open #7: "Name=S:\Core\Data\acsllc\ARMoTran.h[cno],SIZE=0,RecL=239",internal,output 
00500 !   close #7: 
00510   open #99: "Name=PROC."&wsid$,display,output 
00520   restore #99: 
00530   pr #99: "CLEAR"
00540   pr #99: "PROCERR RETURN"
00550   pr #99: "Index S:\Core\Data\acsllc\CLmstr.h[cno]"&' '&"S:\Core\Data\acsllc\CLIndex.h[cno] 1 5 REPLACE DupKeys"
00560   pr #99: "Index S:\Core\Data\acsllc\CLmstr.h[cno]"&' '&"S:\Core\Data\acsllc\CLIndx2.h[cno] 6 28 REPLACE DupKeys"
00570   pr #99: "LOAD S:\Time Management\Client Legacy"
00580   pr #99: "RUN"
00590   close #99: 
00600   chain "PROC=PROC."&wsid$
00610 L610: chain "S:\Time Management\Client Legacy"
00620 ERTN: if err=61 then pr f "23,3,C 75,N": "THIS PROGRAM IS TRYING TO ACCESS A RECORD THAT IS IN USE!" else goto L640
00630   goto L680
00640 L640: pr newpage
00650   if err=4148 then pr f "23,3,C 78,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L670
00660   goto L680
00670 L670: pr f "23,3,C 75,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
00680 L680: pr f "24,3,C 70,N": "PRESS ENTER TO RETRY; ELSE ENTER  Q  TO QUIT"
00690   input fields "24,60,C 1,N": quitcode$
00700   if rtrm$(uprc$(quitcode$))="Q" then goto XIT
00710   pr f "23,3,C 78,N": ""
00720   pr f "24,3,C 78,N": ""
00730   retry 
00740 XIT: fnxit
