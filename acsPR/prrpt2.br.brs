00010 ! Replace S:\acsPR\prRpt2
00020 ! Payroll Report File - Add Records
00030 !
00040   library 'S:\Core\Library': fntop,fnxit, fnoldmsgbox,fncno,fnerror
00045   fntop("S:\acsPR\prRpt2",cap$="Design Reports")
00050   on error goto Ertn
00060 !
00070   dim rt$*78,ch$(2)*132,psc(100),inp(20),pp(20),ti(20)
00080   dim io1$(9),fb$(20),io2$(60),rptemp(20),tempch$(4)*256
00090   dim temp(20,3),cnam$*40,cap$*128,response$(5)*1,cap$*128,msgline$(2)*60
00100 !
00120   fncno(cno,cnam$)
00130   for z=1 to 20
00140     z$=str$(z+1)
00150     c$=str$(z+1)
00160     fb$(z)=z$&",40,N 13.3,UT,N"
00170     io2$((z-1)*3+1)=c$&",15,Nz 3,UT,N"
00180     io2$((z-1)*3+2)=c$&",38,Nz 3,UT,N"
00190     io2$((z-1)*3+3)=c$&",62,N 1,UT,N"
00200   next z
00210   open #1: "Name=[Q]\PRmstr\PRReport.h[cno],KFName=[Q]\PRmstr\prrptidx.h[cno],Shr",internal,outIn,keyed 
00220 !
00230 SCR1: pr newpage
00240   close #101: ioerr L250
00250 L250: open #101: "SRow=4,SCol=6,ERow=22,ECol=74,Border=Dr,Caption=<"&cap$,display,outIn 
00260   pr #101,fields "02,2,Cr 14,N": "Report Number:"
00270   pr #101,fields "03,2,Cr 14,N": "Report Title:"
00280   pr #101,fields "05,2,Cc 66,R,N": "Column Headings"
00290   pr #101,fields "06,2,C 66,N": "    5   10   15   20   25   30   35   40   45   50   55   60    66"
00300   pr #101,fields "08,2,C 66,N": "  70   75   80   85   90   95  100  105  110  115  120  125    132"
00310   pr #101,fields "10,2,C 66,N": "    5   10   15   20   25   30   35   40   45   50   55   60    66"
00320   pr #101,fields "12,2,C 66,N": "  70   75   80   85   90   95  100  105  110  115  120  125    132"
00330   pr #101,fields "16,2,Cr 48,N": "Item Number for pr Selection (blank for all):"
00340   pr #101,fields "17,2,Cr 50,N": "Summarize Departmental Records (Y/N): N"
00350   pr #101,fields "18,2,Cr 50,N": "Use Condensed pr (Y/N): N"
00360   io1$(1)="02,17,Nz 2,UT,N"
00370   io1$(2)="03,17,C 40,UT,N" ! used to be 78... but john decided no one ever need one that long!
00380   io1$(3)="07,2,C 66,UT,N"
00390   io1$(4)="09,2,C 66,UT,N"
00400   io1$(5)="11,2,C 66,UT,N"
00410   io1$(6)="13,2,C 66,UT,N"
00420   io1$(7)="16,51,N 3,UT,N"
00430   io1$(8)="17,51,Cu 1,UT,N"
00440   io1$(9)="18,51,Cu 1,UT,N"
00450   pr f "23,30,C 09,B,1": "Next (F1)"
00460   pr f "23,41,C 09,B,5": "Exit (F5)"
00470 L470: input #101,fields mat io1$: rn,rt$,mat tempch$,ips,sd$,cp$ conv L470
00480   if ce>0 then io1$(ce)(ce1:ce2)="U": ce=0
00490   if cmdkey>0 then goto L560 else ce=curfld
00500 L500: ce=ce+1: if ce>udim(io1$) then ce=1
00510 L510: io1$(ce)=rtrm$(io1$(ce)) : ce1=pos(io1$(ce),"U",1) : if ce1=0 then goto L500
00520   ce2=ce1+1 : io1$(ce)(ce1:ce1)="UC" : goto L470
00530 CONV1: if ce>0 then io1$(ce)(ce1:ce2)="U"
00540   ce=cnt+1
00550 ERR1: pr f "24,78,C 1": bell : goto L510
00560 L560: if cmdkey=5 then goto DONE
00570   read #1,using L580,key=lpad$(str$(rn),2),release: rn nokey L640
00580 L580: form pos 1,n 2
00590 ! pr f "2,40,C 35,N": "This Report Number Already Exists"
00600   msgline$(1)="Report Number "&str$(rn)&" already exists"
00610   msgline$(2)="Please use a different Report Number."
00620   fnoldmsgbox(mat resonse$,cap$,mat msgline$,1)
00630   ce=1 : goto ERR1
00640 L640: if ips<0 or ips>126 then goto L470
00650   if ips>1 and ips<6 then goto L470
00660   if sd<0 or sd>1 then goto L470
00670   if sd$="Y" then sd=1 else sd=0
00680   if sd$<>"Y" and sd$<>"N" then ce=8 : goto ERR1
00690   if cp$="Y" then cp=1 else cp=0
00700   if cp$<>"Y" and cp$<>"N" then ce=9 : goto ERR1
00710   if cp<0 or cp>1 then goto L470
00720   ch$(1)=tempch$(1)&tempch$(2)
00730   ch$(2)=tempch$(3)&tempch$(4)
00740   if rn=0 then ce=1 : goto ERR1
00750   if ips=0 then goto L930
00760   pr newpage
00770   for w=1 to 5
00780     for j=1 to 20
00790       pr f str$(j+1)&",5,C 30,N": "PRINT SELECTION CRITERIA"
00800     next j
00810 L810: input fields mat fb$: mat rptemp conv L810
00820     on w goto L830,L850,L850,L850,L850
00830 L830: k=0
00840     goto L860
00850 L850: k=k+20
00860 L860: for q=1 to 20
00870       if rptemp(q)=0 then goto L930
00880       psc(q+k)=rptemp(q)
00890     next q
00900     mat rptemp=(0)
00910     pr newpage
00920   next w
00930 L930: pr newpage
00940   close #102: ioerr L950
00950 L950: open #102: "SRow=2,SCol=9,ERow=23,ECol=72,Border=Sr,Caption=<"&cap$,display,outIn 
00960   pr #102: newpage
00970   for j=1 to 20
00980     pr #102,fields str$(j+1)&",2,C 12,N": "Item Number:"
00990     pr #102,fields str$(j+1)&",22,C 15,N": "Print Position:"
01000     pr #102,fields str$(j+1)&",45,C 16,N": "Total this Item:"
01010   next j
01020   pr f "24,30,C 09,B,1": "Save (F1)"
01030   pr f "24,40,C 11,B,5": "Cancel (F5)"
01040 L1040: input #102,fields mat io2$: mat temp conv CONV2
01050   if ce>0 then io2$(ce)(ce1:ce2)="U": ce=0
01060   if cmdkey>0 then goto L1130 else ce=curfld
01070 L1070: ce=ce+1: if ce>udim(io2$) then ce=1
01080 L1080: io2$(ce)=rtrm$(io2$(ce)) : ce1=pos(io2$(ce),"U",1) : if ce1=0 then goto L1070
01090   ce2=ce1+1 : io2$(ce)(ce1:ce1)="UC" : goto L1040
01100 CONV2: if ce>0 then io2$(ce)(ce1:ce2)="U"
01110   ce=cnt+1
01120 ERR2: pr f "24,78,C 1": bell : goto L1080
01130 L1130: ! 
01140   if cmdkey=5 then goto SCR1
01150   for j=1 to 20
01160     if temp(j,1)>126 then ce=j*3+1 : goto CONV2 ! Goto 1040 ! j*3+1
01170     if temp(j,2)>132 then ce=j*3+2 : goto CONV2 ! Goto 1040 ! j*3+2
01180     if temp(j,3)<0 or temp(j,3)>1 then ce=j*3+3 : goto CONV2 ! Goto 1040
01190     inp(j)=temp(j,1)
01200     pp(j)=temp(j,2)
01210     ti(j)=temp(j,3)
01220   next j
01230   write #1,using L1240: rn,rt$,mat ch$,ips,sd,cp,mat psc,mat inp,mat pp,mat ti
01240 L1240: form pos 1,n 2,c 78,2*c 132,n 3,2*n 1,100*pd 6.3,40*pd 2,20*n 1
01250   goto SCR1
01260 DONE: pr newpage
01270   close #1: 
01280   open #99: "Name=PROC."&wsid$&",SIZE=0,Replace",display,output 
01290 L1290: form pos 1,c 70
01300   pr #99,using L1290: "CLEAR"
01310   pr #99,using L1290: "PROCERR RETURN"
01320   pr #99,using L1290: "Index [Q]\PRmstr\PRReport.h[cno]"&' '&"[Q]\PRmstr\prrptidx.h[cno] 1 2 Replace DupKeys"
01330   pr #99,using L1290: "PROC S:\acsPR\PRMENU"
01340   close #99: 
01350   chain "proc=PROC."&wsid$&""
01360 !
01370 XIT: fnxit
01380 !
01390 ! <Updateable Region: ERTN>
01400 ERTN: fnerror(program$,err,line,act$,"xit")
01410   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01420   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01430   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01440 ERTN_EXEC_ACT: execute act$ : goto ERTN
01450 ! /region
01460 !
