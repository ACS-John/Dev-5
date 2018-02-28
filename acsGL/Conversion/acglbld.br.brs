00010 ! Replace S:\acsGL\acglBld.br
00020 ! build something
00030   def library fnacglbld
00040     library 'S:\Core\Library': fntop,fnxit, fnxit,fntop, fncno,fnerror,fnchain,fnindex_it
00050     on error goto ERTN
00060 ! ______________________________________________________________________
00070     dim cnam$*40
00080 ! ______________________________________________________________________
00090 ! fntop(program$,"CHANGE_ME")
00100     fncno(cno,cnam$)
00110     close #102: ioerr L120
00120 L120: open #102: "SRow=5,SCol=13,ERow=14,ECol=64,Border=SR,Caption=<Initial File Preparation",display,outIn 
00130     pr #102: newpage !:
          pr #102,fields "1,1,Cc 52,R,N": cnam$ !:
          pr #102,fields "2,1,Cc 52,R,N": "Company Number [cno]"
00140     pr #102,fields "4,1,Cc 52,N": " ******************   WARNING   ******************"
00150     pr #102,fields "6,1,Cc 52,N": " This selection will destroy all existing records"
00160     pr #102,fields "7,1,Cc 52,N": " in the GL Master and Transactions File."
00170     pr #102,fields "9,2,C 26,N": " Enter ERASE to continue:"
00180     pr f "15,35,C 09,B,5": "Exit (F5)"
00190 L190: input #102,fields "9,29,Cu 5,UT,N": pas$
00200     if cmdkey=5 then goto XIT
00210     if pas$><"ERASE" then goto L190
00220 ! ______________________________________________________________________
00230     open #1: "Name=[Q]\GLmstr\GLmstr.h[cno],Size=0,RecL=416,Replace",internal,output 
00240     close #1: 
00250     open #1: "Name=[Q]\GLmstr\glTrans.h[cno],Size=0,RecL=73,Replace",internal,output 
00260     write #1,using L270: 0,0,0,0,0,0,0," "," ",1
00270 L270: form pos 1,n 3,n 6,n 3,n 6,pd 6.2,2*n 2,c 12,c 30,pd 3
00280     close #1: 
00290     open #1: "Name=[Q]\GLmstr\AcTrans.h[cno],Size=0,RecL=72,Replace",internal,output 
00300     close #1: 
00310     open #1: "Name=[Q]\GLmstr\acglPgMn.h[cno],Size=0,RecL=58,Replace",internal,outIn,relative 
00320     for j=1 to 20 !:
            write #1,using 'Form POS 1,C 20,C 35,3*N 1',rec=j: "","",0,0,0 !:
          next j
00330     close #1: 
00340     open #1: "Name=[Q]\GLmstr\gl1099.h[cno]",internal,output ioerr L370
00350     close #1: 
00360     goto L390
00370 L370: open #1: "Name=[Q]\GLmstr\gl1099.h[cno],Size=0,RecL=127",internal,output 
00380     close #1: 
00390 L390: open #1: "Name=[Q]\GLmstr\glTr1099.h[cno]",internal,output ioerr L420
00400     close #1: 
00410     goto L460
00420 L420: open #1: "Name=[Q]\GLmstr\glTr1099.h[cno],Size=0,RecL=64",internal,output,relative 
00430     write #1,using L440,rec=1: "",0,0,"","",1
00440 L440: form pos 1,c 8,n 6,pd 5.2,c 12,c 30,pd 3
00450     close #1: 
00460 L460: open #1: "Name=[Q]\GLmstr\glBRec.h[cno]",internal,output ioerr L490
00470     close #1: 
00480     goto L510
00490 L490: open #1: "Name=[Q]\GLmstr\glBRec.h[cno],Size=0,RecL=68",internal,output 
00500     close #1: 
00510 L510: open #1: "Name=[Q]\GLmstr\PRmstr.h[cno]",internal,output ioerr L530
00520     goto L540
00530 L530: open #1: "Name=[Q]\GLmstr\PRmstr.h[cno],Size=0,RecL=280",internal,output 
00540 L540: close #1: 
00550     open #1: "Name=[Q]\GLmstr\acPrCks.h[cno]",internal,output ioerr L570
00560     goto L580
00570 L570: open #1: "Name=[Q]\GLmstr\acPrCks.h[cno],Size=0,RecL=110",internal,output,relative 
00580 L580: close #1: 
00590     execute "Index [Q]\GLmstr\GLmstr.h[cno]"&' '&"[Q]\GLmstr\GLIndex.h[cno] 1 12 Replace DupKeys -n"
00600     execute "Index [Q]\GLmstr\GLmstr.h[cno]"&' '&"[Q]\GLmstr\glIndx2.h[cno] 13 30 Replace DupKeys -n"
00610     execute "Index [Q]\GLmstr\gl1099.h[cno]"&' '&"[Q]\GLmstr\gl109Idx.h[cno] 1 8 Replace DupKeys -n"
00620     fnindex_it("[Q]\GLmstr\GLBREC.h[cno]","[Q]\GLmstr\GLRecIdx.h[cno]","1 24")
00630     execute "Index [Q]\GLmstr\PRmstr.h[cno]"&' '&"[Q]\GLmstr\PRINDEX.h[cno] 1 4 Replace DupKeys -n"
00640     execute "Index [Q]\GLmstr\AcTrans.h[cno]"&' '&"[Q]\GLmstr\AcTrIdx.h[cno] 1/71/17/13 12/2/2/4 Replace DupKeys -n"
00650     fnchain("S:\General Ledger\Accounts")
00660 ! ______________________________________________________________________
00670     goto XIT
00680 ! ______________________________________________________________________
00690 ! <Updateable Region: ERTN>
00700 ERTN: fnerror(program$,err,line,act$,"xit")
00710     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00720     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00730     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00740 ERTN_EXEC_ACT: execute act$ : goto ERTN
00750 ! /region
00760 ! ______________________________________________________________________
00770 XIT: fnend 
