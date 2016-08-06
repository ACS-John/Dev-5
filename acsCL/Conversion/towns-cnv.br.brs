00010 ! REPLACE R:\acsCL\Conversion\towns-CNV
00020 ! not sure - but it's old
00030 ! ______________________________________________________________________
00040   library 'R:\Core\Library': fnxit,fnerror,fncno
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070 ! fntop
00080   let fncno(cno)
00090   print newpage
00100   print fields "10,10,C 60": "ENTER COMPANY NUMBER:"
00110 L110: rinput fields "10,40,N 2,UE,N": cno conv L110
00120   if cno=0 then goto XIT
00140   dim vn$*8,nam$*30,ad1$*30,ad2$*30,csz$*30,ss$*11,ph$*12,a$(60)*262
00150   dim gl(3),ta(2),aa(2),gl$(5)*12,gld$(5)*20,gla(5),id$*20
00160   dim a(9),dt(5),cd(4)
00170   open #11: "Name=townsupr.asc,RecL=26200",external,input 
00180   open #21: "Name=Q:\CLmstr\PAYMSTR.H"&str$(cno)&",SIZE=0,RecL=164,REPLACE",internal,outin,relative 
00190   open #22: "Name=Q:\CLmstr\PayAlloc.H"&str$(cno)&",SIZE=0,RecL=56,REPLACE",internal,outin,relative 
00200   open #23: "Name=Q:\CLmstr\PayTrans.H"&str$(cno)&",SIZE=0,RecL=102,REPLACE",internal,outin,relative 
00210   open #24: "Name=Q:\CLmstr\UnPdAloc.H"&str$(cno)&",SIZE=0,RecL=70,REPLACE",internal,outin,relative 
00220   mat ta=(0)
00230 L230: read #11,using 'Form C 262': mat a$ eof L360
00240   for j=1 to 60
00250     let x=x+2
00260     let vn$=lpad$(str$(x),8)
00270     let nam$=rtrm$(a$(j)(1:30))&" "&rtrm$(a$(j)(31:60))
00280 ! LET AD1$=A$(J)(121:170)(1:30)
00290     let ad1$=a$(j)(171:230)(1:30)
00300     if len(rtrm$(a$(j)(231:250)))<=15 then let csz$=rtrm$(a$(j)(231:250))&", "&rtrm$(a$(j)(251:252))&" "&rtrm$(a$(j)(253:262))
00310     if len(rtrm$(a$(j)(231:250)))>15 then let csz$=rtrm$(a$(j)(231:250))&", "&rtrm$(a$(j)(251:252))&" "&rtrm$(a$(j)(253:257))
00320     let lr1=lrec(21)+1
00330     write #21,using 'Form POS 1,C 8,4*C 30,PD 5.2,N 2,C 11,2*PD 3,C 12',rec=lr1: vn$,nam$,ad1$,ad2$,csz$,ytdp,typ,ss$,mat ta,ph$
00340   next j
00350   goto L230
00360 L360: close #21: 
00370   close #22: 
00380   close #23: 
00390   close #24: 
00400   execute "INDEX Q:\CLmstr\PAYMSTR.H"&str$(cno)&",Q:\CLmstr\PAYIDX1.H"&str$(cno)&",1,8,REPLACE,DupKeys"
00410   execute "INDEX Q:\CLmstr\PAYMSTR.H"&str$(cno)&",Q:\CLmstr\PayIdx2.H"&str$(cno)&",9,28,REPLACE,DupKeys"
00420   execute "INDEX Q:\CLmstr\PayTrans.H"&str$(cno)&",Q:\CLmstr\UNPDIDX1.H"&str$(cno)&",1,20,REPLACE,DupKeys"
00430 XIT: let fnxit
00440 ! ______________________________________________________________________
00450 ! <Updateable Region: ERTN>
00460 ERTN: let fnerror(cap$,err,line,act$,"xit")
00470   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00480   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00490   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00500 ERTN_EXEC_ACT: execute act$ : goto ERTN
00510 ! /region
00520 ! ______________________________________________________________________
