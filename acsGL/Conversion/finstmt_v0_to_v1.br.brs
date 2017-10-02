00010 ! Replace S:\acsGL\Conversion\FinStmt_v0_to_v1
00020 ! converts fin stmts files (all of them) !:
        ! from recl=79 to recl=83 and version 1
00030   def library fnfinstmt_v0_to_v1
00040     library 'S:\Core\Library': fntop,fnxit, fnerror,fnwait,fnmsgbox,fncno,fnstatus,fnindex_it
00060     on error goto ERTN
00070 ! ______________________________________________________________________
00080     dim cap$*128
00090     dim fil$(6),idx$(6)
00120     let fncno(cno)
00130 ! ______________________________________________________________________
00140     let fnstatus('Converting Financial Statement.')
20000 GOON: ! 
20020     let fil$(1)="acglfnSB": let idx$(1)="fnSBIndx" ! Balance Sheet
20040     let fil$(2)="acglfnSI": let idx$(2)="fnSIIndx" ! Income Statement
20060     let fil$(3)="acglfnSF": let idx$(3)="fnSFIndx" ! Funt Statement / Cash Flow
20080     let fil$(4)="acglfnSC": let idx$(4)="fnSCIndx" ! Secondary Balance Sheet
20100     let fil$(5)="acglfnSJ": let idx$(5)="fnSJIndx" ! Secondary Income Statement
20120     let fil$(6)="acglfnSG": let idx$(6)="fnSGIndx" ! Secondary Fund / Cash Flow
20140 ! 
20160     for j=1 to 6
20180       execute "Copy "&env$('Q')&"\GLmstr\"&fil$(j)&".h"&str$(cno)&' '&env$('temp')&"\WORK."&session$&" -83 -d -n" ioerr NEXT_J
20200       execute "Copy  "&env$('temp')&"\WORK."&session$&' '&env$('Q')&"\GLmstr\"&fil$(j)&".h"&str$(cno)&" -n"
20220       let fnindex_it(env$('Q')&"\GLmstr\"&fil$(j)&".h"&str$(cno),"Index "&env$('Q')&"\GLmstr\"&fil$(j)&".h"&str$(cno)&' '&env$('Q')&"\GLmstr\"&idx$(j)&".h"&str$(cno)&" 1 5 Replace DupKeys ")
20240 ! 
20260       if j=2 or j=5 then 
20280         open #1: "Name="&env$('Q')&"\GLmstr\"&fil$(j)&".h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\"&idx$(j)&".h"&str$(cno),internal,outin,keyed 
20300         let version(1,1)
20320         let delete_count=read_count=0
20340         let end1=st1=st2=rno=rnp=0
20360 PHASE1: ! gosub FIND1
20380         let st1=rno : let st2=99999 : let rnp=0
20400 READ_1: ! 
20420         read #1,using 'Form POS 1,G 5,POS 75,N 1': rno,ic eof PHASE1_EOF,conv PHASE1_READ_CONV
20440         let read_count+=1
20460         pr 'read_1 did'; ! pause
20480         if rno=0 then delete #1: : goto READ_1
20500         if ic=0 then pr ' ic=0, skipped' : goto READ_1
20520         if ic=1 then pr ' let rnp=rno' : let rnp=rno
20540         if ic=2 then pr ' let st2=rno' : let st2=rno : goto PHASE2
20560         pr ' ic=';ic;', skipped'
20580         goto READ_1
20600 ! 
20620 PHASE1_READ_CONV: ! 
20640 !     pr 'PHASE1_READ_CONV,read_count=';read_count : pause
20660         read #1,using 'Form POS 1,C 5,POS 75,N 1': rno$ eof PHASE1_EOF
20680         delete #1: 
20700         let delete_count+=1
20720         goto READ_1
20740 ! 
20760 PHASE1_EOF: ! 
20780 !   pr 'PHASE1_EOF,read_count=';read_count : pause
20800         let end1=1
20820 PHASE2: ! 
20840         pr 'restoring to ';st1
20860 !   pr 'PHASE2,read_count=';read_count : pause
20880         restore #1,key>=lpad$(str$(st1),5): nokey PHASE2_EOF
20900         do 
20920           read #1,using 'Form POS 1,G 5,POS 75,N 1': rno,ic eof PHASE2_EOF
20940           pr 'do read did';
20960 !     if end1=1 then pr 'end1=1' : pause ! goto PHASE2_EOF
20980           if rno<st2 then pr 'going back to L390' : goto L390
21000           if end1=1 then goto PHASE2_EOF
21020           let rnp=0
21040           goto PHASE1
21060 L390:     rewrite #1,using 'Form POS 79,N 5': rnp
21080 !     pr 'rewrite did' : pause
21100         loop 
21120 PHASE2_EOF: ! 
21140         close #1: 
21160       end if  ! j=2 or j=5
21180 NEXT_J: ! 
21200     next j
21220     goto XIT
50570 ! ______________________________________________________________________
50580 ! <Updateable Region: ERTN>
50590 ERTN: let fnerror(program$,err,line,act$,"xit")
50600     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
50610     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
50620     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
50630 ERTN_EXEC_ACT: execute act$ : goto ERTN
50640 ! /region
50650 ! ______________________________________________________________________
50660 XIT: fnend 
50670 ! ______________________________________________________________________
