00010 ! Replace S:\acsGL\schedule_hamster
00020 ! general ledger breakdowns for each schedule
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnhamster,fnagl$
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cap$*128,lbl$(1)*38,tln(1),p$(1)*160,fltyp$(1),sln(1),mask(1),sp(1),c$(1,8)*40
00080 ! ______________________________________________________________________
00090   let fntop(program$,cap$='Bank Reconciliation')
00100   gosub BUILD_LAYOUT
00110   gosub OPEN_FILE : gosub CLOSE_FILE : gosub OPEN_FILE !:
        gosub HAMSTER : gosub FIXGLACCOUNTS: gosub CLOSE_FILE: gosub INDEX
00120   goto XIT
00130 ! ______________________________________________________________________
00140 OPEN_FILE: ! !:
        let schedule=1 !:
        let open_file_count=1 ! this value is used in the close_file sub routine
00150   if exists(env$('Q')&"\GLmstr\Schedule"&str$(schedule)&".h"&str$(cno))=0 then goto L190
00160   if exists(env$('Q')&"\GLmstr\schedule"&str$(schedule)&"-idx.h"&str$(cno))=0 then gosub INDEX
00170   open #open_file_count: "Name="&env$('Q')&"\GLmstr\schedule"&str$(schedule)&".H"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\schedule"&str$(schedule)&"-idx.H"&str$(cno)&",Version=1,Shr",internal,outin,keyed 
00180   goto L220
00190 L190: open #open_file_count: "Name="&env$('Q')&"\GLmstr\schedule"&str$(schedule)&".h"&str$(cno)&",Version=1,Replace,RecL=12",internal,outin 
00200   gosub CLOSE_FILE
00210   gosub INDEX
00220 L220: return 
00230 ! ______________________________________________________________________
00240 INDEX: ! 
00250   execute "Index "&env$('Q')&"\GLmstr\schedule"&str$(schedule)&".H"&str$(cno)&' '&env$('Q')&"\GLmstr\schedule"&str$(schedule)&"-idx.h"&str$(cno) &" 1 12 Replace,DupKeys"
00260   return 
00270 ! ______________________________________________________________________
00280 FIXGLACCOUNTS: ! left pad general ledger number and reference number
00290   restore #open_file_count: 
00300 L300: read #open_file_count, using "form pos 1,c 12": gl$ eof L340
00310   let gl$=lpad$(rtrm$(gl$),12)
00320   rewrite #open_file_count, using "form pos 1,c 12": gl$
00330   goto L300
00340 L340: return 
00350 ! ______________________________________________________________________
00360 CLOSE_FILE: for j=1 to open_file_count
00370     close #j: ioerr L380
00380 L380: next j
00390   return 
00400 ! ______________________________________________________________________
00410 BUILD_LAYOUT: ! 
00420   let fncno(cno)
00430 ! ** Field Labels    ** !:
        let ic=0 ! temporary Item Counter
00440   let lbl$(ic+=1)="G/L Number"
00450 ! ** Text Box / Field Display   Lengths   ** !:
        let ic=0 ! temporary Item Counter !:
        let mmddyy=8 !:
        let ccyymmdd=10
00460   let tln(ic+=1)=12
00470 ! ** Field Types ** !:
        let ic=0
00480   let fltyp$(ic+=1)='C'
00490 ! ** Field Storage Lengths ** !:
        let ic=0 !:
        let mmddyy=6 : let ccyymmdd=8
00500   let sln(ic+=1)=12
00510 ! ** Field Masks ** !:
        let ic=0 !:
        let pointtwo=32 : let number=30 !:
        let ccyymmdd=3 : let mmddyy=1 : let glnumber=53
00520   let mask(ic+=1)=0
00530 ! ** Storage Positions ** !:
        ! starting field position - default to the same as order displayed !:
        let ic=0
00540   let sp(ic+=1)=1
00550 ! ** Combo Boxes **                                                   !:
        let cl=1 : let c$(cl,1)='ComboF' !:
        let c$(cl,2)=env$('Q')&"\GLmstr\GLmstr.h"&str$(cno) !:
        let c$(cl,3)="1" : let c$(cl,4)="12" !:
        let c$(cl,5)="13": let c$(cl,6)="40" !:
        let c$(cl,7)=env$('Q')&"\GLmstr\glindex.h"&str$(cno) !:
        ! C$(CL,8)=limit to list option ('1'=Yes; '0'=No)                     !:
        let limit_to_list$='1'
00560 ! ** Combo Boxes **                                                   !:
        ! Let CL=2 : Let C$(CL,1)='ComboF' !:
        ! Let C$(CL,2)=env$('Q')&"\GLmstr\transcode.h"&STR$(CNO) !:
        ! Let C$(CL,3)="1" : Let C$(CL,4)="2" !:
        ! Let C$(CL,5)="3" : Let C$(CL,6)="30" !:
        ! Let C$(CL,7)=env$('Q')&"\GLmstr\transcode-idx.h"&STR$(CNO) !:
        ! ! C$(CL,8)=limit to list option ('1'=Yes; '0'=No)                     !:
        ! Let LIMIT_TO_LIST$='1'
00570   return 
00580 ! ______________________________________________________________________
00590 HAMSTER: ! 
00600   let fnhamster("schgl",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
00610   return 
00620 ! ______________________________________________________________________
00630 XIT: let fnxit
00640 ! ______________________________________________________________________
00650 ! <Updateable Region: ERTN>
00660 ERTN: let fnerror(program$,err,line,act$,"xit")
00670   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00680   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00690   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00700 ERTN_EXEC_ACT: execute act$ : goto ERTN
00710 ! /region
00720 ! ______________________________________________________________________
00730   let fltyp$(ic+=1)='N'
