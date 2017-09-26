00010 ! Replace S:\acsSU\Client
00020 ! Client File
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnhamster
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cap$*128,lbl$(9)*38,tln(9),p$(9)*160,fltyp$(9),mask(9)
00080 ! dim sln(9)
00090 ! ______________________________________________________________________
00100   let fntop(program$,cap$='Client')
00110   gosub BUILD_LAYOUT
00120   gosub OPEN_FILE : gosub CLOSE_FILE : gosub OPEN_FILE !:
        gosub HAMSTER: gosub CLOSE_FILE
00130   execute "Index "&env$('Q')&"\TMmstr\client.h"&str$(cno)&","&env$('Q')&"\TMmstr\client-idx.h"&str$(cno)&",1,6,replace,DupKeys,Shr -n"
00140   goto XIT
00150 ! ______________________________________________________________________
00160 OPEN_FILE: ! !:
        let open_file_count=0 ! this value is used in the close_file sub routine
00170   open #open_file_count+=1: "Name="&env$('Q')&"\TMmstr\Client.h"&str$(cno)&",Version=1,KFName="&env$('Q')&"\TMmstr\Client-Idx.h"&str$(cno)&",Use,RecL=406,KPs=1,KLn=6,Shr",internal,outin,keyed 
00180   return 
00190 ! ______________________________________________________________________
00200 CLOSE_FILE: for j=1 to open_file_count : close #j: : next j : return 
00210 ! ______________________________________________________________________
00220 BUILD_LAYOUT: ! 
00230   let fncno(cno)
00240 ! ** Field Labels    ** !:
        let ic=0 ! temporary Item Counter
00250   let lbl$(ic+=1)="Client ID" !:
        let lbl$(ic+=1)="Company Name" !:
        let lbl$(ic+=1)="Company Address (1)" !:
        let lbl$(ic+=1)="Company Address (2)" !:
        let lbl$(ic+=1)="Company City State Zip"
00260   let lbl$(ic+=1)="E-Mail Address" !:
        let lbl$(ic+=1)="Primary Contact Name" !:
        let lbl$(ic+=1)="Phone Number" !:
        let lbl$(ic+=1)="x"
00270 ! ** Text Box / Field Display   Lengths   ** !:
        let ic=0 ! temporary Item Counter !:
        let mmddyy=8 !:
        let ccyymmdd=10
00280   let tln(ic+=1)=6 !:
        let tln(ic+=1)=50 !:
        let tln(ic+=1)=50 !:
        let tln(ic+=1)=50 !:
        let tln(ic+=1)=50
00290   let tln(ic+=1)=50 !:
        let tln(ic+=1)=50 !:
        let tln(ic+=1)=50 !:
        let tln(ic+=1)=50
00300 ! ** Field Types ** !:
        let ic=0
00310   let fltyp$(ic+=1)='N' !:
        let fltyp$(ic+=1)='C' !:
        let fltyp$(ic+=1)='C' !:
        let fltyp$(ic+=1)='C' !:
        let fltyp$(ic+=1)='C'
00320   let fltyp$(ic+=1)='C' !:
        let fltyp$(ic+=1)='C' !:
        let fltyp$(ic+=1)='C' !:
        let fltyp$(ic+=1)='C'
00330 ! ** Field Masks ** !:
        let ic=0 !:
        let pointtwo=32 : let number=30 !:
        let ccyymmdd=3 : let mmddyy=1 : let glnumber=53
00340   let mask(ic+=1)=80 !:
        let mask(ic+=1)=80 !:
        let mask(ic+=1)=80 !:
        let mask(ic+=1)=80 !:
        let mask(ic+=1)=80
00350   let mask(ic+=1)=0 !:
        let mask(ic+=1)=0 !:
        let mask(ic+=1)=0 !:
        let mask(ic+=1)=0
00360   return 
00370 ! ______________________________________________________________________
00380 HAMSTER: ! 
00390   let fnhamster("Client",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask)
00400   return 
00410 ! ______________________________________________________________________
00420 XIT: let fnxit
00430 ! ______________________________________________________________________
00440 ! <Updateable Region: ERTN>
00450 ERTN: let fnerror(program$,err,line,act$,"xit")
00460   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00470   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00480   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00490 ERTN_EXEC_ACT: execute act$ : goto ERTN
00500 ! /region
00510 ! ______________________________________________________________________
