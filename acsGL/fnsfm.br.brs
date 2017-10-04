00010 ! Replace S:\acsGL\fnsfm
00020 ! General Ledger Financial Statement Layout - Hamster
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnhamster,fntos,fnfra,fnopt,fncmdkey,fnacs,fncno
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cap$*128,lbl$(21)*38,tln(21),p$(21)*160,fltyp$(21),sln(21),mask(21),sp(21),c$(21,8)*40
00080   dim fil$(6)*18,idx$(6)*18,id$(6)*40
00090 ! ______________________________________________________________________
00100   fntop(program$,cap$='Financial Statement Design')
00110   fncno(cno)
00120   let id$(1)=" 1. Balance Sheet File" !:
        let fil$(1)="ACGLFNSB.H"&str$(cno): let idx$(1)="FNSBINDX.H"&str$(cno)
00130   let id$(2)=" 2. Income Statement File" !:
        let fil$(2)="ACGLFNSI.H"&str$(cno): let idx$(2)="FNSIINDX.H"&str$(cno)
00140   let id$(3)=" 3. Fund Statement / Cash Flow File" !:
        let fil$(3)="ACGLFNSF.H"&str$(cno): let idx$(3)="FNSFINDX.H"&str$(cno)
00150   let id$(4)=" 4. Secondary Balance Sheet File" !:
        let fil$(4)="ACGLFNSC.H"&str$(cno): let idx$(4)="FNSCINDX.H"&str$(cno)
00160   let id$(5)=" 5. Secondary Income Statement File" !:
        let fil$(5)="ACGLFNSJ.H"&str$(cno): let idx$(5)="FNSJINDX.H"&str$(cno)
00170   let id$(6)=" 6. Secondary Fund / Cash Flow File" !:
        let fil$(6)="ACGLFNSG.H"&str$(cno): let idx$(6)="FNSGINDX.H"&str$(cno)
00180   gosub BUILD_LAYOUT
00190 MAIN: ! 
00200   fntos(sn$="FsDesign") !:
        let mylen=20: let mypos=mylen+3 : let right=1
00210   fnfra(1,1,6,60,"Financial Statement Choices","Choose the financial statement to work with.")
00220   fnopt(1,2,id$(1),0,1) !:
        let resp$(1)="True"
00230   fnopt(2,2,id$(2) ,0,1) !:
        let resp$(2)="False"
00240   fnopt(3,2,id$(3),0,1) !:
        let resp$(3)="False"
00250   fnopt(4,2,id$(4),0,1) !:
        let resp$(4)="False"
00260   fnopt(5,2,id$(5),0,1) !:
        let resp$(5)="False"
00270   fnopt(6,2,id$(6),0,1) !:
        let resp$(6)="False"
00280   fncmdkey("&Next",1,1,0,"Access the chosen financial statement design..")
00290   fncmdkey("&Cancel",5,1,0,"Return to main menu.")
00300   fnacs(sn$,0,mat resp$,ckey)
00310   if ckey=5 then goto XIT
00320   if resp$(1)="True" then let selection=1
00330   if resp$(2)="True" then let selection=2
00340   if resp$(3)="True" then let selection=3
00350   if resp$(4)="True" then let selection=4
00360   if resp$(5)="True" then let selection=5
00370   if resp$(6)="True" then let selection=6
00380   gosub OPEN_FILE : gosub CLOSE_FILE : gosub OPEN_FILE !:
        gosub HAMSTER !:
        gosub CLOSE_FILE !:
        gosub INDEX
00390   goto MAIN
00400 ! ______________________________________________________________________
00410 OPEN_FILE: ! !:
        let open_file_count=0 ! this value is used in the close_file sub routine
00420   open #open_file_count+=1: "Name="&env$('Q')&"\GLmstr\"&fil$(selection)&",KFName="&env$('Q')&"\GLmstr\"&idx$(selection)&",Use,RecL=83,KPs=1,KLn=5,Shr",internal,outin,keyed 
00430   return 
00440 ! ______________________________________________________________________
00450 CLOSE_FILE: for j=1 to open_file_count : close #j: : next j : return 
00460 ! ______________________________________________________________________
00470 INDEX: ! 
00480   execute "Index "&env$('Q')&"\GLmstr\"&fil$(selection)&' '&env$('Q')&"\GLmstr\"&idx$(selection)&" 1 5 Replace DupKeys"
00490   return 
00500 ! ______________________________________________________________________
00510 BUILD_LAYOUT: ! 
00520   fncno(cno)
00530 ! ** Field Labels    ** !:
        let ic=0 ! temporary Item Counter
00540   let lbl$(ic+=1)="F/S #" !:
        let lbl$(ic+=1)="Description" !:
        let lbl$(ic+=1)="Type of Entry" !:
        let lbl$(ic+=1)="Starting Print" !:
        let lbl$(ic+=1)="Lines to Skip"
00550   let lbl$(ic+=1)="Dollar Sign" !:
        let lbl$(ic+=1)="Underlines" !:
        let lbl$(ic+=1)="Reverse Sign" !:
        let lbl$(ic+=1)="B/S Column"
00560   let lbl$(ic+=1)="Print Accumulator" !:
        let lbl$(ic+=1)="Clr 1" !:
        let lbl$(ic+=1)="Clr 2" !:
        let lbl$(ic+=1)="Clr 3"
00570   let lbl$(ic+=1)="Clr 4" !:
        let lbl$(ic+=1)="Clr 5" !:
        let lbl$(ic+=1)="Clr 6" !:
        let lbl$(ic+=1)="Clr 7"
00580   let lbl$(ic+=1)="Clr 8" !:
        let lbl$(ic+=1)="Clr 9"
00590   let lbl$(ic+=1)="I/C % Base" !:
        let lbl$(ic+=1)="Cost Center"
00600 ! ** Text Box / Field Display   Lengths   ** !:
        let ic=0 ! temporary Item Counter !:
        let mmddyy=8 !:
        ccyymmdd=10
00610   let tln(ic+=1)=5 !:
        let tln(ic+=1)=50 !:
        let tln(ic+=1)=1 !:
        let tln(ic+=1)=2 !:
        let tln(ic+=1)=2
00620   let tln(ic+=1)=1 !:
        let tln(ic+=1)=1 !:
        let tln(ic+=1)=1 !:
        let tln(ic+=1)=1
00630   let tln(ic+=1)=1 !:
        let tln(ic+=1)=1 !:
        let tln(ic+=1)=1 !:
        let tln(ic+=1)=1
00640   let tln(ic+=1)=1 !:
        let tln(ic+=1)=1 !:
        let tln(ic+=1)=1
00650   let tln(ic+=1)=1 !:
        let tln(ic+=1)=1 !:
        let tln(ic+=1)=1 !:
        let tln(ic+=1)=3
00660   let tln(ic+=1)=5
00670 ! ** Field Types ** !:
        let ic=0
00680   let fltyp$(ic+=1)='c' !:
        let fltyp$(ic+=1)='c' !:
        let fltyp$(ic+=1)='C' !:
        let fltyp$(ic+=1)='N' !:
        let fltyp$(ic+=1)='N'
00690   let fltyp$(ic+=1)='N' !:
        let fltyp$(ic+=1)='N' !:
        let fltyp$(ic+=1)='N' !:
        let fltyp$(ic+=1)='N'
00700   let fltyp$(ic+=1)='N' !:
        let fltyp$(ic+=1)='N' !:
        let fltyp$(ic+=1)='N' !:
        let fltyp$(ic+=1)='N'
00710   let fltyp$(ic+=1)='N' !:
        let fltyp$(ic+=1)='N' !:
        let fltyp$(ic+=1)='N' !:
        let fltyp$(ic+=1)='N'
00720   let fltyp$(ic+=1)='N' !:
        let fltyp$(ic+=1)='N' !:
        let fltyp$(ic+=1)='N'
00730   let fltyp$(ic+=1)='N'
00740 ! ** Field Storage Lengths ** !:
        let ic=0 !:
        let mmddyy=6 : ccyymmdd=8
00750   let sln(ic+=1)=5 !:
        let sln(ic+=1)=50 !:
        let sln(ic+=1)=1 !:
        let sln(ic+=1)=2 !:
        let sln(ic+=1)=2
00760   let sln(ic+=1)=1 !:
        let sln(ic+=1)=1 !:
        let sln(ic+=1)=1 !:
        let sln(ic+=1)=1
00770   let sln(ic+=1)=1 !:
        let sln(ic+=1)=1 !:
        let sln(ic+=1)=1 !:
        let sln(ic+=1)=1
00780   let sln(ic+=1)=1 !:
        let sln(ic+=1)=1 !:
        let sln(ic+=1)=1
00790   let sln(ic+=1)=1 !:
        let sln(ic+=1)=1 !:
        let sln(ic+=1)=1 !:
        let sln(ic+=1)=3
00800   let sln(ic+=1)=5
00810 ! ** Field Masks ** !:
        let ic=0 !:
        let pointtwo=32 : let number=30 !:
        ccyymmdd=3 : let mmddyy=1 : let glnumber=53
00820   let mask(ic+=1)=0 !:
        let mask(ic+=1)=0 !:
        let mask(ic+=1)=0 !:
        let mask(ic+=1)=number !:
        let mask(ic+=1)=number
00830   let mask(ic+=1)=number !:
        let mask(ic+=1)=number !:
        let mask(ic+=1)=number !:
        let mask(ic+=1)=number
00840   let mask(ic+=1)=number !:
        let mask(ic+=1)=number !:
        let mask(ic+=1)=number !:
        let mask(ic+=1)=number
00850   let mask(ic+=1)=number !:
        let mask(ic+=1)=number !:
        let mask(ic+=1)=number !:
        let mask(ic+=1)=number
00860   let mask(ic+=1)=number !:
        let mask(ic+=1)=number !:
        let mask(ic+=1)=number
00870   let mask(ic+=1)=number
00880 ! ** Storage Positions ** !:
        ! default to the same as order displayed !:
        let ic=0
00890   let sp(ic+=1)=1 !:
        let sp(ic+=1)=6 !:
        let sp(ic+=1)=56 !:
        let sp(ic+=1)=57 !:
        let sp(ic+=1)=59
00900   let sp(ic+=1)=61 !:
        let sp(ic+=1)=62 !:
        let sp(ic+=1)=63 !:
        let sp(ic+=1)=64
00910   let sp(ic+=1)=65 !:
        let sp(ic+=1)=66 !:
        let sp(ic+=1)=67 !:
        let sp(ic+=1)=68
00920   let sp(ic+=1)=69 !:
        let sp(ic+=1)=70 !:
        let sp(ic+=1)=71 !:
        let sp(ic+=1)=72
00930   let sp(ic+=1)=73 !:
        let sp(ic+=1)=74: !:
        let sp(ic+=1)=75
00940   let sp(ic+=1)=78
00950   return 
00960 ! ______________________________________________________________________
00970 HAMSTER: ! 
00980   fnhamster("Acglfnsb",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
00990   return 
01000 ! ______________________________________________________________________
01010 XIT: let fnxit
01020 ! ______________________________________________________________________
01030 ! <Updateable Region: ERTN>
01040 ERTN: let fnerror(program$,err,line,act$,"xit")
01050   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01070   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01080 ERTN_EXEC_ACT: execute act$ : goto ERTN
01090 ! /region
01100 ! ______________________________________________________________________
