00010 ! Replace  S:\acsPR\burden
00020 ! Service Code File
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnwin3b,fnerror,fnwait,fncno,fnmsgbox,fnxit,fnprocess,fndat,fntop,fncode_search,fntos,fnlbl,fncmbcode,fncmdkey,fnacs,fntxt,fncmdset,fncombof,fnrgl$,fnqgl,fnagl$,fnchk,fnflexinit1,fncmbemp,fnflexadd1,fncmbburden,fnburden_srch
00050   let fntop("S:\acsPR\burden",cap$="Personnel Burden")
00060   on error goto ERTN
00070   dim cnam$*40,dat$*20,gl(3),sf1$*28,sn$*30,cap$*128,search$(22),resp$(10)*60
00080   dim name$*30
00090   dim holdeno$*8,vcode$*6,de$*30,eno$*8,search$(22),ic$*2,pc$*1
00100   dim df$*256,if$*256
00110   dim code$(2),item2$(4)*30,breakdownde$*30,ml$(3)*80,code2$(3)
00120 ! ______________________________________________________________________
00130   let fncno(cno,cnam$)
00140   let fndat(dat$,1)
00150   def fncd(x)=(x-int(x*.01)*100)*10000+int(x*.01)
00160 ! 
00170   if exists(env$('Q')&"\PRmstr\Burden.H"&str$(cno))=0 then goto SETUP_FILES
00180 L180: open #1: "Name="&env$('Q')&"\PRmstr\Burden.H"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\BurdenIdx.H"&str$(cno)&",Shr",internal,outin,keyed 
00190 L190: form pos 1,n 8,c 30,3*n 6.3
00200 ASKEMPLOYEE: ! 
00205   mat resp$=("")
00210   ad1=0 ! add code - used to tell other parts of the program, !:
        ! that I am currently adding a service code record.
00220   let fntos(sn$="Pr-askemployee") !:
        let respc=0
00230   let fnlbl(1,1,"Employee Number:",20,right)
00240   let fncmbburden(1,23)
00250   if hact$="" then !:
          let resp$(respc+=1)="" else !:
          let resp$(respc+=1)=hact$
00260   let fncmdkey("&Add",1,0,0,"Add a new employee burden record." ) !:
        let fncmdkey("E&dit",2,1,0,"Access the highlited record") !:
        let fncmdkey("&Next",3,0,0,"Access next record in employee burden order") !:
        let fncmdkey("&Search",6,0,0,"Search for employee burden record") !:
        let fncmdkey("&Refresh",7,0,0,"Updates search grids and combo boxes with new employee burden information") !:
        let fncmdkey("&Proof List",8,0,0,"Returns to menu") !:
        let fncmdkey("E&xit",5,0,1,"Returns to menu")
00270   let fnacs(sn$,0,mat resp$,ckey)
00280   if ckey=5 then goto XIT
00290   if ckey=8 then gosub PRINT_PROOF: goto ASKEMPLOYEE
00300   if ckey=1 then goto ADD_RECORD
00310   if ckey=3 then read #1,using L190: eno,name$,burden,burden2,burden3 nokey ASKEMPLOYEE eof ASKEMPLOYEE: let holdeno=eno: goto SCREEN_1
00320   if ckey=4 then read #1,using L190,key=holdeno$: eno,name$,burden,burden2,burden3 nokey ASKEMPLOYEE,ioerr ASKEMPLOYEE : goto SCREEN_1
00330   let eno=val(resp$(1)(1:8)): let holdeno=eno
00340   let eno$=lpad$(str$(eno),8)
00350   if ckey=2 then read #1,using L190,key=eno$: eno,name$,burden,burden2,burden3 nokey ASKEMPLOYEE : goto SCREEN_1
00360   if ckey=6 then let fnburden_srch(eno$,fixgrid) : let eno$=lpad$(rtrm$(eno$),8) : read #1,using L190,key=eno$: eno,name$,burden,burden2,burden3 nokey ASKEMPLOYEE : goto SCREEN_1
00361   if trim$(eno$)="" then goto ASKEMPLOYEE else read #1,using L190,key=eno$: eno,name$,burden,burden2,burden3 nokey ASKEMPLOYEE : goto SCREEN_1
00370   if ckey=7 then gosub RECREATE_GRID: goto ASKEMPLOYEE
00380 SCREEN_1: ! maintain personnel burdern screen
00390   let fntos(sn$="Pr-burden") !:
        let respc=0
00400   let mylen=12: let mypos=mylen+3 : let right=1
00410   let fnlbl(1,1,"Employee #:",mylen,right)
00420   let fntxt(1,mypos,6,0,0,"") !:
        let resp$(1)=eno$
00430   let fnlbl(2,1,"Name:",mylen,right)
00440   let fntxt(2,mypos,30,0,0,"",0,"The name should be pulled from payroll and automatically displayed.") !:
        let resp$(2)=name$
00460   let fnlbl(3,1,"Burden %:",mylen,right)
00470   let fntxt(3,mypos,6,0,0,"33",0,"Enter the % of wage that you wish to use on this employee to calculate the personnel burden charged to jobs.") !:
        let resp$(3)=str$(burden)
00480   let fnlbl(4,1,"Unused:",mylen,right)
00490   let fntxt(4,mypos,6,0,0,"33",0,"Unused field at this time.") !:
        let resp$(4)=""
00500   let fnlbl(5,1,"Unused:",mylen,right)
00510   let fntxt(5,mypos,6,0,0,"33",0,"Unused field at this time.") !:
        let resp$(5)=""
00520   let fncmdkey("&Save",1,1,0,"Saves any changes and returns to main screen.")
00530   let fncmdkey("&Delete",4,0,0,"Deletes this record from the personnel burden file.")
00540   let fncmdkey("&Cancel",5,0,1,"Returns to first screen without saving any changes.")
00550   let fnacs(sn$,0,mat resp$,ckey)
00560   if ckey=5 then goto ASKEMPLOYEE
00562   let eno=val(resp$(1)(1:8)) : let eno$=lpad$(trim$(resp$(1)),8)
00563   let name$=resp$(2)
00564   burden=val(resp$(3))
00570   if ckey<>4 then goto L640
00580   mat ml$(2) !:
        let ml$(1)="You have chosen to delete employee "&trim$(eno$)&" from the burden file!" !:
        let ml$(2)="Select OK to delete; else Cancel to retain the record." !:
        let fnmsgbox(mat ml$,resp$,cap$,49)
00590   if resp$="OK" then goto L600 else goto ASKEMPLOYEE
00600 L600: if ckey=4 then delete #1,key=eno$: : gosub RECREATE_GRID: goto ASKEMPLOYEE
00640 L640: rewrite #1,using L190,key=eno$: eno,name$,burden,burden2,burden3 nokey L650
00650 L650: if ckey=1 then goto ASKEMPLOYEE
00660   goto ASKEMPLOYEE
00670 ! ______________________________________________________________________
00680 RECREATE_GRID: ! 
00690   let fnburden_srch(x$,99) !:
        let df$=env$('Q')&"\PRmstr\Burden.h"&str$(cno) : let if$=env$('Q')&"\PRmstr\Burdenidx.h"&str$(cno) !:
        let fncombof("CBurden",lyne,mypos,43,df$,1,8,9,30,if$,1) !:
        let fncombof("CBurdenaLL",lyne,mypos,43,df$,1,8,9,30,if$,2)
00700   ad1=0 ! set add code back before returning to main screen
00710   return 
00720 ADD_RECORD: ! 
00730   if reindex>3 then goto ERTN
00740   let fntos(sn$="Burden2")
00750   let fnlbl(1,5,"New Personnel Burden Information",45,1)
00760   let fnlbl(3,1,"Employee Number:",15,0)
00770   let fncmbemp(3,18)
00780   let resp$(1)=""
00790   let fncmdset(11)
00800   let fnacs(sn$,0,mat resp$,ckey)
00810   if ckey=5 then goto ASKEMPLOYEE
00820   let eno=val(resp$(1)(1:8)) !:
        let eno$=lpad$(trim$(resp$(1)(1:8)),8)
00830   let name$=(resp$(1)(10:40))
00840   if trim$(eno$)="" then goto ADD_RECORD
00850   read #1,using L190,key=eno$: z$ nokey L870
00860   mat ml$(2) !:
        let ml$(1)="A record # "&eno$&" already exists!" !:
        let ml$(2)="Choose to review the record." !:
        let fnmsgbox(mat ml$,resp$,cap$,48) !:
        goto ADD_RECORD
00870 L870: burden=burden2=burden3=0
00880   write #1,using L190: eno,name$,burden,burden2,burden3
00890   let holdeno=eno
00900   gosub RECREATE_GRID
00910   goto SCREEN_1
00920 SETUP_FILES: ! 
00930   open #1: "Name="&env$('Q')&"\PRmstr\Burden.H"&str$(cno)&",RecL=128,replace",internal,outin 
00940   close #1: 
00950   goto REINDEX
00960 REINDEX: ! indexes if needed
00970   let reindex+=1
00980   close #1: ioerr L990
00990 L990: execute "Index "&env$('Q')&"\PRmstr\Burden.H"&str$(cno)&' '&env$('Q')&"\PRmstr\BurdenIdx.H"&str$(cno)&" 1 8 Replace DupKeys -n"
01000   goto L180
01010 PRINT_PROOF: ! 
01020   let fnopenprn
01030   gosub L1140
01040   restore #1: 
01050 L1050: read #1,using L190,release: eno,name$,burden,burden2,burden3 eof L1090
01060   pr #255,using L1070: eno,name$,burden pageoflow L1130
01070 L1070: form pos 1,n 8,x 6,c 30,n 6.3 ,skip 1
01080   goto L1050
01090 L1090: if nw=0 then pr #255: newpage
01100   let fncloseprn
01110   on fkey 5 ignore 
01120   goto ASKEMPLOYEE
01130 L1130: pr #255: newpage : gosub L1140 : continue 
01140 L1140: pr #255,using L1150: date$,cnam$
01150 L1150: form pos 1,c 10,pos 20,cc 40,skip 1
01160   pr #255,using L1150: time$,"Personnel Burden "
01170   pr #255: 
01180   pr #255: " Employee #  Name                         Burden %"
01190   pr #255: " __________  ____________________         ________"
01200   return 
01210   form pos 1,c 6,c 12,c 30,pd 3
01220 ! ______________________________________________________________________
01230 POF1: pr #255: newpage
01240   pr #255,using L1300: date$('mm/dd/yy'),cnam$,time$,"SERVICE CODE PROOF LIST",dat$
01250   let pcnt=4
01260   continue 
01270 ! ______________________________________________________________________
01280 POF2: pr #255: newpage
01290   pr #255,using L1300: date$('mm/dd/yy'),cnam$,time$,"SERVICE CODE PROOF LIST",dat$
01300 L1300: form skip 3,pos 1,c 8,pos namtab,c 40,skip 1,pos 1,c 8,pos 53,c 30,skip 1,pos dattab,c 20,skip 2
01310   pr #255,using L1320: pl$(9,1),pl$(9,2)
01320 L1320: form pos 20,2*c 50,skip 1
01330   let pcnt=5
01340   continue 
01350 ! ______________________________________________________________________
01360 XIT: let fnxit
01370 ! ______________________________________________________________________
01380 ERTN: let fnerror(program$,err,line,act$,"xit")
01390   if uprc$(act$)<>"PAUSE" then goto L1420
01400   if trim$(env$("ACSDeveloper"))<>"" then !:
          execute "list "&str$(line) !:
          pause  !:
          goto L1420
01410   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause 
01420 L1420: execute act$
01430   goto ERTN
