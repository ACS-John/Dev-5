00010 ! (formerly)  S:\acsPR\category
00020 ! Service Code File
00030 !
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnerror,fnmsgbox,fnxit,fndat,fntop,fnTos,fnLbl,fnCmdKey,fnAcs,fnTxt,fnCmdSet,fncombof,fnflexinit1,fnflexadd1,fncmbcategory,fncategory_srch
00050   fntop(program$)
00060   on error goto Ertn
00070   dim dat$*20,gl(3),sf1$*28,sn$*30,search$(22),resp$(10)*60
00080   dim holdcategory$*5,category$*5
00090   dim df$*256,if$*256
00100   dim item2$(4)*30,name$*30,ml$(3)*80
00110 !
00130   fndat(dat$,1)
00140   def fncd(x)=(x-int(x*.01)*100)*10000+int(x*.01) ! /r
00150 ! 
00160   if exists("[Q]\PRmstr\Category.H[cno]")=0 then goto SETUP_FILES
00170 L170: open #1: "Name=[Q]\PRmstr\Category.H[cno],KFName=[Q]\PRmstr\categoryIDX.H[cno],Shr",internal,outIn,keyed 
00180 L180: form pos 1,n 5,c 30
00190 ASKCATEGORY: ! 
00200   mat resp$=("")
00210   ad1=0 ! add code - used to tell other parts of the program, !:
        ! that I am currently adding a service code record.
00220   fnTos(sn$="Pr-askcategory") !:
        respc=0
00230   fnLbl(1,1,"Category Number:",20,right)
00240   fncmbcategory(1,23)
00250   if hact$="" then !:
          resp$(respc+=1)="" else !:
          resp$(respc+=1)=hact$
00260   fnCmdKey("&Add",1,0,0,"Add a new category record." ) !:
        fnCmdKey("E&dit",2,1,0,"Access the highlited record") !:
        fnCmdKey("&Next",3,0,0,"Access next record in category order") !:
        fnCmdKey("&Search",6,0,0,"Search for category record") !:
        fnCmdKey("&Refresh",7,0,0,"Updates search grids and combo boxes with new category information") !:
        fnCmdKey("&Proof List",8,0,0,"Returns to menu") !:
        fnCmdKey("E&xit",5,0,1,"Returns to menu")
00270   fnAcs(sn$,0,mat resp$,ckey)
00280   if ckey=5 then goto XIT
00290   if ckey=8 then gosub PRINT_PROOF: goto ASKCATEGORY
00300   if ckey=1 then goto ADD_RECORD
00310   if ckey=3 then read #1,using L180: category,name$ nokey ASKCATEGORY eof ASKCATEGORY: holdcategory=category: goto SCREEN_1
00320   category=val(resp$(1)(1:5)): holdcategory=category
00330   category$=lpad$(str$(category),5)
00340   if ckey=2 then read #1,using L180,key=category$: category,name$ nokey ASKCATEGORY : goto SCREEN_1
00350   if ckey=6 then let fncategory_srch(category$,fixgrid) : category$=lpad$(rtrm$(category$),5) : read #1,using L180,key=category$: category,name$ nokey ASKCATEGORY : goto SCREEN_1
00360   if trim$(category$)="" then goto ASKCATEGORY else read #1,using L180,key=catergory$: category,name$ nokey ASKCATEGORY : goto SCREEN_1
00370   if ckey=7 then gosub RECREATE_GRID: goto ASKCATEGORY
00380 SCREEN_1: ! maintain category screen
00390   fnTos(sn$="Pr-Category") !:
        respc=0
00400   mylen=12: mypos=mylen+3 : right=1
00410   fnLbl(1,1,"Category #:",mylen,right)
00420   fnTxt(1,mypos,6,0,0,"") !:
        resp$(1)=str$(category)
00430   fnLbl(2,1,"Name:",mylen,right)
00440   fnTxt(2,mypos,30,0,0,"",0,"The enter the category name.") !:
        resp$(2)=name$
00450   fnCmdKey("&Save",1,1,0,"Saves any changes and returns to main screen.")
00460   fnCmdKey("&Delete",4,0,0,"Deletes this record from the Category file.")
00470   fnCmdKey("&Cancel",5,0,1,"Returns to first screen without saving any changes.")
00480   fnAcs(sn$,0,mat resp$,ckey)
00490   if ckey=5 then goto ASKCATEGORY
00500   category=val(resp$(1)(1:5)) : category$=lpad$(trim$(resp$(1)),5)
00510   name$=resp$(2)
00520   category=val(resp$(1))
00530   if ckey<>4 then goto L570
00540   mat ml$(2) !:
        ml$(1)="You have chosen to delete category "&trim$(category$)&" from the Category file!" !:
        ml$(2)="Select OK to delete; else Cancel to retain the record." !:
        fnmsgbox(mat ml$,resp$,'',49)
00550   if resp$="OK" then goto L560 else goto ASKCATEGORY
00560 L560: if ckey=4 then delete #1,key=catergory$: : gosub RECREATE_GRID: goto ASKCATEGORY: fnmsgbox(mat ml$,resp$,'',49)
00570 L570: rewrite #1,using L180,key=category$: category,name$ nokey L580
00580 L580: if ckey=1 then goto ASKCATEGORY
00590   goto ASKCATEGORY
00600 !
00610 RECREATE_GRID: ! 
00620   fncategory_srch(x$,99) !:
        df$="[Q]\PRmstr\category.h[cno]" : if$="[Q]\PRmstr\categoryidx.h[cno]" !:
        fncombof("Ccategory",lyne,mypos,43,df$,1,5,6,30,if$,1) !:
        fncombof("CcategoryaLL",lyne,mypos,43,df$,1,5,6,30,if$,2)
00630   ad1=0 ! set add code back before returning to main screen
00640   return 
00650 ADD_RECORD: ! 
00660   if reindex>3 then goto ERTN
00670   fnTos(sn$="category2")
00680   fnLbl(1,5,"New Category Information",30,1)
00690   fnLbl(3,1,"Category Number:",15,0)
00700   fnTxt(3,18,5,0,0,"") !:
        resp$(1)=str$(category)
00710   resp$(1)=""
00720   fnCmdSet(11)
00730   fnAcs(sn$,0,mat resp$,ckey)
00740   if ckey=5 then goto ASKCATEGORY
00750   category=val(resp$(1)(1:5)) !:
        category$=lpad$(trim$(resp$(1)(1:5)),5)
00760   name$=(resp$(1)(10:40))
00770   if trim$(category$)="" then goto ADD_RECORD
00780   read #1,using L180,key=category$: z$ nokey L800
00790   mat ml$(2) !:
        ml$(1)="A record # "&category$&" already exists!" !:
        ml$(2)="Choose to review the record." !:
        fnmsgbox(mat ml$,resp$,'',48) !:
        goto ADD_RECORD
00800 L800: write #1,using L180: category,name$
00810   holdcategory=category
00820   gosub RECREATE_GRID
00830   goto SCREEN_1
00840 SETUP_FILES: ! 
00850   open #1: "Name=[Q]\PRmstr\Category.H[cno],RecL=128,replace",internal,outIn 
00860   close #1: 
00870   goto REINDEX
00880 REINDEX: ! indexes if needed
00890   reindex+=1
00900   close #1: ioerr L910
00910 L910: execute "Index [Q]\PRmstr\Category.H[cno]"&' '&"[Q]\PRmstr\categoryIDX.H[cno] 1 5 Replace DupKeys -n"
00920   goto L170
00930 PRINT_PROOF: ! 
00940   fnopenprn
00950   gosub L1060
00960   restore #1: 
00970 L970: read #1,using L180,release: category,name$ eof L1010
00980   pr #255,using L990: category,name$ pageoflow L1050
00990 L990: form pos 1,n 5,x 8,c 30,skip 1
01000   goto L970
01010 L1010: if nw=0 then pr #255: newpage
01020   fncloseprn
01030   on fkey 5 ignore 
01040   goto ASKCATEGORY
01050 L1050: pr #255: newpage : gosub L1060 : continue 
01060 L1060: pr #255,using L1070: date$,env$('cnam')
01070 L1070: form pos 1,c 10,pos 20,cc 40,skip 1
01080   pr #255,using L1070: time$,"Category Names "
01090   pr #255: 
01100   pr #255: " Category #  Name          "
01110   pr #255: " __________  ____________________  "
01120   return 
01130 !
01140 POF1: pr #255: newpage
01150   pr #255,using L1200: date$('mm/dd/yy'),env$('cnam'),time$,"CATEGORY LISTING",dat$
01160   continue 
01170 !
01180 POF2: pr #255: newpage
01190   pr #255,using L1200: date$('mm/dd/yy'),env$('cnam'),time$,"CATEGPRU LISTING",dat$
01200 L1200: form skip 3,pos 1,c 8,pos namtab,c 40,skip 1,pos 1,c 8,pos 53,c 30,skip 1,pos dattab,c 20,skip 2
01210   continue 
01220 !
01230 XIT: fnxit
01240 !
01250 ERTN: fnerror(program$,err,line,act$,"xit")
01260   if uprc$(act$)<>"PAUSE" then goto L1290
01270   if trim$(env$("ACSDeveloper"))<>"" then !:
          execute "list "&str$(line) !:
          pause  !:
          goto L1290
01280   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause 
01290 L1290: execute act$
01300   goto ERTN
