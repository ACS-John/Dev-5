! (formerly)  S:\acsPR\category
! Service Code File
 
	autoLibrary
	fnTop(program$)
	on error goto Ertn
	dim dat$*20,gl(3),sf1$*28,sn$*30,search$(22),resp$(10)*60
	dim holdcategory$*5,category$*5
	dim df$*256,if$*256
	dim item2$(4)*30,name$*30,ml$(3)*80
 
	fndat(dat$,1)
 
	if exists("[Q]\PRmstr\Category.h[cno]")=0 then goto SETUP_FILES
L170: open #1: "Name=[Q]\PRmstr\Category.h[cno],KFName=[Q]\PRmstr\categoryIDX.h[cno],Shr",i,outIn,k
L180: form pos 1,n 5,c 30
ASKCATEGORY: !
	mat resp$=("")
	ad1=0 ! add code - used to tell other parts of the program, : _
	! that I am currently adding a service code record.
	fnTos(sn$="Pr-askcategory") : _
	respc=0
	fnLbl(1,1,"Category Number:",20,right)
	fnCmbCategory(1,23)
	if hact$="" then : _
		resp$(respc+=1)="" else : _
		resp$(respc+=1)=hact$
	fnCmdKey("&Add",1,0,0,"Add a new category record." ) : _
	fnCmdKey("E&dit",2,1,0,"Access the highlited record") : _
	fnCmdKey("&Next",3,0,0,"Access next record in category order") : _
	fnCmdKey("&Search",6,0,0,"Search for category record") : _
	fnCmdKey("&Refresh",7,0,0,"Updates search grids and combo boxes with new category information") : _
	fnCmdKey("&Proof List",8,0,0,"Returns to menu") : _
	fnCmdKey("E&xit",5,0,1,"Returns to menu")
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	if ckey=8 then gosub PRINT_PROOF: goto ASKCATEGORY
	if ckey=1 then goto ADD_RECORD
	if ckey=3 then read #1,using L180: category,name$ nokey ASKCATEGORY eof ASKCATEGORY: holdcategory=category: goto SCREEN_1
	category=val(resp$(1)(1:5)): holdcategory=category
	category$=lpad$(str$(category),5)
	if ckey=2 then read #1,using L180,key=category$: category,name$ nokey ASKCATEGORY : goto SCREEN_1
	if ckey=6 then fncategory_srch(category$,fixgrid) : category$=lpad$(rtrm$(category$),5) : read #1,using L180,key=category$: category,name$ nokey ASKCATEGORY : goto SCREEN_1
	if trim$(category$)="" then goto ASKCATEGORY else read #1,using L180,key=catergory$: category,name$ nokey ASKCATEGORY : goto SCREEN_1
	if ckey=7 then gosub RECREATE_GRID: goto ASKCATEGORY
SCREEN_1: ! maintain category screen
	fnTos(sn$="Pr-Category") : _
	respc=0
	mylen=12: mypos=mylen+3 : right=1
	fnLbl(1,1,"Category #:",mylen,right)
	fnTxt(1,mypos,6,0,0,"") : _
	resp$(1)=str$(category)
	fnLbl(2,1,"Name:",mylen,right)
	fnTxt(2,mypos,30,0,0,"",0,"The enter the category name.") : _
	resp$(2)=name$
	fnCmdKey("&Save",1,1,0,"Saves any changes and returns to main screen.")
	fnCmdKey("&Delete",4,0,0,"Deletes this record from the Category file.")
	fnCmdKey("&Cancel",5,0,1,"Returns to first screen without saving any changes.")
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto ASKCATEGORY
	category=val(resp$(1)(1:5)) : category$=lpad$(trim$(resp$(1)),5)
	name$=resp$(2)
	category=val(resp$(1))
	if ckey<>4 then goto L570
	mat ml$(2) : _
	ml$(1)="You have chosen to delete category "&trim$(category$)&" from the Category file!" : _
	ml$(2)="Select OK to delete; else Cancel to retain the record." : _
	fnMsgBox(mat ml$,resp$,'',49)
	if resp$="OK" then goto L560 else goto ASKCATEGORY
L560: if ckey=4 then delete #1,key=catergory$: : gosub RECREATE_GRID: goto ASKCATEGORY: fnMsgBox(mat ml$,resp$,'',49)
L570: rewrite #1,using L180,key=category$: category,name$ nokey L580
L580: if ckey=1 then goto ASKCATEGORY
	goto ASKCATEGORY
 
RECREATE_GRID: !
	fncategory_srch(x$,99) : _
	df$="[Q]\PRmstr\category.h[cno]" : if$="[Q]\PRmstr\categoryidx.h[cno]" : _
	fnComboF("Ccategory",lyne,mypos,43,df$,1,5,6,30,if$,1) : _
	fnComboF("CcategoryaLL",lyne,mypos,43,df$,1,5,6,30,if$,2)
	ad1=0 ! set add code back before returning to main screen
return
ADD_RECORD: !
	if reindex>3 then goto ERTN
	fnTos(sn$="category2")
	fnLbl(1,5,"New Category Information",30,1)
	fnLbl(3,1,"Category Number:",15,0)
	fnTxt(3,18,5,0,0,"") : _
	resp$(1)=str$(category)
	resp$(1)=""
	fnCmdSet(11)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto ASKCATEGORY
	category=val(resp$(1)(1:5)) : _
	category$=lpad$(trim$(resp$(1)(1:5)),5)
	name$=(resp$(1)(10:40))
	if trim$(category$)="" then goto ADD_RECORD
	read #1,using L180,key=category$: z$ nokey L800
	mat ml$(2) : _
	ml$(1)="A record # "&category$&" already exists!" : _
	ml$(2)="Choose to review the record." : _
	fnMsgBox(mat ml$,resp$,'',48) : _
	goto ADD_RECORD
L800: write #1,using L180: category,name$
	holdcategory=category
	gosub RECREATE_GRID
	goto SCREEN_1
SETUP_FILES: !
	open #1: "Name=[Q]\PRmstr\Category.h[cno],RecL=128,replace",internal,outIn
	close #1:
	goto REINDEX
REINDEX: ! indexes if needed
	reindex+=1
	close #1: ioerr L910
L910: execute "Index [Q]\PRmstr\Category.h[cno]"&' '&"[Q]\PRmstr\categoryIDX.h[cno] 1 5 Replace DupKeys -n"
	goto L170
PRINT_PROOF: !
	fnOpenPrn
	gosub L1060
	restore #1:
L970: read #1,using L180,release: category,name$ eof L1010
	pr #255,using L990: category,name$ pageoflow L1050
L990: form pos 1,n 5,x 8,c 30,skip 1
	goto L970
L1010: if nw=0 then pr #255: newpage
	fnClosePrn
	on fkey 5 ignore
	goto ASKCATEGORY
L1050: pr #255: newpage : gosub L1060 : continue
L1060: pr #255,using L1070: date$,env$('cnam')
L1070: form pos 1,c 10,pos 20,cc 40,skip 1
	pr #255,using L1070: time$,"Category Names "
	pr #255:
	pr #255: " Category #  Name          "
	pr #255: " __________  ____________________  "
return
 
POF1: pr #255: newpage
	pr #255,using L1200: date$('mm/dd/yy'),env$('cnam'),time$,"CATEGORY LISTING",dat$
	continue
 
POF2: pr #255: newpage
	pr #255,using L1200: date$('mm/dd/yy'),env$('cnam'),time$,"CATEGPRU LISTING",dat$
L1200: form skip 3,pos 1,c 8,pos namtab,c 40,skip 1,pos 1,c 8,pos 53,c 30,skip 1,pos dattab,c 20,skip 2
	continue
 
Xit: fnXit
 
include: ertn
