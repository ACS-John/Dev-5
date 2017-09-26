00010 ! Prtflex2 !:
        ! DO NOT RENUMBER
00011 ! Replace Core\PrtFlex\PrtFlex2
00013 ! ______________________________________________________________________
00019 ! ______________________________________________________________________
00020   library 'Core\Library': fnacs,fnlbl,fntxt,fntos,fnerror,fnflexadd1,fnflexinit1,fnbutton,fncombof,fnmsgbox,fncursys$,fnchain,fnxit,fncmdset,fntop
00021   on error goto ERTN
00022 ! ______________________________________________________________________
00023   dim programfolder$*256,datafolder$*256,gridname$*40
00024   dim name$*30,colmask$*3,tt$*200,colhdr$(80)*30,colmask$(80)*3
00025   dim response$(87)*80,text$*40, cap$*128,lastgridresponse$*87
00026   dim options$(300)*87,ln$*132,item$(80)*80,abbrev$*20,open_read$*80,tg(11)
00027   dim z$*8,rp$*3,py$(8)*25,ss$*11,pb$(10)*2,pl$(5)*13,pf$(7)*6,pa$(8)*1
00028   dim pm(40),adr(2)
00029   dim dg$(6)*6,pc$(5)*5,pcd(5),oc$(5)*2,ocd(5),va$(5)*2,vaa(5)
00031   dim df$*1,dr$*9,dc$*2,da$*17,extra(23),extra$(11)*30,item$(80)*50
00032   dim abbrev$*20,open_read$*80,tg(11)
00039 ! ______________________________________________________________________
00040   let fntop("Core\Programs\PrtFlex2",cap$="Print Flex")
00099   let programfolder$=fncursys$&"mstr" !:
        let datafolder$=env$('Q')&'\'&fncursys$&"mstr" 
00102 ! 
00122   dim saddr$*40,scity$*20,sstate$*2,szip$*11,msgnum$*12,maddr$*39,mcity$*20,mstate$*2,mzip$*11,atime$*8,crn$*9,dtl$*8,name$(3)*25,ss$*11,race$*18,sex$*1
00123   dim tr(7),tr$*12,td$*30
00124   dim ck1$*1,ck2$*1,ck3$*1,ck4$*1,ck5$*1,ck5d$*60,amt(7),amt2(5),cksa$*1,eshome$*1,esstreet$*30,weather$*1,sign$*1,signhelp$*30,witname$*30
00125   dim ifnot$*80,comment$*80,worker$*20,amt3(12),hes$(10)*10
00126   dim chk4b1$*1,status$*10,payee$(3)*5,payamt(3),chk4c1$*1,chk4c2$*1
00127   dim vod$*80,chk4d1$*1,chk4d2$*1,chk4d3$*1,chk4d4$*1,existingss$*12
00128   dim chk4d5$*1,chk4d6$*1,chk4d7$*1,chk4d8$*1,otherspec$*40,chk4d9$*1
00129   dim heap$*1,chk4d21$*1,comment2$*150,votime$*8,holdname$(3)*30
00209 ! ______________________________________________________________________
00210   let columns = 7
00300   gosub OPENFILES
00400   open #11: "Name="&env$('temp')&"\Gridname.tmp",internal,input,relative  !:
        read #11,using 'Form POS 1,C 40',rec=1: gridname$ !:
        close #11: 
03999 ! __________________ this is 3999 next is 4000 _________________________
04000 PRINTGRID: ! Prints the grid
04001   mat item$(columns)
04002   mat item$=("")
04003   mat colhdr$(columns)
04004   mat colhdr$=("")
04005   mat colmask$(columns)
04006   mat colmask$=("")
04010   let fntos(sn$="mstrflex")
04018   let fnlbl(1,1,uprc$(gridname$),20,2,3)
04075   gosub GRIDHEADING ! reads the headings that were created above
04090   let fnflexinit1("flexprint",3,1,10,70,mat colhdr$,mat colmask$,1)
04095 ! Restore #1:
04096 L4096: gosub READDATAFILES ! reads the database for the grid information                                     These read statements and form statements must
04097 !                     be in a dispaly file in the data base folder with                               a "_read" on the end of the file name.  The file                                name must be the same as the database name + _read
04112   gosub GRIDDETAILS ! Assign the variable names to                                each column
04120   let fnflexadd1(mat item$)
04130   goto L4096
04140 EOFONREAD: ! Complete the grid once all data has been read
04144 ! Let FNLBL(15,1,"Export the grid to a fixed width file, for later use.")
04145   let fncmdset(52): let fnacs(sn$,win,mat response$,ckey) !:
        ! CALL items selected
04146   let lastgridresponse$=response$(1)
04160   if ckey=5 then chain "Core\prtflex\PRTFLEX1",programfolder$,datafolder$
04170 ! Let FNXIT(CURSYS$)
04180 ! ____________________________________________________________________
07999 ! __________________ this is 7999 next is 8000 _________________________
08000 OPENFILES: ! The following lines will be proc in from a display file                          you have created. They are in the same file as the read                         statements explained above.  Don't forget the del lines to
08001 !             remove the old reads in case they dont match
08010   open #1: "Name="&datafolder$&"\gltrans.h"&env$('cno')&",Shr", internal, input,relative 
08100   return 
08999 ! __________________ this is 8999 next is 9000 _________________________
09000 READDATAFILES: !  These read statements will be contained in a display                            file that matches the data base name plus _info
09010 L9010: form pos 1,n 3,n 6,n 3,n 6,pd 6.2,2*n 2,c 12,c 30
09020   read #1,using L9010: mat tr,tr$,td$ eof EOFONREAD
09100   return 
09999 ! __________________ this is 9999 next is 10000 ________________________
10000 GRIDHEADING: ! The followng lines will be generated each time a grid is                        printed.  Don't ever renumber this program unless you are                       prepared to spend some time figuring out where lines are!
10010   let colhdr$(1)="Dept" !:
        let colmask$(1)="30"
10020   let colhdr$(2)="Major" !:
        let colmask$(2)="30"
10030   let colhdr$(3)="Date" !:
        let colmask$(3)="1"
10040   let colhdr$(4)="Desc" !:
        let colmask$(4)="80"
10050   let colhdr$(5)="Amt" !:
        let colmask$(5)="10"
10060   let colhdr$(6)="RefNum" !:
        let colmask$(6)="80"
10070   let colhdr$(7)="TransCode" !:
        let colmask$(7)="30"
10499   return 
10500 GRIDDETAILS: ! The following lines are generated lines.  They will be                          removed and added back just before each grid is printed
10510   let item$(1)= str$(tr(1))
10520   let item$(2)= str$(tr(2))
10530   let item$(3)= str$(tr(4))
10540   let item$(4)=td$
10550   let item$(5)= str$(tr(5))
10560   let item$(6)=tr$
10570   let item$(7)= str$(tr(6))
10990   return 
10999 ! __________________ this is 10999 next is 11000 ________________________
11000 DONE: close #1: ioerr XIT
11010 XIT: let fnxit(cursys$)
11020 ! _______________________________________________________________________
11021 ! <Updateable Region: ERTN>
11022 ERTN: let fnerror(program$,err,line,act$,"xit")
11023   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
11024   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
11025   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
11026 ERTN_EXEC_ACT: execute act$ : goto ERTN
11027 ! /region
11028 ! _______________________________________________________________________
