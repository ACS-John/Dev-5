00010 ! Prtflex2 ! DO NOT RENUMBER
00011 ! Replace R:\Core\PrtFlex\PrtFlex2
00013 ! ______________________________________________________________________
00019 ! ______________________________________________________________________
00020   library 'R:\Core\Library': fnacs,fnlbl,fntxt,fntos,fnerror,fncomboa,fnflexadd1,fnflexinit1,fnbutton,fncombof,fnmsgbox,fncno,fncursys$,fnchain,fnxit,fncmdset,fntop,fnfra,fnopt,fncmbact,fncmdkey
00021   on error goto ERTN
00022 ! r: dims
00023   dim programfolder$*60,datafolder$*256,gridname$*40
00024   dim name$*30,colmask$*3,tt$*200,colhdr$(80)*30,colmask$(80)*3
00025   dim response$(87)*80,text$*40, cap$*128,lastgridresponse$*87,resp$(10)*60
00026   dim options$(300)*87,ln$*132,item$(80)*80,abbrev$*20,open_read$*80,tg(11)
00027   dim z$*8,rp$*3,py$(8)*25,ss$*11,pb$(10)*2,pl$(5)*13,pf$(7)*6,pa$(8)*1
00028   dim pm(40),adr(2),udf$*256
00029   dim dg$(6)*6,pc$(5)*5,pcd(5),oc$(5)*2,ocd(5),va$(5)*2,vaa(5)
00031   dim df$*1,dr$*9,dc$*2,da$*17,extra(23),extra$(11)*30,item$(80)*50
00032   dim abbrev$*20,open_read$*80,tg(11)
00039 ! /r
00040   let fntop("R:\Core\Programs\PrtFlex2",cap$="Print Flex")
00050   let programfolder$=fncursys$&"mstr"
00060   let datafolder$='Q:\'&fncursys$&"mstr"
00070   let fncno(cno)
00080   let dataext$=".h"&str$(cno)
00101   let udf$=env$('temp')&'\'
00122   dim saddr$*40,scity$*20,sstate$*2,szip$*11,msgnum$*12,maddr$*39,mcity$*20,mstate$*2,mzip$*11,atime$*8,crn$*9,dtl$*8,name$(3)*25,ss$*11,race$*18,sex$*1
00123   dim tg(11),p$*10
00124   dim ck1$*1,ck2$*1,ck3$*1,ck4$*1,ck5$*1,ck5d$*60,amt(7),amt2(5),cksa$*1,eshome$*1,esstreet$*30,weather$*1,sign$*1,signhelp$*30,witname$*30
00125   dim ifnot$*80,comment$*80,worker$*20,amt3(12),hes$(10)*10
00126   dim chk4b1$*1,status$*10,payee$(3)*5,payamt(3),chk4c1$*1,chk4c2$*1
00127   dim vod$*80,chk4d1$*1,chk4d2$*1,chk4d3$*1,chk4d4$*1,existingss$*12
00128   dim chk4d5$*1,chk4d6$*1,chk4d7$*1,chk4d8$*1,otherspec$*40,chk4d9$*1
00129   dim heap$*1,chk4d21$*1,comment2$*150,votime$*8,holdname$(3)*30
00209 ! ______________________________________________________________________
00210   let columns=1
00300   gosub OPENFILES
00400   open #11: "Name="&udf$&"Gridname.tmp",internal,input,relative 
00402   read #11,using 'Form POS 1,C 40',rec=1: gridname$
00404   close #11: 
00500   if fncursys$='UB' and rln(1)=102 then gosub ASKTRANSET
03999 ! __________________ this is 3999 next is 4000 _________________________
04000 PRINTGRID: ! r: Prints the grid
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
04096 READ_NEXT: gosub READDATAFILES ! reads the database for the grid information                                     These read statements and form statements must
04097 !                     be in a dispaly file in the data base folder with                               a "_read" on the end of the file name.  The file                                name must be the same as the database name + _read
04098   if trim$(c$)<>"[All]" and trim$(c$)<>trim$(p$) then goto READ_NEXT
04099   if beg_date<>0 and beg_date>tdate then goto READ_NEXT
04100   if end_date<>0 and end_date<tdate then goto READ_NEXT
04101   if sel_code=2 and tcode<>1 then goto READ_NEXT
04112   gosub GRIDDETAILS ! Assign the variable names to                                each column
04120   let fnflexadd1(mat item$)
04130   goto READ_NEXT
04140 EOFONREAD: ! Complete the grid once all data has been read
04144 ! Let FNLBL(15,1,"Export the grid to a fixed width file, for later use.")
04145   let fncmdset(52): let fnacs(sn$,win,mat response$,ckey) ! CALL items selected
04146   let lastgridresponse$=response$(1)
04160   if ckey=5 then chain "R:\Core\prtflex\PRTFLEX1",programfolder$,datafolder$
04170 ! Let FNXIT(CURSYS$)
04180 ! /r
07999 ! __________________ this is 7999 next is 8000 _________________________
08000 OPENFILES: ! r: The following lines will be proc in from a display file                          you have created. They are in the same file as the read                         statements explained above.  Don't forget the del lines to
08001 !             remove the old reads in case they dont match
08010   open #1: "name="&datafolder$&"\ubtransvb"&dataext$&",kfname="&datafolder$&"\ubtrindx"&dataext$&",Use,RecL=102,KPs=1,KLn=19",internal,outin,keyed 
08100   return  ! /r
08999 ! __________________ this is 8999 next is 9000 _________________________
09000 READDATAFILES: !  r: These read statements will be contained in a display                            file that matches the data base name plus _info
09010 L9010: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
09020   read #1,using L9010: p$,tdate,tcode,tamount,mat tg,we,wu,er,eu,gr,gu,tbal,pcode eof EOFONREAD
09100   return  ! /r
09999 ! __________________ this is 9999 next is 10000 ________________________
10000 GRIDHEADING: ! r: The following lines will be generated each time a grid is                        printed.  Don't ever renumber this program unless you are                       prepared to spend some time figuring out where lines are!
10010   let colhdr$(1)="Name" : let colmask$(1)="80"
10499   return  ! /r
10500 GRIDDETAILS: ! r: The following lines are generated lines.  They will be                          removed and added back just before each grid is printed
10510   let item$(1)=e$(2)
10990   return  ! /r
10999 ! __________________ this is 10999 next is 11000 ________________________
11000 DONE: close #1: ioerr ignore
11010 XIT: let fnxit
11020 IGNORE: continue 
11021 ! <Updateable Region: ERTN>
11022 ERTN: let fnerror(cap$,err,line,act$,"xit")
11023   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
11024   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
11025   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
11026 ERTN_EXEC_ACT: execute act$ : goto ERTN
11027 ! /region
11028 ! _______________________________________________________________________
12000 ASKTRANSET: ! r:
12010   let transtype$(1)="Charge"
12011   let transtype$(2)="Penalty"
12012   let transtype$(3)="Collection"
12013   let transtype$(4)="Credit Memo"
12014   let transtype$(5)="Debit Memo"
12030   let fntos(sn$="Gridtrans-1")
12032   let rc=cf=0
12040   let fnfra(1,1,6,23,"Transaction Type","You can review all transactions or any specific type of transaction",0)
12042   let cf+=1 : let fratype=cf
12050   let fnopt(1,3,"[All]",0,fratype)
12051   if sel_code=1 or sel_code=0 then 
12052     let resp$(rc+=1)="True"
12053   else 
12054     let resp$(rc+=1)="False"
12055   end if 
12060   let fnopt(2,3,"Charges",0,fratype)
12061   if sel_code=2 then 
12062     let resp$(rc+=1)="True"
12063   else 
12064     let resp$(rc+=1)="False"
12065   end if 
12070   let fnopt(3,3,"Penalties",0,fratype)
12071   if sel_code=3 then 
12072     let resp$(rc+=1)="True"
12073   else 
12074     let resp$(rc+=1)="False"
12075   end if 
12080   let fnopt(4,3,"Collections",0,fratype)
12081   if sel_code=4 then 
12082     let resp$(rc+=1)="True"
12083   else 
12084     let resp$(rc+=1)="False"
12085   end if 
12090   let fnopt(5,3,"Credit Memos",0,fratype)
12091   if sel_code=5 then 
12092     let resp$(rc+=1)="True"
12093   else 
12094     let resp$(rc+=1)="False"
12095   end if 
12100   let fnopt(6,3,"Debit Memos",0,fratype)
12101   if sel_code=6 then 
12102     let resp$(rc+=1)="True"
12103   else 
12104     let resp$(rc+=1)="False"
12105   end if 
12110   let fnfra(1,30,3,42,"Date Range","You can transactions for any date range or leave these blank to see all transactions.")
12112   let cf+=1 : let fradate=cf : let mylen=26 : let mypos=mylen+2
12120   let fnlbl(1,1,"Starting Date:",mylen,1,0,fradate)
12130   let fntxt(1,mypos,10,0,1,"3",0,empty$,fradate)
12132   let resp$(rc+=1)=str$(beg_date)
12140   let fnlbl(2,1,"Ending Date:",mylen,1,0,fradate)
12150   let fntxt(2,mypos,10,0,1,"3",0,empty$,fradate)
12152   let resp$(rc+=1)=str$(end_date)
12160   let fnfra(6,30,2,60,"Account","You review transactions for all accounts or for an individual.")
12162   let cf+=1 : let fraaccount=cf
12170   let fnlbl(1,1,"Account:",8,1,0,fraaccount)
12180   let fncmbact(1,10,1,fraaccount)
12182   let rc+=1
12183   if trim$(hact$)<>"" then 
12184     let resp$(rc)=hact$
12185   else if resp$(rc)="" then 
12186     let resp$(rc)="[All]"
12187   end if 
12190   let fncmdkey("&Display",1,1,0,"Displays a list of transactions on the screen")
12210   let fncmdkey("&Cancel",5,0,1,"Returns to customer record")
12220   let fnacs(sn$,0,mat resp$,ckey)
12222   if ckey=cancel then goto XIT_ASKTRANSET
12224   if resp$(1)="True" then 
12225     let sel_code=1
12226   else if resp$(2)="True" then 
12227     let sel_code=2
12228   else if resp$(3)="True" then 
12229     let sel_code=3
12230   else if resp$(4)="True" then 
12231     let sel_code=4
12232   else if resp$(5)="True" then 
12233     let sel_code=5
12234   else if resp$(6)="True" then 
12235     let sel_code=6
12236   end if 
12240   let beg_date=val(resp$(7))
12242   let end_date=val(resp$(8))
12244   let c$=resp$(9)(1:10)
12250 XIT_ASKTRANSET: return  ! /r
